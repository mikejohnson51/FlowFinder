library(FlowFinder)
shinyServer(function(input, output, session) {
  
  # Include code for download handler
  source("server_download_handler.R", local = TRUE)$value
  
  ########## Initial Setup ####################################################################
  
  # Set up reactive values
  values <- reactiveValues()
  
  # Define base map
  basemap <- reactive({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addScaleBar("bottomleft") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addLayersControl(
        baseGroups = c("Basemap","Imagery"),
        overlayGroups = c("USGS Stations", "NHD Flowlines", "Water bodies", "AOI"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomleft"
      ) %>% 
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ 
                      var num = Math.random();
                      Shiny.onInputChange('currentLoc', {
                        num: num,
                      }); 
                   }"))) 
    # %>% 
    #   addEasyButton(easyButton(
    #     icon="fa-filter", title="Filter",
    #     onClick=JS("function(btn, map){ $('#filterModal').modal('toggle') }")))
    
  })
  
  observe({
    req(input$currentLoc)
    if(!is.null(input$lat)){
      updateTextInput(session = session, inputId =  "place", value = paste(input$lat, input$long, sep = " "), placeholder = "Current Location")
      shinyjs::click("do")
    } else {
      showNotification("Your current location can't be determined. Make sure you have given your browser the necessarry permissions.", type = "error")
    }
  })
  
  
  
  # Set map
  output$map <- renderLeaflet({ basemap() })

  # Define reactive values corresponding to current COMID
  update_cur_id <- function(input) {
    
    values$id = getIDs(input)[1]
    
    values$data = values$nwm %>% 
      filter(COMID == values$id)
    
    values$normals = norm %>% 
      filter(COMID == values$id)
    
  }
  
  # On go, calculate reactive values
  observeEvent(input$do, {

    # Don't let user enter new location while processing previous
    shinyjs::disable("do")
    
    # Initialize progress bar
    withProgress(message = 'Analyzing Location', value = 0, {
      
      # Get initial location
      values$loc = get_location(place = input$place)
      incProgress(1/8, detail = "Getting Spatial Objects")
      
      # Get spatial data
      values$flow_data = suppressMessages(
                          AOI::getAOI(clip = list(values$loc$lat, values$loc$lon, size, size)) %>% 
                            HydroData::findNHD(ids = TRUE) %>% 
                            HydroData::findWaterbodies() %>% 
                            HydroData::findNWIS()
                         )
      
      # Determine what state (or if) AOI is in
  
      state = latlong2state(lat = values$loc$lat, lon = values$loc$lon)
      
      
      
      # Set global variable and show notification if no streams in AOI
      # Continue with data prep if there are streams
      if (!exists('nhd', where=values$flow_data) || (is.null(state))) {
        values$any_flow = FALSE
        if (is.null(state)) {
          showNotification("Warning: AOI appears to be outside CONUS", type = "warning", duration = 10)
        }
      }
      else {
        values$any_flow = TRUE
        
        incProgress(4/8, detail = "Subsetting Stream Data")

        # Subset data
        values$nwm = subset_nomads(comids = values$flow_data$comid) %>% 
          mutate()
        
        # Merge nwm and nhd data sets
        
        vals = values$nwm %>% 
          tidyr::spread(dateTime, Q_cfs) %>% 
          select(-agency_code) %>% 
          rename("comid" = COMID)
        
        values$flow_data$nhd <- values$flow_data$nhd %>% 
          inner_join(vals, by = "comid")
        
        rm(vals)
        
        nwm_summary <- values$nwm %>% 
          group_by(COMID) %>% 
          summarise(max = max(Q_cfs), 
                    mean = mean(Q_cfs), 
                    min = min(Q_cfs)) %>%
          mutate(range = max - min) %>% 
          rename("comid" = COMID)
        
        averages = norm %>%
          `colnames<-`(c("comid", "month_avg")) %>% 
          mutate(month_avg = month_avg * 35.3147)
        
        values$flow_data$nhd <- values$flow_data$nhd %>% 
          inner_join(nwm_summary, by = "comid") %>% 
          inner_join(averages, by = "comid") %>% 
          mutate(mean_dif = month_avg - mean)
        
        find_diff <- function(id) {
          diff(values$nwm %>% filter(COMID == id) %>% .$Q_cfs)
        }
        
        diff_matrix <- matrix(ncol=length(unique(values$nwm$dateTime)), nrow=length(nwm_summary$comid))
        for (j in 1:length(nwm_summary$comid)) {
          id = nwm_summary$comid[j]
          diff_matrix[j,] <- c(id, find_diff(id))
        }
        diff_df <- data.frame(diff_matrix)
        colnames(diff_df) <- c("comid", paste0('der_',unique(values$nwm$dateTime)[1:39]))
      
        values$flow_data$nhd <- values$flow_data$nhd %>% 
          inner_join(diff_df, by = "comid")
        
        values$colors = c('#bdd3fb',
                          '#01c6fa',
                          '#0085a5',
                          '#0074f6',
                          '#004e7c',
                          '#ad18dd',
                          '#ffa50a',
                          '#ff0017',
                          '#b0062c')
        
        values$div_colors = c('#d73027',
                              '#f46d43',
                              '#fdae61',
                              '#bababa',
                              '#abd9e9',
                              '#74add1',
                              '#4575b4')
        
        create_div_palette <- function(mag) {
          num_colors = length( values$div_colors)
          bins <- as.double(seq(-3.5,3.5,1) %>% 
                              purrr::map(function(x) ifelse(x > 0, ceiling(x * mag/3.5), floor(x * mag/3.5))))
          pal <- colorBin(values$div_colors, bins = bins)
          return(list(pal = pal, bins = bins))
        }
  
        create_cont_palette <- function(vals) {
          num_colors = length(values$colors)
          diff <- max(vals, na.rm = T) - min(vals, na.rm = T)
          bins <- as.double(0:num_colors %>% purrr::map(function(x) ceiling(x * diff/num_colors)))
          pal <- colorBin(values$colors, bins = bins)
          return(list(pal = pal, bins = bins))
        }
        
        values$palette <- create_cont_palette(nwm_summary$max)
        values$range_palette <- create_cont_palette(nwm_summary$range)
        values$mean_palette <- create_cont_palette(nwm_summary$mean)
        values$month_palette <- create_cont_palette(values$flow_data$nhd$month_avg)
        
        mag = max(abs(c(max(diff_matrix[,2:40], na.rm = T), min(diff_matrix[,2:40], na.rm = T))))
        values$deriv_palette <- create_div_palette(mag)
        
        mag = max(abs(c(max(values$flow_data$nhd$mean_dif, na.rm = T), min(values$flow_data$nhd$mean_dif, na.rm = T))))

        values$mean_dif_palette <- create_div_palette(mag)
        
        
        incProgress(2/8, detail = "Finding Upstream/Downstream")
        
        # Set upstream/downstream data
        values$flow_data$nhd_prep = prep_nhd(flines = values$flow_data$nhd)
        values$hmm = get_upstream(flines = values$flow_data$nhd_prep)
      }
      incProgress(1/8, detail = "Mapping")

      # Map data
      clearMarkers()
      add_layers(map = leafletProxy("map"), values = values)
    })
    
    # Update title
    if (input$do == 1) {
      updateTextInput(session = session, inputId =  "place", value = "")
    }
    
    # Page title
    output$data_loc <- renderText({ input$place })
    
    #Table 1: Station info
    output$stations = renderTable({station_table(values)}, striped = TRUE, sanitize.text.function = function(x) x)
  
    # Table 2: Flowline info
    output$Flowlines = renderTable(flowlines_table(values = values), striped = TRUE)
    
    # Table 3: NWM info
    output$meta = renderTable(nwm_table(values), striped = TRUE)
    
    ui_flow_only = list("prevCOMID", "nextCOMID", "flow_selector", 
                     "mark_flowline", "data_csv", "data_rda", 
                     "data_nhd", "plot_png", "plot_dygraph")
  
    # Only set choices if there are flowlines in AOI
    if(values$any_flow) {
      # Get and set initial COMID choices 
      choices = set_choices(values = values)
      values$choices = choices$choices
      updatePickerInput(session, 'flow_selector', choices = values$choices, selected = choices$default,
                        choicesOpt = list(
                                      style = ifelse(choices$non_zeros,
                                                     yes = "color:#0069b5;font-weight:bold;",
                                                     no = "style=color:#a8a8a8")
                                      )
                        )
      
      # Set initial values based on default COMID
      update_cur_id(choices$default)
      
      show_hide_all(elements = ui_flow_only, action = "enable")
      
    } 
    # Disable stream-specific UI elements
    else {
      showNotification("No flowlines found in this AOI", type = "error", duration = 10)
      values$choices = NULL
      updatePickerInput(session, 'flow_selector', choices = "", selected = NULL)
      show_hide_all(elements = ui_flow_only, action = "disable")
    }

    # Re-enable search
    shinyjs::enable("do")
  })
  
  # Move to current location when possible
  observe({
    if(!is.null(input$lat)){
      updateTextInput(session = session, inputId =  "place", value = paste(input$lat, input$long, sep = " "), placeholder = "Current Location")
      shinyjs::click("do")
    } else if (!is.null(input$getIP)) {
      updateTextInput(session = session, inputId = "place", value = paste(input$getIP$latitude, input$getIP$longitude, sep = " "), placeholder = "Search FlowFinder")
      shinyjs::click("do")
    }
  })
  
  ########## MAP TAB ####################################################################
  
  # Clear markers on map
  clearMarkers <- function() {
      leafletProxy("map", session) %>%
        clearGroup(list("view-on-map", "up-stream", "down-stream"))
  }
  
  # Reset button
  observeEvent(input$reset, { clearMarkers() })
  
  # Mark upstream flows from leaflet popup
  observe({
    if (is.null(input$upStream))
      return()
    clearMarkers()
    
    from_nodes <- values$hmm %>% 
      filter(comid == input$upStream$comid) %>% 
      .$fromCOMID
    
    mark_up_down(map = leafletProxy("map", session), 
                 values = values, 
                 stream = input$upStream$comid, 
                 data = values$flow_data$nhd[values$flow_data$nhd$comid %in% from_nodes,], 
                 group = "up-stream", 
                 color = "#84bd00")
  })
  
  # Mark downstream flows from leaflet popup
  observe({
    if (is.null(input$downStream))
      return()
    clearMarkers()
    
    to_nodes <- values$flow_data$nhd_prep %>% 
      filter(comid == input$downStream$comid) %>% 
      .$toCOMID
    
    mark_up_down(map = leafletProxy("map", session), 
                 values = values, 
                 stream = input$downStream$comid, 
                 data = values$flow_data$nhd[values$flow_data$nhd$comid %in% to_nodes,], 
                 group = "down-stream", 
                 color = "red")
  })
  
  # Select flowline from popup
  observe({
    if (is.null(input$goto))
      return()
    updatePickerInput(session = session, inputId = "flow_selector", selected = input$goto$text)
  })
  
  ########## DATA TAB ####################################################################
  
  # Update current ID info when selector is changed
  observeEvent(input$flow_selector, {
    req(input$flow_selector)
    update_cur_id(input$flow_selector)
  })
  
  # Generate dygraph plot
  output$dygraph <- dygraphs::renderDygraph({
    req(values$any_flow, input$flow_selector)
    withProgress(message = 'Analyzing Location', value = .5, {
      dygraph_plot(values = values, selected = input$flow_selector )
    })
  })
  
  # Keep as easy way to vizualize downloadable ggplot
  # output$plot2<-renderPlot({
  #   static_plot(values = values, selected = input$flow_selector )
  # })
  
  # Previous stream button
  observeEvent(input$prevCOMID, {
    current <- which(values$choices == input$flow_selector)
    if(current > 1){
      updatePickerInput(session, "flow_selector",
                           selected = values$choices[current - 1])
    }
  })
  
  # Next stream button
  observeEvent(input$nextCOMID, {
    current <- which(values$choices == input$flow_selector)
    if(current < length(values$choices)){
      updatePickerInput(session, "flow_selector",
                           selected = values$choices[current + 1])
    }
  })
  
  # View on map button
  observeEvent(input$mark_flowline, {
    clearMarkers()
    reaches = values$flow_data$nhd[values$flow_data$comid %in% getIDs(input$flow_selector), ]
    bb = AOI::getBoundingBox(reaches)
    leafletProxy("map", session) %>%
      fitBounds(min(bb@bbox[1,]), min(bb@bbox[2,]), max(bb@bbox[1,]), max(bb@bbox[2,])) %>% 
      addPolylines(data = reaches, 
                   color = "red", 
                   weight = 15,
                   opacity = 0.8,
                   options = pathOptions(clickable = FALSE),
                   group = "view-on-map"
      )
  })
  
  # Find up/downstream streams for current COMID
  observe({
    req(values$flow_data$nhd)
    values$up_down_stream = find_connecting_streams(values = values)
  })
  
  # Activate/Deactivate buttons depending on number of COMIDs selected
  observe({
    elements = list("tbl_up", "tbl_down", "prevCOMID", "nextCOMID")
    if (length(input$flow_selector) > 1) {
      show_hide_all(elements = elements, action = "hide")
    } else {
      show_hide_all(elements = elements, action = "show")
    }
  })
  
  # Upstream Table
  output$tbl_up <- DT::renderDataTable({
    req(values$any_flow)
    stream_table(data = values$up_down_stream$upstream, direction = "upstream", values = values, session = session)
  })
  
  # Downstream Table
  output$tbl_down <- DT::renderDataTable({
    req(values$any_flow)
    stream_table(data = values$up_down_stream$downstream, direction = "downstream", values = values, session = session)
  })
  
  # After 'eye' icon is clicked in up/downstream tables
  observe({
    if (is.null(input$switchStream))
      return()
    else {
      streams = unlist(strsplit(input$switchStream$stream ,","))
      isolate({
        if (length(streams) > 1) {
          streams = c(streams, input$flow_selector)
        }
        updatePickerInput(session = session, inputId = "flow_selector", selected = streams)
      })
    }
  })
  
  # Change download button based on whether or not any items are checked
  # Code for download handler in server_download_handler.R
  observe({
    download_options = c(input$data_csv, input$data_nhd, input$data_rda, input$plot_png, input$plot_dygraph, input$maps_floods, input$maps_flow)
    if (any(download_options)) {
      shinyjs::enable("downloadData")
      runjs("
            var text = document.getElementById('downloadData').firstChild;
            text.data = 'Download!'
            ")
    } else {
      shinyjs::disable("downloadData")
      runjs("
            var text = document.getElementById('downloadData').firstChild;
            text.data = 'Check one or more boxes'
            ")
    }
  })
  
  ########## MAP TAB ####################################################################
  
  # Set high flows map
  output$flood_map <- renderLeaflet({ flood_map })
  
  # Generate dygraph plot
  output$flood_dygraph <- dygraphs::renderDygraph({
    req(input$map_flood$comid)
    reset = values$reset_fl_graph
    comid = input$map_flood$comid
    isolate({
      if (comid == "reset") {
        values$flood_data <- NULL
        dygraph(data = data.frame(x = 1)) %>%
          dyOptions(drawGrid = FALSE,
                    drawYAxis = FALSE,
                    drawXAxis = FALSE)
      } else {
        #values$flood_data_ids <- ifelse(is.null(values$flood_data_ids), comid, c(values$flood_data_ids, comid))
        if (is.null(values$flood_data_ids)) {
          values$flood_data_ids <- comid
        } else {
          values$flood_data_ids <- c(values$flood_data_ids, comid)
        }
        
        withProgress(message = 'Making plot', value = .5, {
          new_data = dygraph_flood(comid = comid, data = values$flood_data, number = length(values$flood_data_ids))
          values$flood_data = new_data$data_set
          new_data$graph
        })
      }
    })
  })
  
  observe({
    req(input$map_flood$comid)
    comid = input$map_flood$comid
    
    isolate({
      if (comid == "reset") {
        values$flood_data_ids <- NULL
        leafletProxy("flood_map", session) %>%
          clearGroup("flood_markers")
      } else {
        colors = c("orange", "green", "red", "purple",
                   "lightblue", "lightgreen", "pink", "lightred", "gray",
                   "darkblue", "darkred", "darkgreen", "darkpurple",
                   "orange", "green", "red", "purple",
                   "lightblue", "lightgreen", "pink", "lightred", "gray",
                   "darkblue", "darkred", "darkgreen", "darkpurple")
        
        color = colors[length(values$flood_data_ids) + 1]
      
        leafletProxy("flood_map", session) %>%
          addAwesomeMarkers(
            icon = awesomeIcons(
              icon = 'circle',
              iconColor = '#FFFFFF',
              library = 'fa',
              markerColor = color
            ),
            data = data,
            lat = as.numeric(input$map_flood$lat),
            lng = as.numeric(input$map_flood$lon),
            popup = paste0("<strong>NHD COMID: </strong>", comid),
            group = "flood_markers"
          )
      }
    })
  })
  
  ########## FILTER TAB ####################################################################
  
  output$displayOptions <- renderUI({
    req(values$nwm)
    if (input$display_type == "Static") {
      awesomeRadio(inputId = "flow_display", 
                   label = NULL, 
                   choices = c("Default" = "default",
                               "Positive" = "positive",
                               "Month Average" = "month_avg",
                               "Average" = "mean",
                               "Average Comparison" = "mean_dif",
                               "Peak Flow" = "max_value",
                               "Range" = "range"),
                   checkbox = TRUE)
    }
    
    else {
      values$nwm <- values$nwm %>% 
        mutate(data_char = as.character.POSIXt(values$nwm$dateTime, tz = "GMT"))
      values$times = unique(values$nwm$data_char)
      clearGroup(map = leafletProxy("map2"), group = "NHD Flowlines")
      list(
        awesomeRadio(inputId = "flow_display_dy", 
                     label = NULL, 
                     choices = c("Time Series" = "time_series",
                                 "Change" = "deriv",
                                 "Positive" = "positive"),
                     checkbox = TRUE),
        sliderInput("selected_time", NULL,
                    min = as.POSIXct(values$times[1],tz = "GMT"), 
                    max = as.POSIXct(values$times[length(values$times)], tz = "GMT"),
                    ticks = FALSE,
                    value = as.POSIXct(values$times[1], tz = "GMT"), step = 10800,
                    timezone = "+0000",
                    timeFormat = "%F %T",
                    animate =
                      animationOptions(interval = 750, loop = FALSE))
      )
    }
    
  })
  
  observe({
    req(input$selected_time)
    if (input$flow_display == "time_series") {
      updateSliderInput(session = session, inputId = "selected_time", max = as.POSIXct(values$times[length(values$times)], tz = "GMT") )
    }
    else if (input$flow_display == "deriv") {
      updateSliderInput(session = session, inputId = "selected_time", max = as.POSIXct(values$times[length(values$times)-1], tz = "GMT") )
    }
  })
  
  basemap_filter <- reactive({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addLayersControl(
        baseGroups = c("Basemap","Imagery"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomleft"
      ) %>% 
      addEasyButton(easyButton(
        id = "legend_switch",
        icon="fa-key", title="",
        onClick=JS("function(btn, map){ 
                      $('.legend').toggle();; 
                    }")))
  })
  
  output$map2 <- renderLeaflet({ 
    req(values$flow_data$nhd)
    basemap_filter() %>%  
      add_bounds(AOI = values$flow_data$AOI) %>%
      add_flows(data = values$flow_data$nhd, color = "blue")
  })
  
  observe({
    req(values$flow_data$nhd, values$filtered_data)
    positive_pal <- colorBin(c('red', 'blue'), bins = c(-1, 0.000000000000000001, 9000000000000000000000))
    values$static_dic = list(
      default = list(c("blue"), list(c('blue'), c('Flowline'), NULL)),
      positive = list(~positive_pal(values$filtered_data$max),list(c('red', 'blue'), c('0', '>0'), "Q_cfs")),
      month_avg = list(~values$month_palette$pal(values$filtered_data$month_avg), values$month_palette),
      mean = list(~values$mean_palette$pal(values$filtered_data$mean), values$mean_palette),
      mean_dif = list(~values$mean_dif_palette$pal(values$filtered_data$mean_dif), values$mean_dif_palette),
      max_value = list(~values$palette$pal(values$filtered_data$max), values$palette),
      range = list(~values$range_palette$pal(values$filtered_data$range), values$range_palette)
    )
  })
  
  observe({
    req(input$display_type == "Static")
    req(values$static_dic, input$flow_display)
    type = input$flow_display
    isolate({
       data = eval(parse(text = paste0('values$static_dic$', "`", type,"`")))
       clearGroup(map = leafletProxy("map2"), group = "NHD Flowlines")
       add_flows(map = leafletProxy("map2"), data = values$filtered_data, color = data[[1]])
       if (!type %in% c("default", "positive")) {
         add_legend(pal = data[[2]])
       } 
       else {
         add_legend(colors = data[[2]][[1]], labels = data[[2]][[2]], title = data[[2]][[3]])
       }
    })
  })
  
  observe({
    req(values$flow_data$nhd, values$filtered_data)
    positive_pal <- colorBin(c('red', 'blue'), bins = c(-1, 0.000000000000000001, 9000000000000000000000))
    values$dynamic_dic = list(
      # name = list(color, pal, legend)
      deriv = list('values$filtered_data@data$`der_', values$deriv_palette$pal, values$deriv_palette),
      time_series = list('values$filtered_data@data$`', values$palette$pal, values$palette),
      positive = list('values$filtered_data@data$`', positive_pal, list(c('red', 'blue'), c('0', '>0'), "Q_cfs"))
    )
  })

  observe({
    req(input$display_type == "Dynamic")
    req(input$selected_time, values$filtered_data, input$flow_display_dy, values$dynamic_dic)
    type = input$flow_display_dy
    isolate({
      time = as.character.POSIXt(input$selected_time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      data = eval(parse(text = paste0('values$dynamic_dic$', "`", type,"`")))
      color_determination = eval(parse(text = paste0(data[[1]], time,"`")))
      add_flows(map = leafletProxy("map2"), data =  values$filtered_data, ~data[[2]](color_determination))

      if (input$flow_display_dy == "positive") {
        add_legend(colors = data[[3]][[1]], labels = data[[3]][[2]], title = data[[3]][[3]])
      }
      else {
        add_legend(pal = data[[3]])
      }
    })
  })
  
  # Options Available For Table
  output$nhd_options <- renderUI({
    req(values$flow_data$nhd)
    generate_options(href = "collapseNhd", inputID = "nhdSelector", 
                     choices = c("comid",
                                 "gnis_name",
                                 "reachcode",
                                 "lengthkm",
                                 "streamorde",
                                 "slope"), 
                     selected = c("gnis_name", "comid", "lengthkm", "streamorde", "reachcode", "slope"))
  })
  
  # Put time on map for dynamic sets
  output$map_time <- renderUI({
    # req(input$selected_time)
    if (input$display_type == "Static") {
      absolutePanel()
    }
    else {
      absolutePanel(id = "time_stamp", top = 0, left = 0, right = 0,
                    tags$div(id = "ts", tags$h3(HTML(paste(as.character.POSIXt(input$selected_time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), "UTC"))))
      )
    }
  })
  
  # Filterable table
  output$nhd_table <- DT::renderDataTable({
    req(values$flow_data$nhd, input$nhdSelector)
    data <- values$flow_data$nhd@data %>% 
      select(one_of(input$nhdSelector))
    base_table(data)
  })
  
  # Generate data set based on filtered table
  observe({
    req(values$flow_data$nhd, input$nhd_table_rows_all)
    values$filtered_data <- values$flow_data$nhd %>% 
      slice(input$nhd_table_rows_all)
  })
  
  # Display info module 
  output$display_info<- renderUI({ 
    
    if (input$chart_type == "Static") {
    
      description = c("Default", "Positive", "Month Average", "Average", "Average Comparison", "Peak Flow", "Range")
      color = c("All streams are blue", 
                "Blue: Streams with at least one measured value (Q_cfs) > 0 <br> Grey: Streams with no flow during time range",
                paste0(kableExtra::footnote_marker_symbol(1), "Stream colors are determined by the monthly long term monthly average for each stream"),
                paste0(kableExtra::footnote_marker_symbol(1), "Stream colors are determined by the average stream value during the time range"),
                paste0(kableExtra::footnote_marker_symbol(1), "Stream colors are determined by the subtracting the long term average from the current average"),
                paste0(kableExtra::footnote_marker_symbol(1), "Stream colors are determined by the maximum flow during the time range"),
                paste0(kableExtra::footnote_marker_symbol(1), "Stream colors are determined by subtracting the minumum value during the time range from the maximum"))
      df = data.frame(description, color)
      colnames(df) <- c("", "Color Scheme")
      HTML(paste0(df %>% knitr::kable(escape = F) %>% 
             kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% 
             kableExtra::footnote(
               general = "In all cases, stream width is determined by stream order. ",
               symbol = "The range of values (Qcfs) are determined, split into 8 equal categories, and mapped to a color pallete") %>% 
             kableExtra::column_spec(1, bold = T)), "<br>", "")
    }
    
    else {
      description = c("Time Series", "Change")
      color = c(paste0(kableExtra::footnote_marker_symbol(1), "Stream colors are determined by the value during each time step"),
                paste0(kableExtra::footnote_marker_symbol(1), "Stream colors are determined by calculating the change between the current point and the next"))
      df = data.frame(description, color)
      colnames(df) <- c("", "Color Scheme")
      HTML(paste0(df %>% knitr::kable(escape = F) %>% 
                    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% 
                    kableExtra::footnote(
                      general = "In all cases, stream width is determined by stream order. ",
                      symbol = "The range of values (Qcfs) are determined, split into 8 equal categories, and mapped to a color pallete") %>% 
                    kableExtra::column_spec(1, bold = T)), "<br>", "")
    }
    
  })
  

})