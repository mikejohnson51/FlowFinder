library(FlowFinder)

shinyServer(function(input, output, session) {
  
  # Include code for download handler
  source("R/server_download_handler.R", local = TRUE)$value
  
  ########## MAP TAB ####################################################################
  
  location_information <- reactiveValues()
  flags <- reactiveValues(nwm_calculated = FALSE,
                          high_flows_loaded = FALSE,
                          filter_initial_draw = FALSE,
                          filterable_data = FALSE)
  
  # Create reactive base map
  # Using as a reactive allows re-use for downloads
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
      leaflet.extras::addDrawToolbar(
        targetGroup='AOI',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(
          selectedPathOptions = leaflet.extras::selectedPathOptions(), 
          remove = FALSE)
      ) %>% 
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ 
                      var num = Math.random();
                      Shiny.onInputChange('currentLoc', {
                        num: num,
                      }); 
                    }"))) 
  })
  
  move_to_current_location <- function() {
    if(!is.null(input$lat)){
      updateSearchInput(session = session, inputId =  "place", 
                        value = paste(input$lat, input$long, sep = " "), 
                        placeholder = "Current Location",
                        trigger = TRUE)
    } else if (!is.null(input$getIP)) {
      updateSearchInput(session = session, inputId =  "place", 
                        value = paste(input$getIP$latitude, input$getIP$longitude, sep = " "),
                        placeholder = "Search FlowFinder",
                        trigger = TRUE)
    }
  }
  
  # Move to current location when possible
  observe({
    lat <- input$lat
    ip <- input$getIP
    move_to_current_location()
  })
  
  observe({
    req(input$currentLoc)
    move_to_current_location()
  })
  
  clearMarkers <- function() {
    leafletProxy("map") %>%
      clearGroup(list("view-on-map", "up-stream", "down-stream"))
  }
  
  # Render map
  output$map <- renderLeaflet({ basemap() })
  
  location <- reactive({
   get_location(place = input$place)
  })
 
  state <- reactive({
   latlong2state(lat = location()$lat, lon = location()$lon)
  })
 
  observe({
    if (is.null(state())) {
      showNotification("Warning: AOI appears to be outside CONUS", type = "warning", duration = 10)
    }
  })
  
  rawData <- reactive({
    req(input$place)
    req(location()$lat)
    req(state())
    isolate({
      withProgress(message = 'Analyzing Location', value = 0, {
        incProgress(1/8, detail = "Getting location information")
        if (is.null(location_information$bbox)) {
          location_information$area <- 15^2
          AOI <- AOI::getAOI(clip = list(location()$lat, location()$lon, 15, 15))
        } else {
          AOI <- AOI::bbox_sp(location_information$bbox)
        }
        incProgress(2/8, detail = "Finding nearby streams")
        NHD <- HydroData::findNHD(AOI = AOI)
        #NHD = NHD$nhd$comid
        incProgress(4/8, detail = "Finding nearby waterbodies")
        WB <- HydroData::findWaterbodies(AOI = AOI)
        incProgress(6/8, detail = "Finding nearby NWIS stations")
        NWIS <- findNWIS(AOI = AOI)
        location_information$bbox <- NULL
        flags$nwm_calculated <- FALSE
        flags$filter_initial_draw = FALSE
        flags$filterable_data = FALSE
      })
      list(AOI = NHD$AOI, nhd = NHD$nhd, waterbodies = WB$waterbodies, nwis = NWIS$nwis)
    })
  })
  
  observe({
    req(rawData())
    isolate({
      leafletProxy("map") %>%
        clearGroup(group = list("NHD Flowlines", "Location", "USGS Stations", "Water bodies", "AOI")) %>%
        add_location(values = location()) %>% 
        add_bounds(AOI = rawData()$AOI) %>%
        add_water_bodies(wb = rawData()$waterbodies) %>%
        add_flows(data = rawData()$nhd, color = "blue", opacity = 0.5) %>%
        add_stations(values = rawData()$nwis)
    })
  })
  
  # Get Downstream Values
  downstream <- reactive({
    prep_nhd(flines = rawData()$nhd)
  })
  
  # Map Downstream Flows
  observe({
    req(input$downStream)
    clearMarkers()
    downstream() %>% 
      filter(comid == input$downStream$comid) %>% 
      .$toCOMID %>% (function(df) {
        mark_up_down(map = leafletProxy("map", session), 
                     values = rawData(), 
                     stream = input$downStream$comid, 
                     data = rawData()$nhd[rawData()$nhd$comid %in% df,], 
                     group = "down-stream", 
                     color = "red")
      }) 
  })
  
  upstream <- reactive({
    get_upstream(flines = downstream())
  })
  
  # Reset button
  observeEvent(input$reset, { clearMarkers() })
  
  # Map Upstream Flows
  observe({
    req(input$upStream)
    clearMarkers()
    upstream() %>% 
      filter(comid == input$upStream$comid) %>% 
      .$fromCOMID %>% (function(df) {
        mark_up_down(map = leafletProxy("map", session), 
                     values = rawData(), 
                     stream = input$upStream$comid, 
                     data = rawData()$nhd[rawData()$nhd$comid %in% df,], 
                     group = "up-stream", 
                     color = "#84bd00")
      }) 
  })
  
  observeEvent(input$map_draw_edited_features,{
    feature <- input$map_draw_edited_features
    
    if (!length(feature$features) == 0) {
      lng <- c(
        feature$features[[1]]$geometry$coordinates[[1]][[1]][[1]],
        feature$features[[1]]$geometry$coordinates[[1]][[3]][[1]]
      )
      
      lat <- c(
        feature$features[[1]]$geometry$coordinates[[1]][[1]][[2]],
        feature$features[[1]]$geometry$coordinates[[1]][[2]][[2]]
      )
      
      location_information$bbox <- append(lng, lat)
      
      region_info <- AOI::bbox_sp(location_information$bbox) %>% describe()
      
      location_information$cent_lat <- region_info$latCent
      location_information$cent_lng <- region_info$lngCent
      location_information$area = region_info$height * region_info$width
      
      if (location_information$area < 400) {
        updateSearchInput(session = session, inputId =  "place", 
                          value = paste(location_information$cent_lat, location_information$cent_lng, sep = " "), 
                          placeholder = "Current Location",
                          trigger = TRUE)
      } else {
        confirmSweetAlert(
          session = session, inputId = "aoiconfirmation", type = "warning",
          text = "This AOI is greater than 400 square miles and may take a while to load.",
          title = "Are you sure?", 
          danger_mode = TRUE
        )
      }
    }
  })
  
  observeEvent(input$aoiconfirmation,{
    if (input$aoiconfirmation) {
      updateTextInput(session = session, inputId =  "place", value = paste(location_information$cent_lat, location_information$cent_lng, sep = " "), placeholder = "Current Location")
      shinyjs::click("place_search")
    } else {
      leafletProxy("map", session) %>% 
        clearGroup("AOI") %>% 
        add_bounds(AOI = rawData()$AOI)
    }
  })
  
  observeEvent(input$map_shape_click,{
    object = input$map_shape_click
    if (object$group == "AOI") {
      runjs("
            var editButton = document.getElementsByClassName('leaflet-draw-edit-edit')[0];
            editButton.click();
            ")
    } 
  })
  
  observeEvent(input$map_click, {
    req(input$map_draw_editstart)
    isolate({
      loc = input$map_click
      bbox = rawData()$AOI@bbox
      if (loc$lng < bbox[1] || loc$lng > bbox[3] || loc$lat > bbox[2] || loc$lat < bbox[4]) {
        runjs("
              var saveButton = document.getElementsByClassName('leaflet-draw-actions')[0].firstChild.firstChild;
              saveButton.click();
              ")
      }
    })
    
  })
  
  observe({
    req(input$map_draw_editstart)
    runjs("
          document.getElementsByClassName('leaflet-draw-tooltip-subtext')[0].innerHTML = 'Press esc to cancel changes';
          var el = document.getElementsByClassName('leaflet-draw-tooltip')[0].children[2];
          var newEl = document.createElement('span');
          newEl.innerHTML = '<br>Press enter or outside of AOI to save changes';
          newEl.className = 'editConfirm';
          el.parentNode.insertBefore(newEl, el.nextSibling);
          ")
  })
  
  observeEvent(input[["enterPressed"]], {
    req(input$map_draw_editstart)
    runjs("
          var saveButton = document.getElementsByClassName('leaflet-draw-actions')[0].firstChild.firstChild;
          saveButton.click();
          ")
  })
  
  observeEvent(input[["escPressed"]], {
    req(input$map_draw_editstart)
    runjs("
          var exitButton = document.getElementsByClassName('leaflet-draw-actions')[0].lastChild.firstChild;
          exitButton.click();
          ")
  })
  
  # reset editing variables
  observeEvent(input$map_draw_editstop, {
    session$sendCustomMessage("started_editing", "stop")
  })
  
  ########## DATA TAB ####################################################################
  
  NWM <- reactiveValues()
  
  
  # Select flowline from popup
  observe({
    if (is.null(input$goto))
      return()
    updatePickerInput(session = session, inputId = "flow_selector", selected = input$goto$text)
  })
  
  # Calculate NWM when data tab is switched to
  # flags$nwm_calculated is used to prevent unnecessarily re-calculating NWM
  observe({
    req(input$nav=="data" || input$nav=="filter")
    isolate({
      if(!flags$nwm_calculated) {
        withProgress(message = 'Fetching Data', value = .6, {
          flags$nwm_calculated <- TRUE
          NWM$data <- subset_nomads(comids = rawData()$nhd$comid)
        })
      }
    })
  })
  
  timezone <- reactive({
    req(input$nav=="data" || input$nav=="Info")
    lutz::tz_lookup_coords(location()$lat, location()$lon, method = "accurate")
  })
  
  normals <- reactive({
    req(input$nav=="data" || input$nav=="filter" || input$nav=="high_flows")
    fst::read_fst(path = month_files)
  })
  
  streams <- reactive({
    rawData()$nhd@data %>%
      select(comid, gnis_name) %>%
      mutate(name = paste0(
        paste0(ifelse(is.na(gnis_name), "", gnis_name)),
        paste0(" COMID: ", comid))) %>%
      select(-gnis_name)
  })
  
  positive_streams <- reactive({
    NWM$data %>%
      filter(Q_cfs > 0) %>%
      select(COMID) %>%
      distinct() %>%
      .$COMID 
  })
  
  positive_stream_names <- reactive({
    req(positive_streams())
    purrr::map_lgl(getIDs(streams()$name), ~(. %in% positive_streams()))
  })
  
  max_stream <- reactive({
    req(NWM$data)
    NWM$data %>%
      top_n(1, Q_cfs) %>%
      .$COMID
  })
  
  default_stream <- reactive({
    streams() %>%
      filter(comid == max_stream()[1]) %>%
      .$name
  })

  observe({
    req(NWM$data, positive_stream_names())
    isolate({
      default <- ifelse(is.null(input$goto), default_stream(), input$goto)
      updatePickerInput(session, 'flow_selector', 
                        choices = streams()$name,
                        selected = default,
                        choicesOpt = list(
                          style = ifelse(positive_stream_names(),
                                         yes = "color:#0069b5;font-weight:bold;",
                                         no = "style=color:#a8a8a8")
                        ))
    })
  })
  
  # Previous stream button
  observeEvent(input$prevCOMID, {
    current <- which(streams()$name == input$flow_selector)
    if(current > 1){
      updatePickerInput(session, "flow_selector",
                        selected = streams()$name[current - 1])
    }
  })
  
  # Next stream button
  observeEvent(input$nextCOMID, {
    current <- which(streams()$name == input$flow_selector)
    if(current < length(streams()$name)){
      updatePickerInput(session, "flow_selector",
                        selected = streams()$name[current + 1])
    }
  })
  
  # View on map button
  observeEvent(input$mark_flowline, {
    clearMarkers()
    reaches = rawData()$nhd[rawData()$nhd$comid %in% getIDs(input$flow_selector), ]
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
  
  current_id <- reactive({
    getIDs(input$flow_selector)[1]
  })
  
  current_data <- reactive({
    req(NWM$data)
    NWM$data %>%
      filter(COMID == current_id())
  })

  current_xts <- reactive({
    req(NWM$data)
    req(!is.null(input$flow_selector))
    selected = input$flow_selector
    isolate({
      output <- matrix(ncol=length(selected), nrow=length(current_data()$dateTime))
      ids = c()
      for (j in 1:length(selected)) {
        text = selected[j]
        id = getIDs(text)[1]
        
        data = NWM$data %>% 
          filter(COMID == id)
        
        output[,j] <- data$Q_cfs
        ids = c(ids, as.character(id))
      }
      df <- data.frame(output)
      colnames(df) <- ids
      xts::xts(df, order.by = lubridate::with_tz(data$dateTime, tzone = timezone()), tz = timezone())
      # xts::xts(df, order.by = lubridate::with_tz(data$dateTime))
    })
  })
  
  dygraph <- reactive({
    req(current_xts())
    
    graph <- dygraphs::dygraph(current_xts())  %>%
      dyOptions(drawPoints = TRUE,
                pointSize = 2,
                gridLineColor = "lightblue",
                useDataTimezone = TRUE) %>%
      dyRangeSelector(height = 20) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 1) %>%
      dyAxis("y", label = "Streamflow (cfs)" )%>%
      dyLegend(show = "onmouseover")
    
    if (length(input$flow_selector) == 1) {
      
      mn = mean(zoo::coredata(current_xts()), na.rm = TRUE)
      std = sd(zoo::coredata(current_xts()), na.rm = TRUE)
      
      cutoff <- normals() %>% 
        filter(COMID == current_id())
      
      cutoff = cutoff[,2] * 35.3147
    
      graph = graph %>%
        dyOptions(
          useDataTimezone = TRUE,
          drawPoints = TRUE, 
          pointSize = 2,
          gridLineColor = "lightblue",
          fillGraph = TRUE, 
          fillAlpha = 0.1 ) %>% 
        dyLimit(cutoff, 
                strokePattern = "solid", 
                color = "red", 
                label = paste0("Monthly Average (", round(cutoff,2), " cfs)")) %>%
        dyShading(from = mn - std, 
                  to = mn + std, 
                  axis = "y")
    }
    graph
  })
  
  output$dygraph <- dygraphs::renderDygraph({
    dygraph()
  })
  
  # Keep as easy way to vizualize downloadable ggplot
  # output$plot2<-renderPlot({
  #   req(NWM$data, input$flow_selector)
  #   static_plot(values = NWM, selected = input$flow_selector, id = current_id(), normals = normals())
  # })
  
  upstream_from <- reactive({
    req(current_id())
    upstream = data.frame(Upstream=NA)[numeric(0), ]
    up = rawData()$nhd[rawData()$nhd$comid %in% c(upstream()[upstream()$comid == current_id(), 2]),]
    if (length(up) > 0) {
      upstream = data.frame(paste0(paste0(ifelse(is.na(up$gnis_name), "", up$gnis_name)), paste0(" COMID: ", up$comid)))
      colnames(upstream) = c("Upstream")
    }
    upstream
  })
  
  # Upstream Table
  output$tbl_up <- DT::renderDataTable({
    upstream <- upstream_from()
    isolate({
      stream_table(data = upstream, direction = "upstream", current_id = current_id(), session = session)
    })
  })
  
  downstream_from <- reactive({
    req(current_id())
    downstream = data.frame(Downstream=NA)[numeric(0), ]
    down = rawData()$nhd[rawData()$nhd$comid %in% c(downstream()[downstream()$comid == current_id(), 4]),]
    if (length(down) > 0) {
      downstream = data.frame(paste0(paste0(ifelse(is.na(down$gnis_name), "", down$gnis_name)), paste0(" COMID: ", down$comid)))
      colnames(downstream) = c("Downstream")
    }
    downstream
  })
  
  # Downstream Table
  output$tbl_down <- DT::renderDataTable({
    downstream <- downstream_from()
    isolate({
      stream_table(data = downstream, direction = "downstream", current_id = current_id(), session = session)
    })
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
  
  # Activate/Deactivate buttons depending on number of COMIDs selected
  observe({
    elements = list("tbl_up", "tbl_down", "prevCOMID", "nextCOMID")
    if (length(input$flow_selector) > 1) {
      show_hide_all(elements = elements, action = "hide")
    } else {
      show_hide_all(elements = elements, action = "show")
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
  
  
  
  ########## HIGH FLOWS TAB ####################################################################
  
  # Set high flows map
  high_flows_data <- reactiveValues()
  
  observe({
    # req(input$nav=="high_flows")
    tab = input$nav
    isolate({
      if(!flags$high_flows_loaded && (tab == "high_flows" || tab == "data")) {
        load('data/current_nc/flood_map.rda')
        flags$high_flows_loaded <- TRUE
        high_flows_data$map <- flood_map
      }
    })
  })
  
  
  output$flood_map <- renderLeaflet({ 
    req(high_flows_data$map)
    high_flows_data$map
  })
  

  colors = rep(c("orange", "green", "red", "purple",
                 "lightblue", "lightgreen", "pink", "lightred", "gray",
                 "darkblue", "darkred", "darkgreen", "darkpurple"), 10)
  
  # Generate dygraph plot
  output$flood_dygraph <- dygraphs::renderDygraph({
    req(input$map_flood$comid, normals())
    comid = input$map_flood$comid
    isolate({
      if (comid == "reset") {
        high_flows_data$data <- NULL
      } else {
        if (is.null(high_flows_data$data_ids)) {
          high_flows_data$data_ids <- comid
        } else {
          high_flows_data$data_ids <- c(high_flows_data$data_ids, comid)
        }
        withProgress(message = 'Making plot', value = .5, {
          new_data = dygraph_flood(comid = comid, data = high_flows_data$data, 
                                   number = length(high_flows_data$data_ids),
                                   normals = normals())
          high_flows_data$data = new_data$data_set
          new_data$graph
        })
      }
    })
  })
  
  high_flows_selected_id <- reactive({
    req(input$map_flood$comid)
    input$map_flood$comid
  })
  
  observe({
    req(input$map_flood$comid)
    comid = input$map_flood$comid
    isolate({
      if (comid == "reset") {
        high_flows_data$data_ids <- NULL
        leafletProxy("flood_map", session) %>%
          clearGroup("flood_markers")
      } else {
        color = colors[length(high_flows_data$data_ids) + 1]
        leafletProxy("flood_map", session) %>%
          addAwesomeMarkers(
            icon = awesomeIcons(
              icon = 'circle',
              iconColor = '#FFFFFF',
              library = 'fa',
              markerColor = color
            ),
            lat = as.numeric(input$map_flood$lat),
            lng = as.numeric(input$map_flood$lon),
            popup = paste0("<strong>NHD COMID: </strong>", comid),
            group = "flood_markers"
          )
      }
    })
  })
  
  ########## INFO TAB ####################################################################
  
  # Page title
  output$data_loc <- renderText({ 
    input$place 
  })
  
  # Table 1: Station info
  output$stations = renderTable({
    station_table(rawData()$nwis)
  }, striped = TRUE, sanitize.text.function = function(x) x)

  # Table 2: Flowline info
  output$Flowlines = renderTable({
    flowlines_table(values = rawData()$nhd, 
                    area = location_information$area, 
                    waterbodies = length(rawData()$waterbodies))
  }, striped = TRUE)

  # Table 3: NWM info
  output$meta = renderTable({
    nwm_table(timezone = timezone())
  }, striped = TRUE)

  ########## FILTER TAB ####################################################################
  
  filterable <- reactiveValues()
  values <- reactiveValues() # TODO Replace
  
  find_diff <- function(id) {
    diff(NWM$data %>% filter(COMID == id) %>% .$Q_cfs)
  }
  
  observe({
    req(NWM$data, input$nav=="filter")
    isolate({
      if(!flags$filterable_data) {
        withProgress(message = 'Generating Filterable Data', value = .6, {
          
          vals = NWM$data %>% 
            tidyr::spread(dateTime, Q_cfs) %>% 
            select(-agency_code) %>% 
            rename("comid" = COMID)
          
          nwm_summary <- NWM$data %>% 
            group_by(COMID) %>% 
            summarise(max = max(Q_cfs), 
                      mean = mean(Q_cfs), 
                      min = min(Q_cfs)) %>%
            mutate(range = max - min) %>% 
            rename("comid" = COMID)
          
          averages = normals() %>%
            `colnames<-`(c("comid", "month_avg")) %>% 
            mutate(month_avg = month_avg * 35.3147)
            
          diff_matrix <- matrix(ncol=length(unique(NWM$data$dateTime)), nrow=length(nwm_summary$comid))
          for (j in 1:length(nwm_summary$comid)) {
            id = nwm_summary$comid[j]
            diff_matrix[j,] <- c(id, find_diff(id))
          }
          
          diff_df <- data.frame(diff_matrix)
          colnames(diff_df) <- c("comid", paste0('der_',unique(NWM$data$dateTime)[1:39]))
            
          filterable$data <- rawData()$nhd %>%
            inner_join(vals, by = "comid") %>% 
            inner_join(nwm_summary, by = "comid") %>% 
            inner_join(averages, by = "comid") %>% 
            mutate(mean_dif = mean - month_avg) %>% 
            inner_join(diff_df, by = "comid")
            
          values$palette <- create_palette(vals = nwm_summary$max)
          values$range_palette <- create_palette(vals = nwm_summary$range)
          values$mean_palette <- create_palette(vals = nwm_summary$mean)
          values$month_palette <- create_palette(vals = filterable$data$month_avg)
            
          mag = max(abs(c(max(diff_matrix[,2:40], na.rm = T), min(diff_matrix[,2:40], na.rm = T))))
          values$deriv_palette <- create_palette(vals = mag, continuous = FALSE)
            
          mag = max(abs(c(max(filterable$data$mean_dif, na.rm = T), min(filterable$data$mean_dif, na.rm = T))))
            
          values$mean_dif_palette <- create_palette(vals = mag, continuous = FALSE)
          flags$filterable_data <- TRUE
     
        })
      }
    })
  })
  
  output$displayOptions <- renderUI({
    # req(values$nwm)
    req(input$display_type)
    isolate({
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
        values$times = unique(as.character(.POSIXct(NWM$data$dateTime, tz = "GMT")))

        clearGroup(map = leafletProxy("map_filter"), group = "NHD Flowlines")
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
  
  output$map_filter <- renderLeaflet({ 
    req(input$nav=="filter")
    isolate({
      if(!flags$filter_initial_draw) {
        # flags$filter_initial_draw <- TRUE
        basemap_filter() %>%  
          add_bounds(AOI = rawData()$AOI)# %>%
          # add_flows(data = rawData()$nhd, color = divergent_colors(1))
      }
    })
  })
  
  # Options Available For Table
  output$nhd_options <- renderUI({
    generate_options(href = "collapseNhd", inputID = "nhdSelector", 
                     choices = c("comid",
                                 "gnis_name",
                                 "reachcode",
                                 "lengthkm",
                                 "streamorde",
                                 "slope"), 
                     selected = c("gnis_name", "comid", "lengthkm", "streamorde", "reachcode", "slope"))
  })
  
  
  # Filterable table
  output$nhd_table <- DT::renderDataTable({
    req(filterable$data, input$nhdSelector)
    data <- filterable$data@data %>% 
      select(one_of(input$nhdSelector))
    base_table(data)
  })
  
  # Generate data set based on filtered table
  filtered_data <- reactive({
    req(filterable$data, input$nhd_table_rows_all)
    isolate({
      filterable$data %>% 
        slice(input$nhd_table_rows_all)
    })
  })
  
  
  
  observe({
    req(filtered_data())
    values$static_dic = list(
      default = list(divergent_colors(1), list(divergent_colors(1), c('Flowline'), NULL)),
      positive = list(~positive_pal(filtered_data()$max),list(divergent_colors(2), c('0', '>0'), "Q_cfs")),
      month_avg = list(~values$month_palette$pal(filtered_data()$month_avg), values$month_palette),
      mean = list(~values$mean_palette$pal(filtered_data()$mean), values$mean_palette),
      mean_dif = list(~values$mean_dif_palette$pal(filtered_data()$mean_dif), values$mean_dif_palette),
      max_value = list(~values$palette$pal(filtered_data()$max), values$palette),
      range = list(~values$range_palette$pal(filtered_data()$range), values$range_palette)
    )
  })
  
  observe({
    req(input$display_type == "Static")
    req(values$static_dic, input$flow_display)
    type = input$flow_display
    tab = input$nav
    isolate({
      data = eval(parse(text = paste0('values$static_dic$', "`", type,"`")))
      clearGroup(map = leafletProxy("map_filter"), group = "NHD Flowlines")
      add_flows(map = leafletProxy("map_filter"), data = filtered_data(), color = data[[1]])
      if (!type %in% c("default", "positive")) {
        add_legend(pal = data[[2]])
      } 
      else {
        add_legend(colors = data[[2]][[1]], labels = data[[2]][[2]], title = data[[2]][[3]])
      }
    })
  })
  
  observe({
    req(rawData()$nhd, filtered_data())
    values$dynamic_dic = list(
      # name = list(color, pal, legend)
      deriv = list('filtered_data()@data$`der_', values$deriv_palette$pal, values$deriv_palette),
      time_series = list('filtered_data()@data$`', values$palette$pal, values$palette),
      positive = list('filtered_data()@data$`', positive_pal, list(divergent_colors(2), c('0', '>0'), "Q_cfs"))
    )
  })
  
  observe({
    req(input$display_type == "Dynamic")
    req(input$selected_time, input$flow_display_dy, values$dynamic_dic)
    type = input$flow_display_dy
    tab = input$nav
    isolate({
      time = as.character.POSIXt(input$selected_time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      data = eval(parse(text = paste0('values$dynamic_dic$', "`", type,"`")))
      
      color_determination = eval(parse(text = paste0(data[[1]], time,"`")))
      add_flows(map = leafletProxy("map_filter"), data = filtered_data(), ~data[[2]](color_determination))
      
      if (input$flow_display_dy == "positive") {
        add_legend(colors = data[[3]][[1]], labels = data[[3]][[2]], title = data[[3]][[3]])
      }
      else {
        add_legend(pal = data[[3]])
      }
    })
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

