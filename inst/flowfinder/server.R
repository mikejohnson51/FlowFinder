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
      )
  })
  
  # Set map
  output$map <- renderLeaflet({ basemap() })

  # Define reactive values corresponding to current COMID
  update_cur_id <- function(input) {
    values$id = unlist(strsplit(input, split='COMID: ', fixed=TRUE))[2]
    values$i = match(values$id, values$flow_data$nhd@data$comid)
    values$data = values$nwm[values$nwm$COMID == values$flow_data$nhd$comid[values$i],]
    values$normals = norm[norm$COMID == values$flow_data$nhd$comid[values$i],]
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
      incProgress(4/8, detail = "Subsetting Stream Data")
      
      # Determine what state (or if) AOI is in
      state = latlong2state(lat = values$loc$lat, lon = values$loc$lon)
      
      # Set global variable and show notification if no streams in AOI
      # Continue with data prep if there are streams
      if (!exists('nhd', where=values$flow_data) || (is.null(state))) {
        values$any_flow = FALSE
        if (is.null(state)) {
          showNotification("Warning: AOI appears to be outside CONUS", type = "warning", duration = 10)
        }
      } else {
        values$any_flow = TRUE
        # Subset data
        values$nwm = subset_nomads(comids = values$flow_data$comid)
        incProgress(2/8, detail = "Finding Upstream/Downstream")
        
        # Set upstream/downstream data
        values$flow_data$nhd_prep = suppressWarnings(prep_nhd(flines = values$flow_data$nhd))
        values$hmm = get_upstream(flines = values$flow_data$nhd_prep)
      }
      incProgress(1/8, detail = "Mapping")

      # Map data
      clearMarkers()
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
    
    ui_flow_only = c("prevCOMID", "nextCOMID", "flow_selector", 
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
        clearGroup("view-on-map") %>%
        clearGroup("up-stream") %>%
        clearGroup("down-stream")
  }
  
  # Current location button
  observeEvent(input$current_loc, {
    if(!is.null(input$lat)){
      updateTextInput(session = session, inputId =  "place", value = paste(input$lat, input$long, sep = " "), placeholder = "Current Location")
      shinyjs::click("do")
    } else {
      showNotification("Your current location can't be determined. Make sure you have given your browser the necessarry permissions.", type = "error")
    }
  })
  
  # Reset button
  observeEvent(input$reset, { clearMarkers() })
  
  # Mark upstream flows from leaflet popup
  observe({
    if (is.null(input$upStream))
      return()
    clearMarkers()
    mark_up_down(map = leafletProxy("map", session), values = values, stream = input$upStream$comid, data = values$flow_data$nhd[values$flow_data$nhd$comid %in% c(values$hmm[values$hmm$comid == input$upStream$comid, 2]),], group = "up-stream", color = "#84bd00")
  })
  
  # Mark downstream flows from leaflet popup
  observe({
    if (is.null(input$downStream))
      return()
    clearMarkers()
    mark_up_down(map = leafletProxy("map", session), values = values, stream = input$downStream$comid, data = values$flow_data$nhd[values$flow_data$nhd$comid %in% c(values$flow_data$nhd_prep[values$flow_data$nhd_prep$comid == input$downStream$comid, 4]),], group = "down-stream", color = "red")
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
    elements = c("tbl_up", "tbl_down", "prevCOMID", "nextCOMID")
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
  
})