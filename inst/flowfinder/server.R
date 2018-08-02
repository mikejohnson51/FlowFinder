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
  
  # Display error messages
  error_message <- function(message) {
    output$server_problems <- renderText({ message })
  }
  
  # Define reactive values corresponding to current COMID
  update_cur_id <- function(input) {
    values$id = unlist(strsplit(input, split='COMID: ', fixed=TRUE))[2]
    values$i = match(values$id, values$flow_data$nhd@data$comid)
    values$data = values$nwm[values$nwm$COMID == values$flow_data$nhd$comid[values$i],]
    values$normals = norm[norm$COMID == values$flow_data$nhd$comid[values$i],]
  }
  
  # On go, calculate reactive values
  observeEvent(input$do, {
    
    # Clear error messages
    error_message("")
    
    # Don't let user enter new location while processing previous
    shinyjs::disable("do")
    
    # Initialize progress bar
    withProgress(message = 'Analyzing Location', value = 0, {
      
      # Get initial location
      values$loc = get_location(place = input$place)
      incProgress(1/8, detail = "Getting Spatial Objects")
      
      # Get spatial data
      values$flow_data = AOI::getAOI(clip = list(values$loc$lat, values$loc$lon, size, size)) %>% 
                         findNHD(ids = TRUE) %>% 
                         findWaterbodies() %>% 
                         findNWIS()
      incProgress(4/8, detail = "Subsetting Stream Data")

      # Subset data
      values$nwm = subset_nomads_rda_drop(comids = values$flow_data$comid)
      incProgress(2/8, detail = "Finding Upstream/Downstream")
      
      # Set upstream/downstream data
      values$flow_data$nhd_prep = suppressWarnings(prep_nhd(flines = values$flow_data$nhd))
      values$hmm = get_upstream(flines = values$flow_data$nhd_prep)
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
    suppressMessages(
      leafletProxy("map", session) %>%
        clearGroup("view-on-map") %>%
        clearGroup("up-stream") %>%
        clearGroup("down-stream")
    )
  }
  
  # Current location button
  observeEvent(input$current_loc, {
    if(!is.null(input$lat)){
      updateTextInput(session = session, inputId =  "place", value = paste(input$lat, input$long, sep = " "), placeholder = "Current Location")
      shinyjs::click("do")
    } else {
      error_message(" Your current location can't be determined.
                    Make sure you have given your browser the necessarry permissions. ")
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
    dygraph_plot(values = values, selected = input$flow_selector )
  })
  
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
    stream_table(data = values$up_down_stream$upstream, direction = "upstream", values = values, session = session)
  })
  
  # Downstream Table
  output$tbl_down <- DT::renderDataTable({
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
  
})