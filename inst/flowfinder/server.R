library(FlowFinder)
# library(promises)
# library(future)
# plan(multiprocess)

shinyServer(function(input, output, session) {
  
  ########## MAP TAB ####################################################################
  
  location_information <- reactiveValues()
  flags <- reactiveValues()
  
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
  
  clearMarkers <- function() {
    leafletProxy("map") %>%
      clearGroup(list("view-on-map", "up-stream", "down-stream"))
  }
  
  # Render map
  output$map <- renderLeaflet({ basemap() })
  
  # Get location from search bar
  # location <- eventReactive(input$place, {
  #   get_location(place = input$place)
  # })
  
 location <- reactive({
   get_location(place = input$place)
 })
 
 
  
  rawData <- reactive({
    req(input$place)
    req(location()$lat)
    isolate({
      withProgress(message = 'Analyzing Location', value = 0, {
        incProgress(1/8, detail = "Getting location information")
        if (is.null(location_information$bbox)) {
          AOI <- AOI::getAOI(clip = list(location()$lat, location()$lon, 15, 15))
        } else {
          AOI <- AOI::bbox_sp(location_information$bbox)
        }
        incProgress(2/8, detail = "Finding nearby streams")
        NHD <- HydroData::findNHD(AOI = AOI, ids = TRUE)
        incProgress(4/8, detail = "Finding nearby waterbodies")
        WB <- HydroData::findWaterbodies(AOI = AOI)
        incProgress(6/8, detail = "Finding nearby NWIS stations")
        NWIS <- findNWIS(AOI = AOI)
        location_information$bbox <- NULL
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
      area = region_info$height * region_info$width
      
      if (area < 400) {
        print('here')
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
  
  NWM <- reactive({
    req(input$on_data_tab)
    withProgress(message = 'Fetching Data', value = .6, {
      subset_nomads(comids = rawData()$nhd$comid)
    })
  })
  
  streams <- reactive({
    rawData()$nhd@data %>%
      select(comid, gnis_name) %>%
      mutate(name = paste0(
        paste0(ifelse(is.na(gnis_name), "", gnis_name)),
        paste0(" COMID: ", comid))) %>%
      select(-gnis_name)
  })
  
  # positive_streams <- reactive({
  #   NWM() %>%
  #     filter(Q_cfs > 0) %>%
  #     select(COMID) %>%
  #     distinct() %>%
  #     .$COMID
  # })
  
  max_stream <- reactive({
    req(NWM())
    NWM() %>%
      top_n(1, Q_cfs) %>%
      .$COMID
  })
  
  default_stream <- reactive({
    streams() %>%
      filter(comid == max_stream()[1]) %>%
      .$name
  })
  
  observe({
    req(NWM())
    updatePickerInput(session, 'flow_selector', 
                      choices = streams()$name,
                      selected = default_stream())
  })
  
  current_id <- reactive({
    getIDs(input$flow_selector)[1]
  })
  
  current_data <- reactive({
    req(NWM())
    NWM() %>%
      filter(COMID == current_id())
  })

  current_xts <- reactive({
    req(NWM())
    req(!is.null(input$flow_selector))
    selected = input$flow_selector
    isolate({
      output <- matrix(ncol=length(selected), nrow=length(current_data()$dateTime))
      ids = c()
      print(selected)
      for (j in 1:length(selected)) {
        text = selected[j]
        id = getIDs(text)[1]
        
        data = NWM() %>% 
          filter(COMID == id)
        
        output[,j] <- data$Q_cfs
        ids = c(ids, as.character(id))
      }
      df <- data.frame(output)
      colnames(df) <- ids
      
      # timeZone = lutz::tz_lookup_coords(values$loc$lat, values$loc$lon, method = "accurate")
      xts::xts(df, order.by = lubridate::with_tz(data$dateTime))
    })
  })
  
  output$dygraph <- dygraphs::renderDygraph({
    req(current_xts())
    dygraphs::dygraph(current_xts())
  })
  
  ########## HIGH FLOWS TAB ####################################################################
  
  # Set high flows map
  
  flood_map <- reactive({
    req(input$nav=="high_flows")
    load('data/current_nc/flood_map.rda')
    flood_map
  })
  
  output$flood_map <- renderLeaflet({ 
    req(flood_map())
    flood_map() 
  })
  
  observe({
    print(input$nav)
  })
  
})