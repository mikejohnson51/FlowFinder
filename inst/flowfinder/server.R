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
    req(input$nav=="data")
    withProgress(message = 'Fetching Data', value = .6, {
      subset_nomads(comids = rawData()$nhd$comid)
    })
  })
  
  timezone <- reactive({
    req(input$nav=="data")
    lutz::tz_lookup_coords(location()$lat, location()$lon, method = "accurate")
  })
  
  normals <- reactive({
    req(input$nav=="data")
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
    NWM() %>%
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
    req(NWM(), positive_stream_names())
    updatePickerInput(session, 'flow_selector', 
                      choices = streams()$name,
                      selected = default_stream(),
                      choicesOpt = list(
                        style = ifelse(positive_stream_names(),
                                       yes = "color:#0069b5;font-weight:bold;",
                                       no = "style=color:#a8a8a8")
                      ))
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
      
      cutoff <- norm %>% 
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
    req(upstream_from())
    isolate({
      stream_table(data = upstream_from(), direction = "upstream", current_id = current_id(), session = session)
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
    req(downstream_from())
    stream_table(data = downstream_from(), direction = "downstream", current_id = current_id(), session = session)
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

})