shinyServer(function(input, output, session) {
  
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
        overlayGroups = c("USGS Stations", "NHD Flowlines"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomleft"
      )
  })
  
  # Set map
  output$map <- renderLeaflet({
     basemap()
  })
  
  # Set high flows map
  output$flood_map <- renderLeaflet({
    flood_map
  })
  
  error_message <- function(message) {
    output$server_problems <- renderText({ message })
  }
  
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
    
    # Initialize Progress Bar
    withProgress(message = 'Analyzing Location', value = 0, {
      
      # Get Initial Location
      incProgress(1/6, detail = "Getting location coordinates")
      values$loc = get_location(place = input$place)
      
      
      # Get Spatial Data
      incProgress(3/6, detail = "Getting Spatial Objects")
      values$flow_data = AOI::getAOI(clip = list(values$loc$lat, values$loc$lon, size, size)) %>% 
                         findNHD(ids = TRUE) %>% 
                         findWaterbodies() %>% 
                         findNWIS()
      AOI = values$flow_data$AOI
      
      #Subset Data
      incProgress(1/6, detail = "Subsetting Stream Data")
      values$nwm = subset_nomads_rda_drop(comids = values$flow_data$comid)
      
      if (input$do == 1) {
        updateTextInput(session = session, inputId =  "place", value = "")
      }
      
      incProgress(1/6, detail = "Finding Upstream/Downstream")
      # Set Upstream/Downstream Data
      values$flow_data$nhd_prep = suppressWarnings(prep_nhd(flines = values$flow_data$nhd))
      values$hmm = get_upstream(flines = values$flow_data$nhd_prep)
      
      # Map Data
      incProgress(1/6, detail = "Mapping")
      clearMarkers()
      incProgress(5/6, detail = "Flowlines")
      leafletProxy("map") %>%
        clearGroup("NHD Flowlines") %>%
        clearGroup("Location") %>%
        clearGroup("USGS Stations") %>%
        add_bounds(AOI = AOI) %>% 
        add_water_bodies(wb = values$flow_data$waterbodies) %>% 
        add_flows(values = values) %>%
        add_stations(values = values)
      
    })
    
    error_message("")
    shinyjs::enable("do")

    ################# INFO TAB ############################
    
    # Page title
    output$data_loc <- renderText({ input$place })
    
    #Table 1: Station info
    output$stations = renderTable({station_table(values)}, striped = TRUE, sanitize.text.function = function(x) x)
  
    # Table 2: Flowline info
    output$Flowlines = renderTable(flowlines_table(values = values), striped = TRUE)
    
    # Table 3: NWM info
    output$meta = renderTable(nwm_table(values), striped = TRUE)
    
  
    ################# DATA TAB ############################
    
    # Get and set initial COMID choices 
    choices = set_choices(values = values)
    values$choices = choices$choices
 
    updatePickerInput(session, 'flow_selector', choices = values$choices, selected = choices$default,
                      choicesOpt = list(
                        style = ifelse(choices$non_zeros,
                                       yes = "color:#0069b5;font-weight:bold;",
                                       no = "style=color:#a8a8a8")
                      ))
  
    # Set initial values based on default COMID
    update_cur_id(choices$default)
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
  
  ########## MAP ####################################################################
  
  # Fucntion to clear markers
  clearMarkers <- function() {
    suppressMessages(
      leafletProxy("map", session) %>%
        clearGroup("view-on-map") %>%
        clearGroup("up-stream") %>%
        clearGroup("down-stream")
    )
  }
  
  # Current Location Button
  observeEvent(input$current_loc, {
    if(!is.null(input$lat)){
      updateTextInput(session = session, inputId =  "place", value = paste(input$lat, input$long, sep = " "), placeholder = "Current Location")
      shinyjs::click("do")
    } else {
      error_message(" Your current location can't be determined.
                    Make sure you have given your browser the necessarry permissions. ")
    }
    })
  
  # Reset Button
  observeEvent(input$reset, {
    clearMarkers()
  })
  
  # Mark upstream flows from leaflet popup
  observe({
    if (is.null(input$upStream))
      return()
    clearMarkers()
    leafletProxy("map", session) %>%
      addPolylines(data = values$flow_data$nhd[values$flow_data$nhd$comid == input$upStream$comid,],
                   color = "blue",
                   opacity = 1,
                   group = "up-stream",
                   options = pathOptions(clickable = FALSE))  %>%
      addPolylines(data = values$flow_data$nhd[values$flow_data$nhd$comid %in% c(values$hmm[values$hmm$comid == input$upStream$comid, 2]),], 
                   color = "#84bd00",
                   opacity = 1,
                   group = "up-stream",
                   options = pathOptions(clickable = FALSE))
  })
  
  # Mark downstream flows from leaflet popup
  observe({
    if (is.null(input$downStream))
      return()
    clearMarkers()
    leafletProxy("map", session) %>%
      addPolylines(data = values$flow_data$nhd[values$flow_data$nhd$comid == input$downStream$comid,],
                   color = "blue",
                   opacity = 1,
                   group = "down-stream",
                   options = pathOptions(clickable = FALSE))  %>%
      addPolylines(data = values$flow_data$nhd[values$flow_data$nhd$comid %in% c(values$flow_data$nhd_prep[values$flow_data$nhd_prep$comid == input$downStream$comid, 4]),],
                   color = "red",
                   opacity = 1,
                   group = "down-stream",
                   options = pathOptions(clickable = FALSE))
  })
  
  ########## Stream Flow ####################################################################
  
  # Change header
  observeEvent(input$flow_selector, {
    output$stream <- renderText({ input$flow_selector })
  })
  
  observeEvent(input$flow_selector, {
    req(input$flow_selector)
    text = input$flow_selector
    update_cur_id(text)
  })
  
  output$dygraph <- dygraphs::renderDygraph({
    dygraph_plot(values = values, selected = input$flow_selector )
  })
  
  # Previous Stream
  observeEvent(input$prevCOMID, {
    current <- which(values$choices == input$flow_selector)
    if(current > 1){
      updatePickerInput(session, "flow_selector",
                           selected = values$choices[current - 1])
    }
  })
  
  # Next Stream
  observeEvent(input$nextCOMID, {
    current <- which(values$choices == input$flow_selector)
    if(current < length(values$choices)){
      updatePickerInput(session, "flow_selector",
                           selected = values$choices[current + 1])
    }
  })
  
  
  # Used to select flowline from popup
  observe({
    if (is.null(input$goto))
      return()
    updatePickerInput(session = session, inputId = "flow_selector", selected = input$goto$text)
  })
  
  
  getIDs <- function(streams) {
    ids = c()
    for (stream in streams) {
      ids = c(ids, unlist(strsplit(stream, split='COMID: ', fixed=TRUE))[2])
    }
    print(length(ids))
    return(ids)
  }
  
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
  
  observe({
    req(values$flow_data$nhd)
    upstream = data.frame(Upstream=NA)[numeric(0), ]
    downstream = data.frame(Downstream=NA)[numeric(0), ]
    up = values$flow_data$nhd[values$flow_data$comid %in% c(values$hmm[values$hmm$comid == values$id, 2]),]
    down = values$flow_data$nhd[values$flow_data$comid %in% c(values$flow_data$nhd_prep[values$flow_data$nhd_prep$comid == values$id, 4]),]
    if (length(up) > 0) {
      upstream = data.frame(paste0(paste0(ifelse(is.na(up$gnis_name), "", up$gnis_name)), paste0(" COMID: ", up$comid)))
      colnames(upstream) = c("Upstream")
    }
    if (length(down) > 0) {
      downstream = data.frame(paste0(paste0(ifelse(is.na(down$gnis_name), "", down$gnis_name)), paste0(" COMID: ", down$comid)))
      colnames(downstream) = c("Downstream")
    } 
    values$upstream = upstream
    values$downstream = downstream
  })
  
  observe({
    elements = c("tbl_up", "tbl_down", "prevCOMID", "nextCOMID")
    if (length(input$flow_selector) > 1) {
      show_hide_all(elements = elements, action = "hide")
    } else {
      show_hide_all(elements = elements, action = "show")
    }
  })
  
  output$tbl_up <- DT::renderDataTable({
    stream_table(data = values$upstream, direction = "upstream", values = values, session = session)
  })
  
  output$tbl_down <- DT::renderDataTable({
    stream_table(data = values$downstream, direction = "downstream", values = values, session = session)
  })
  
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
  

  observe({
    if (input$data_csv || input$data_nhd || input$data_rda || input$plot_png || input$plot_dygraph || input$maps_floods || input$maps_flow) {
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
  
  mapdown <- reactive({
      basemap() %>% add_flows
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output", "zip", sep=".")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      
      loc = input$place
      if (loc == "") {
        loc = "current_location"
      }
      
      ######### DATA #########
      
      # CSV file
      if (input$data_csv) {
        path <- paste(paste(loc, Sys.Date(), sep = '_'), "csv", sep = ".")
        fs <- c(fs, path)
        write.table(values$nwm, path, sep = ",", row.names = FALSE)
      }
      
      if (input$data_rda) {
        path <- paste(paste(loc, Sys.Date(), sep = '_'), "rda", sep = ".")
        fs <- c(fs, path)
        data = values$nwm
        save(data, file = path)
      }
      
      if (input$data_nhd) {
        uid = sample(1:1000000, 1)
        d = paste0(loc, "_shapefile")
        dir.create(file.path(tempdir(), as.character(uid)), showWarnings = FALSE)
        temp = paste0(tempdir(),"/",as.character((uid)))
        setwd(temp)
        rgdal::writeOGR(obj=values$flow_data$nhd, dsn= temp, layer="nhd", driver="ESRI Shapefile")
        ls = list.files(path = temp, pattern = 'nhd')
        ls = paste(d,"/",ls, sep="")
        fs = c(fs, ls)
        setwd(tempdir())
      }
      
      ######### Plots #########
      
      if (input$plot_png) {
        path <- paste(paste(values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], Sys.Date(), sep = '_'), "png", sep = ".")
        fs <- c(fs, path)
        device <- function(..., width, height) {
          grDevices::png(..., width = 8, height = 4, units = "in",
                         res = 300)
        }
        cutoff = values$normals[,2] * 35.3147
        values$data$color <- ifelse(values$data$Q_cfs <= cutoff, '#0069b5', 'red')
        ggsave(path, plot = 
                 ggplot()+
                 geom_line(data = values$data, aes(x = dateTime, y = Q_cfs, color="Medium Range Forecast"), size = 1.5, alpha=0.4 )  +
                 geom_point(data = values$data, aes(x = dateTime, y = Q_cfs), size = 2, color = values$data$color) +
                 geom_area(data = values$data, aes(x = dateTime, y = Q_cfs),fill = '#0069b5', alpha = .1) +
                 geom_hline(aes(yintercept = cutoff, colour = "Average Monthly Flow"), alpha = .2, size=5, show.legend = TRUE) +
                 scale_colour_manual("",
                                     breaks = c("Medium Range Forecast", "Average Monthly Flow"),
                                     values = c("red","#0069b5" )) +
                 labs(x = "Date and Time",
                      y = "Streamflow (cfs)",
                      title = paste0(ifelse(is.na(values$flow_data$nhd$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]]), "", paste0(values$flow_data$nhd@data$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], " ")),
                                     paste0("COMID: ", values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]]))) +
                 theme(plot.title = element_text(color="#0069b5", size=16, face="bold.italic"),
                       legend.position="bottom"),
               device = device)
      }
      
      if (input$plot_dygraph) {
        path <- paste(paste(values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], Sys.Date(), sep = '_'), "html", sep = ".")
        fs <- c(fs, path)
        graph = dygraph_plot(values = values, selected = input$flow_selector )
        htmlwidgets::saveWidget(graph, file = path)
      }
      
      ######### Maps #########
      
      if (input$maps_floods) {
        path <- paste0("flood_predections_", Sys.Date(), ".html" )
        fs <- c(fs, path)
        htmlwidgets::saveWidget(flood_map, file = path)
      }
      
      if (input$maps_flow) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        path <- paste0("flow_map_", Sys.Date(), ".html" )
        fs <- c(fs, path)
        clip = list(values$loc$lat, values$loc$lon, size, size)
        AOI = AOI::getAOI(clip = clip)
        graph = basemap() %>% 
                add_bounds(AOI = AOI) %>% 
                add_water_bodies(wb = values$flow_data$waterbodies) %>% 
                add_flows(values = values) %>%
                add_stations(values = values)
        htmlwidgets::saveWidget(graph, file = path)
        #mapview::mapshot(mapdown(), file = path, cliprect = "viewport")
      }
    
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
    })