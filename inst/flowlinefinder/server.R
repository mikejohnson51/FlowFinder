library(FlowlineFinder)
library(dygraphs)
options(shiny.sanitize.errors = FALSE)

month = as.numeric(substr(list.files("data/current_nc")[1], 1,2))
month_files = list.files("data/", pattern = as.character(month), full.names = T)
norm = fst::read_fst(path = month_files)
size = 15
load('data/current_nc/flood_map.rda')

# Generate icon for usgs stations
USGSicon = leaflet::makeIcon(
  iconUrl= "www/USGS_logo.png",
  iconWidth = 40, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 10)

shinyServer(function(input, output, session) {
  
  ########## Initial Setup ####################################################################
  # Set up reactive values
  values <- reactiveValues()
  
  # Define Initial Map
  output$map <- renderLeaflet({
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
  
  output$flood_map <- renderLeaflet({
    flood_map
  })
  
  error_message <- function(message) {
    output$server_problems <- renderText({ message })
  }
  
  # On go, calculate reactive values
  observeEvent(input$do, {
    shinyjs::disable("do")
    start.time <- Sys.time()
    withProgress(message = 'Analyzing Location', value = 0, {
      incProgress(1/6, detail = "Getting location coordinates")
      # Check if input is likely a lat/lon pair
      split = unlist(strsplit(input$place, split=" ", fixed=TRUE))
      if ((length(split) == 2) && !is.na(as.numeric(split[1])) && !is.na(as.numeric(split[2])) )  {
        values$lat = as.numeric(split[1])
        values$lon = as.numeric(split[2])
      } else {
        if (input$place == "") {
          point = "National Water Center"
        } else {
          point = input$place
        }
        loc = AOI::getPoint(name = point)
        values$lat = loc$lat
        values$lon = loc$lon
      }
      
      clip = list(values$lat, values$lon, size, size)
      AOI = AOI::getAOI(clip = clip)
      
      incProgress(2/6, detail = "Getting Spatial Objects")
      values$nhd = tryCatch({
        suppressMessages(HydroData::findNHD(clip = AOI, ids = TRUE))
      },
      error=function(error_message) {
        return(NA)
      })
      if (is.na(values$nhd[1])) {
        error_message("Could not find any features in this region. ")
        return()
      }
      
      incProgress(1/6, detail = "Getting USGS Stations")
      values$stats = tryCatch({
        suppressMessages(HydroData::findUSGS(clip = AOI)$nwis)
      },
      error=function(error_message) {
        return(NA)
      })
      
      incProgress(1/6, detail = "Subsetting Stream Data")
      values$nwm = subset_nomads_rda_drop(comids = values$nhd$ids)
      
      if (input$do == 1) {
        updateTextInput(session = session, inputId =  "place", value = "")
      }
      
      values$nhd_prep = suppressWarnings(prep_nhd(flines = values$nhd$flowlines))
      values$hmm = get_upstream(flines = values$nhd_prep)
      
      incProgress(1/6, detail = "Mapping")
    })
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    print(time.taken)
    shinyjs::enable("do")
    withProgress(message = 'Mapping Location', value = 0, {
      clearMarkers()
      incProgress(5/6, detail = "Flowlines")
      leafletProxy("map", session) %>%
        clearGroup("NHD Flowlines") %>%
        clearGroup("Location") %>%
        clearGroup("USGS Stations") %>%
        fitBounds(AOI@bbox[1], AOI@bbox[2], AOI@bbox[3], AOI@bbox[4]) %>%
        addRectangles(
          lng1 = AOI@bbox[1] , lat1 = AOI@bbox[2],
          lng2 = AOI@bbox[3] , lat2 = AOI@bbox[4],
          fillColor = "transparent",
          group = 'AOI', color = "red"
        ) %>%
        addPolylines(data = values$nhd$flowlines, color = 'blue', weight = values$nhd$flowlines$streamorde,
                     popup = paste(sep = " ",
                                   paste0("<b><a class='open-stream'>",paste0(ifelse(is.na(values$nhd$flowlines@data$gnis_name), "", values$nhd$flowlines@data$gnis_name)),
                                          paste0(" COMID: ", values$nhd$flowlines$comid),"</a></b></br>"),
                                   '<a class="stream-data"><i class="fa fa-line-chart"></i></a>',
                                   '<a class="upstream-flow"><i class="fa fa-angle-double-up"></i></a>',
                                   '<a class="downstream-flow"><i class="fa fa-angle-double-down"></i></a>'
                     ),
                     options = popupOptions(className = "stream_popup"), 
                     group = "NHD Flowlines",
                     highlight = highlightOptions(
                       weight = 10,
                       color = "#666",
                       fillOpacity = 0.7,
                       bringToFront = FALSE)
        ) %>%
        addCircleMarkers(lng = as.numeric(values$lon), lat = as.numeric(values$lat), radius = 6, color = 'green', 
                         stroke = FALSE, fillOpacity = 0.5, group = "Location")
      error_message("")
      # Don't try and map USGS stations if there are none
      incProgress(1/6, detail = "USGS Stations")
      if(typeof(values$stats) == "S4") {
        leafletProxy("map", session) %>%
          addMarkers(data = values$stats,
                     icon = USGSicon,
                     group = "USGS Stations",
                     popup = pop <- paste(
                       paste("<strong>Site Number:</strong>",
                             paste0('<a href=',sprintf(
                               "https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",values$stats$site_no),' target="_blank">',values$stats$site_no,"</a>")
                       ),
                       paste("<strong>NHD COMID:</strong>", values$stats$feature_id),
                       paste("<strong>Site Name:</strong>", values$stats$site_name),
                       sep = "<br/>"
                     ) )
        
      }
    })
    output$data_loc <- renderText({ input$place })
    
    max_order = max(values$nhd$flowlines@data$streamorde)
    table = rbind(cbind("Largest Stream Order: ", max_order),
                  cbind("Largest Stream Name: ", values$nhd$flowlines@data$gnis_name[match(max_order, values$nhd$flowlines@data$streamorde)]),
                  cbind("Number of Flowlines: ", length(values$nhd$flowlines)),
                  cbind("Total Area (SqMi): ", size^2),
                  cbind("Unique HUC8 units: ", paste(unique(as.numeric(na.omit(unique(substr(values$nhd$flowlines$reachcode,1,8))))), collapse = ", ")))
    colnames(table) = c('Hydrography', 'Value')
    output$Flowlines = renderTable(table, striped = TRUE)
    
    if(typeof(values$stats) == "S4") {
      station_data = cbind(paste0('<a href=',sprintf(
        "https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",values$stats$site_no),' target="_blank">',values$stats$site_name,"</a>"),values$stats$site_no, round(values$stats$da_sqkm, digits = 0))
    } else {
      station_data = cbind('NA', 'NA', 'NA')
    }
    colnames(station_data) = c("USGS Site", "Site No.", "Drainage Area (SqKm)")
    output$stations = renderTable({station_data}, striped = TRUE, sanitize.text.function = function(x) x)
    
    dateTime = read.csv('data/current_nc/dateTime.csv', stringsAsFactors = FALSE)
    timeZones = fst::read.fst('data/comids_w_tz.fst')
    timeZone = timeZones[timeZones$COMID==values$nhd$flowlines$comid[1],]$tz
    dif = as.POSIXct(paste(dateTime$date, dateTime$time), "%Y-%m-%d %H", tz = "GMT") - as.POSIXct(paste(dateTime$date, dateTime$time), "%Y-%m-%d %H", tz = timeZone)
    
    nwm_table = rbind(cbind("Forcast Type: ", "Medium"),
                         cbind("Forcast Date: ", dateTime$date),
                         cbind("Forcast Time: ", paste(dateTime$time, "UTC")),
                         cbind("Local Time Zone: ", timeZone),
                         cbind("Time Difference: ", dif)
                        )
    rm(dateTime, timeZones)
    
    colnames(nwm_table) = c("NWM", "Value")
    output$meta = renderTable(nwm_table, striped = TRUE)
    
    
    
    
    
    
    max_qcms = values$nwm[match(max(values$nwm$Q_cfs), values$nwm$Q_cfs),]$COMID
    name = values$nhd$flowlines[values$nhd$flowlines$comid == max_qcms,]$gnis_name

    values$choices = as.list(paste0(paste0(ifelse(is.na(values$nhd$flowlines@data$gnis_name), "", values$nhd$flowlines@data$gnis_name)),
                                    paste0(" COMID: ", values$nhd$flowlines$comid)))
    
    non_zero = unique(values$nwm[values$nwm$Q_cfs > 0,]$COMID)
    non_zeros = c()
    for (stream in values$choices) {
      id = unlist(strsplit(stream, split='COMID: ', fixed=TRUE))[2]
      non_zeros = c(non_zeros, id %in% non_zero)
    }
    
    text = paste0(paste0(ifelse(is.na(name), "", name)), paste0(" COMID: ", max_qcms))
  
    updatePickerInput(session, 'flow_selector', choices = values$choices, selected = text,
                      choicesOpt = list(
                        style = ifelse(non_zeros,
                                       yes = "color:#0069b5;font-weight:bold;",
                                       no = "style=color:#a8a8a8")
                      ))
  
    values$id = unlist(strsplit(text, split='COMID: ', fixed=TRUE))[2]
    values$i = match(values$id, values$nhd$flowlines@data$comid)
    values$data = values$nwm[values$nwm$COMID == values$nhd$flowlines$comid[values$i],]
    values$normals = norm[norm$COMID == values$nhd$flowlines$comid[values$i],]
    
    })
  
  # Move to current location when possible
  observe({
    if(!is.null(input$lat)){
      updateTextInput(session = session, inputId =  "place", value = paste(input$lat, input$long, sep = " "), placeholder = "Current Location")
      shinyjs::click("do")
    } else if (!is.null(input$getIP)) {
      updateTextInput(session = session, inputId = "place", value = paste(input$getIP$latitude, input$getIP$longitude, sep = " "), placeholder = "Search Flowline Finder")
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
      addPolylines(data = values$nhd$flowlines[values$nhd$flowlines$comid == input$upStream$comid,],
                   color = "blue",
                   opacity = 1,
                   group = "up-stream",
                   options = pathOptions(clickable = FALSE))  %>%
      addPolylines(data = values$nhd$flowlines[values$nhd$flowlines$comid %in% c(values$hmm[values$hmm$comid == input$upStream$comid, 2]),], 
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
      addPolylines(data = values$nhd$flowlines[values$nhd$flowlines$comid == input$downStream$comid,],
                   color = "blue",
                   opacity = 1,
                   group = "down-stream",
                   options = pathOptions(clickable = FALSE))  %>%
      addPolylines(data = values$nhd$flowlines[values$nhd$flowlines$comid %in% c(values$nhd_prep[values$nhd_prep$comid == input$downStream$comid, 4]),],
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
    values$id = unlist(strsplit(text, split='COMID: ', fixed=TRUE))[2]
    values$i = match(values$id, values$nhd$flowlines@data$comid)
    values$data = values$nwm[values$nwm$COMID == values$nhd$flowlines$comid[values$i],]
    values$normals = norm[norm$COMID == values$nhd$flowlines$comid[values$i],]
  })
  
  dygraph_plot <- function() {
    
    df = data.frame(time = values$data$dateTime)
    
    for (stream in input$flow_selector) {
      text = stream
      id = unlist(strsplit(text, split='COMID: ', fixed=TRUE))[2]
      i = match(id, values$nhd$flowlines@data$comid)
      data = values$nwm[values$nwm$COMID == values$nhd$flowlines$comid[i],]
      df[as.character(id)] = data$Q_cfs
    }
    rownames(df) = df[[1]]
    #title = ifelse((input$flow_selector) == 1,lengthpaste0(ifelse(is.na(values$nhd$flowlines$gnis_name[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]]), "", paste0(values$nhd$flowlines@data$gnis_name[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]], " ")),
     #              paste0("COMID: ", values$nhd$flowlines$comid[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]])), "Multiple Reaches Selected")
    graph = dygraphs::dygraph(df) %>%
      dyRangeSelector(height = 20) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 1) %>%
      dyAxis("y", label = "Streamflow (cfs)" )%>%
      dyLegend(show = "onmouseover") %>%
      dyOptions(drawPoints = TRUE, 
                pointSize = 2,
                gridLineColor = "lightblue",
                labelsUTC = TRUE)

    if (length(input$flow_selector) == 1) {
      cutoff = values$normals[,2] * 35.3147
      mn = mean(df[[2]], na.rm = TRUE)
      std = sd(df[[2]], na.rm = TRUE)
      graph = graph %>%
        dyOptions(
          drawPoints = TRUE, 
          pointSize = 2,
          gridLineColor = "lightblue",
          labelsUTC = TRUE,
          fillGraph = TRUE, 
          fillAlpha = 0.1
        )  %>%
        dyLimit(cutoff, strokePattern = "solid", color = "red", label = paste0("Monthly Average (", round(cutoff,2), " cfs)")) %>%
        dyShading(from = mn - std, to = mn + std, axis = "y")
    }
    
    return(graph)
  }
  
  output$dygraph <- dygraphs::renderDygraph({
    dygraph_plot()
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
  
  # View on map button
  observeEvent(input$mark_flowline, {
    clearMarkers()
    leafletProxy("map", session) %>%
      setView(lng = mean(values$nhd$flowlines@lines[values$nhd$flowlines$comid == values$id][[1]]@Lines[[1]]@coords[,1]),
              lat = mean(values$nhd$flowlines@lines[values$nhd$flowlines$comid == values$id][[1]]@Lines[[1]]@coords[,2]), 
              zoom = 14) %>% 
      addPolylines(data = values$nhd$flowlines[values$nhd$flowlines$comid == values$id, ], 
                   color = "red", 
                   weight = 15,
                   opacity = 0.9,
                   options = pathOptions(clickable = FALSE),
                   group = "view-on-map"
      )
  })
  
  observe({
    req(values$nhd$flowlines)
    upstream = data.frame(Upstream=NA)[numeric(0), ]
    downstream = data.frame(Downstream=NA)[numeric(0), ]
    up = values$nhd$flowlines[values$nhd$flowlines$comid %in% c(values$hmm[values$hmm$comid == values$id, 2]),]
    down = values$nhd$flowlines[values$nhd$flowlines$comid %in% c(values$nhd_prep[values$nhd_prep$comid == values$id, 4]),]
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
    if (length(input$flow_selector) > 1) {
      shinyjs::hide("tbl_up")
      shinyjs::hide("tbl_down")
      shinyjs::hide("prevCOMID")
      shinyjs::hide("nextCOMID")
    } else {
      shinyjs::show("tbl_up")
      shinyjs::show("tbl_down")
      shinyjs::show("prevCOMID")
      shinyjs::show("nextCOMID")
    }
  })
  
  stream_table <- function(data = NULL, direction = NULL) {
    if (length(data) > 0) {
      df <- data %>%
        dplyr::mutate(View = paste('<a class="go-stream" href="" data-stream="', data[[1]], '"><i class="fa fa-eye"></i></a>', sep=""))
      
      all = data.frame(paste0("All ", "(",nrow(df), ")"), paste('<a class="go-stream" href="" data-stream="', paste(data[[1]],collapse=","), '"><i class="fa fa-eye"></i></a>', sep=""))
      df = rbind(setNames(all, names(df)), df)
      
      
      
      action <- DT::dataTableAjax(session, df, rownames = FALSE)
      DT::datatable(df,
                    options = list(ajax = list(url = action), 
                                   dom = 't'
                                   ), 
                    escape = FALSE, 
                    selection = 'none', rownames = FALSE
                  )
    } else {
      df <- data
      df <- rbind(df, paste0("No ", direction, " reaches from COMID ", values$id))
      colnames(df) = ifelse(direction == "upstream", "Upstream", "Downstream")
      DT::datatable(df, options = list(dom = 't'), escape = FALSE, selection = 'none')
    }
  } 
  
  output$tbl_up <- DT::renderDataTable({
    stream_table(data = values$upstream, direction = "upstream")
  })
  
  output$tbl_down <- DT::renderDataTable({
    stream_table(data = values$downstream, direction = "downstream")
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
    if (input$data_csv || input$data_nhd || input$data_rda || input$plot_png || input$plot_dygraph || input$maps_floods) {
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
        rgdal::writeOGR(obj=values$nhd$flowlines, dsn= temp, layer="nhd", driver="ESRI Shapefile")
        ls = list.files(path = temp, pattern = 'nhd')
        ls = paste(d,"/",ls, sep="")
        fs = c(fs, ls)
        setwd(tempdir())
      }
      
      ######### Plots #########
      
      if (input$plot_png) {
        path <- paste(paste(values$nhd$flowlines$comid[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]], Sys.Date(), sep = '_'), "png", sep = ".")
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
                      title = paste0(ifelse(is.na(values$nhd$flowlines$gnis_name[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]]), "", paste0(values$nhd$flowlines@data$gnis_name[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]], " ")),
                                     paste0("COMID: ", values$nhd$flowlines$comid[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]]))) +
                 theme(plot.title = element_text(color="#0069b5", size=16, face="bold.italic"),
                       legend.position="bottom"),
               device = device)
      }
      
      if (input$plot_dygraph) {
        path <- paste(paste(values$nhd$flowlines$comid[values$nhd$flowlines$comid == values$nhd$flowlines$comid[values$i]], Sys.Date(), sep = '_'), "html", sep = ".")
        fs <- c(fs, path)
        graph = dygraph_plot()
        htmlwidgets::saveWidget(graph, file = path)
      }
      
      ######### Maps #########
      
      if (input$maps_floods) {
        path <- paste0("flood_predections_", Sys.Date(), ".html" )
        fs <- c(fs, path)
        htmlwidgets::saveWidget(flood_map, file = path)
      }
      
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
    })