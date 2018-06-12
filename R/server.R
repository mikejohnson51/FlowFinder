library(shiny)
library(HydroData)
library(ncdf4)
library(dismo)
library(shinyjs)
library(DT)

library(data.table)
library(fst)
library(dplyr)

source("../R/subset_nomads_rda.R")
source("../R/nhdModifier.R")

# Generate icon for usgs stations
USGSicon = leaflet::makeIcon(
  iconUrl= "www/USGS_logo.png",
  iconWidth = 40, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 10)

shinyServer(function(input, output, session) {
  
  ########## Initial Setup ####################################################################
  
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
  
  error_message <- function(message) {
    output$server_problems <- renderText({ message })
  }
  
  # Set up reactive values
  values <- reactiveValues()
  
  # On go, calculate reactive values
  observeEvent(input$do, {
    # Check if input is likely a lat/lon pair
    split = unlist(strsplit(input$place, split=" ", fixed=TRUE))
    if ((length(split) == 2) && !is.na(as.numeric(split[1])) && !is.na(as.numeric(split[2])) )  {
        values$lat = as.numeric(split[1])
        values$lon = as.numeric(split[2])
    } else {
      loc = dismo::geocode( input$place, output = 'latlon' )
      values$lat = loc$lat
      values$lon = loc$lon
    }
    
    clip = list(values$lat, values$lon, 5, 5)
    
    values$nhd = tryCatch({
      suppressMessages(findNHD(clip_unit = clip, ids = TRUE))
    },
    error=function(error_message) {
      return(NA)
    })
    if (is.na(values$nhd[1])) {
      error_message("Could not find any features in this region. ")
      return()
    }

    values$flow = values$nhd$flowlines
    values$ids =  values$flow$comid
    values$ids2 = values$nhd$ids
    
    values$stats = tryCatch({
      suppressMessages(findUSGS(clip_unit = clip)$nwis)
    },
    error=function(error_message) {
      return(NA)
    })
    
    values$nwm = subset_nomads_rda(comids = values$ids2)

    if (input$do == 1) {
      updateTextInput(session = session, inputId =  "place", value = "")
    }
    
    values$nhd_prep = suppressWarnings(prep_nhd(flines = values$flow))
    values$hmm = get_upstream(flines = values$nhd_prep)

   })
  
  # Function to determine bounds
  calc_bounds <- function(lat, lon) {
    dl = ((5/2)/69) / cos(lat * pi/180)
    df = ((5/2)/69)
    south = lat - df
    north = lat + df
    west  = lon - dl
    east  = lon + dl
    coords = data.frame(south = south, north = north, west = west, east = east)
  }
  
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
  
  # On go, draw map
  observeEvent(input$do, {
    bounds = calc_bounds(values$lat, values$lon)
    clearMarkers()
    leafletProxy("map", session) %>%
      clearGroup("NHD Flowlines") %>%
      fitBounds(bounds$west, bounds$south, bounds$east, bounds$north) %>%
      addPolylines(data = values$flow, color = 'blue', weight = values$flow$streamorde,
                   popup = paste(sep = " ",
                                 paste0("<b><a class='open-stream'>",paste0(ifelse(is.na(values$flow@data$gnis_name), "", values$flow@data$gnis_name)),
                                        paste0(" COMID: ", values$flow$comid),"</a></b></br>"),
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
      addCircleMarkers(lng = as.numeric(values$lon), lat = as.numeric(values$lat), radius = 6, color = 'green', stroke = FALSE, fillOpacity = 0.5)
      error_message("")
    # Don't try and map USGS stations if there are none  
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
      addPolylines(data = values$flow[values$flow$comid == input$upStream$comid,],
                   color = "blue",
                   opacity = 1,
                   group = "up-stream",
                   options = pathOptions(clickable = FALSE))  %>%
      addPolylines(data = values$flow[values$flow$comid %in% c(values$hmm[values$hmm$comid == input$upStream$comid, 2]),], 
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
      addPolylines(data = values$flow[values$flow$comid == input$downStream$comid,],
                   color = "blue",
                   opacity = 1,
                   group = "down-stream",
                   options = pathOptions(clickable = FALSE))  %>%
      addPolylines(data = values$flow[values$nhd$flowlines$comid %in% c(values$nhd_prep[values$nhd_prep$comid == input$downStream$comid, 4]),],
                   color = "red",
                   opacity = 1,
                   group = "down-stream",
                   options = pathOptions(clickable = FALSE))
  })
  
  ########## Information ####################################################################
  
  # Change header
  observeEvent(input$do, {
    output$data_loc <- renderText({ input$place })
  })
  
  # Render tables
  observeEvent(input$do, {
    max_order = max(values$flow@data$streamorde)
    
    table = rbind(cbind("Largest Stream Name: ", values$flow@data$gnis_name[match(max_order, values$flow@data$streamorde)]),
                  cbind("Number of Flowlines: ", length(values$flow)),
                  cbind("Largest Stream Order: ", max_order),
                  cbind("Total Area (SqMi): ", 25),
                  cbind("Unique HUC8 units: ", paste(unique(as.numeric(na.omit(unique(substr(values$flow$reachcode,1,8))))), collapse = ", ")))
    colnames(table) = c('Statistic', 'Value')
    output$Flowlines = renderTable(table, striped = TRUE)
    
    if(typeof(values$stats) == "S4") {
      station_data = cbind(paste0('<a href=',sprintf(
        "https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",values$stats$site_no),' target="_blank">',values$stats$site_name,"</a>"),values$stats$site_no, round(values$stats$da_sqkm, digits = 0))
    } else {
      station_data = cbind('NA', 'NA', 'NA')
    }
    colnames(station_data) = c("USGS Site", "Site No.", "Drainage Area (SqKm)")
    output$stations = renderTable({station_data}, striped = TRUE, sanitize.text.function = function(x) x)
  })
  
  ########## Stream Flow ####################################################################
  
  # Change header
  observeEvent(input$flow_selector, {
    output$stream <- renderText({ input$flow_selector })
  })
  
  # Update Drop-Down Options
  observeEvent(input$do, {
    max_qcms = values$nwm[match(max(values$nwm$Q_cfs), values$nwm$Q_cfs),]$COMID
    name = values$flow[values$flow$comid == max_qcms,]$gnis_name
    e = paste0(paste0(ifelse(is.na(name), "", name)), paste0(" COMID: ", max_qcms))
    values$choices = as.list(paste0(paste0(ifelse(is.na(values$flow@data$gnis_name), "", values$flow@data$gnis_name)),
                            paste0(" COMID: ", values$flow$comid)))
    updateSelectInput(session = session, inputId = "flow_selector", choices = values$choices , selected = e)
  })
  
  # Set values for current stream
  observe({
    req(input$flow_selector)
    text = input$flow_selector
    values$id = unlist(strsplit(text, split='COMID: ', fixed=TRUE))[2]
    values$i = match(values$id, values$flow@data$comid)
    values$data = values$nwm[values$nwm$COMID == values$ids[values$i],]
  })
  
  # Draw Plot
  output$streamFlow <- renderPlot({
    plot( x = values$data$dateTime,
          y = values$data$Q_cfs,
          type = "b",
          pch = 16,
          col = 'blue',
          lwd =3,
          main = paste0(ifelse(is.na(values$flow$gnis_name[values$flow$comid == values$ids[values$i]]), "", values$flow@data$gnis_name[values$flow$comid == values$ids[values$i]]),
                        paste0(" COMID: ", values$flow$comid[values$flow$comid == values$ids[values$i]],"\nMedium Range National Water Model Forecasts")),
          ylab = "streamflow (cfs)",
          xlab = 'Date and Time', axes = F)
    axis(1, at= seq(min(values$data$dateTime), max(values$data$dateTime), 10800), 
         labels= seq(min(values$nwm$dateTime), max(values$nwm$dateTime), 10800), 
         cex.axis = .95,
         lwd = 2
    )
    axis(2, at= seq(min(values$data$Q_cfs), max(values$data$Q_cfs), ((max(values$data$Q_cfs) - min(values$data$Q_cfs)) / 10)), 
         labels= round(seq(min(values$data$Q_cfs), max(values$data$Q_cfs), ((max(values$data$Q_cfs) - min(values$data$Q_cfs)) / 10)), 1), 
         las = 2,
         lwd = 2,
         cex.axis = .8)
  })
  
  # Previous Stream
  observeEvent(input$prevCOMID, {
    current <- which(values$choices == input$flow_selector)
    if(current > 1){
      updateSelectInput(session, "flow_selector",
                        selected = values$choices[current - 1])
    }
  })
  
  # Next Stream
  observeEvent(input$nextCOMID, {
    current <- which(values$choices == input$flow_selector)
    if(current < length(values$choices)){
      updateSelectInput(session, "flow_selector",
                        selected = values$choices[current + 1])
    }
  })
  
  
  # Used to select flowline from popup
  observe({
    if (is.null(input$goto))
      return()
    updateSelectInput(session = session, inputId = "flow_selector", selected = input$goto$text)
  })
  
  # View on map button
  observeEvent(input$mark_flowline, {
    clearMarkers()
    leafletProxy("map", session) %>%
      setView(lng = mean(values$flow@lines[values$flow$comid == values$id][[1]]@Lines[[1]]@coords[,1]),
              lat = mean(values$flow@lines[values$flow$comid == values$id][[1]]@Lines[[1]]@coords[,2]), 
              zoom = 14) %>% 
      addPolylines(data = values$flow[values$flow$comid == values$id, ], 
                   color = "red", 
                   weight = 15,
                   opacity = 0.9,
                   options = pathOptions(clickable = FALSE),
                   group = "view-on-map"
      )
  })

  observe({
    req(values$flow)
    upstream = data.frame(Upstream=NA)[numeric(0), ]
    downstream = data.frame(Downstream=NA)[numeric(0), ]
    up = values$flow[values$flow$comid %in% c(values$hmm[values$hmm$comid == values$id, 2]),]
    down = values$flow[values$flow$comid %in% c(values$nhd_prep[values$nhd_prep$comid == values$id, 4]),]
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
  
  output$tbl_up <- DT::renderDataTable({
    if (length(values$upstream) > 0) {
      df <- values$upstream %>%
        mutate(View = paste('<a class="go-stream" href="" data-stream="', Upstream, '"><i class="fa fa-eye"></i></a>', sep=""))
      action <- DT::dataTableAjax(session, df)
      DT::datatable(df, options = list(ajax = list(url = action), dom = 't'), escape = FALSE)
    } else {
      df <- values$upstream
      df <- rbind(df, "No streams in this AOI")
      colnames(df) = "Upstream"
      DT::datatable(df, options = list(dom = 't'), escape = FALSE)
    }
    
  })
  
  output$tbl_down <- DT::renderDataTable({
    if (length(values$downstream) > 0) {
      df <- values$downstream %>%
        mutate(View = paste('<a class="go-stream" href="" data-stream="', Downstream, '"><i class="fa fa-eye"></i></a>', sep=""))
      action <- DT::dataTableAjax(session, df)
      DT::datatable(df, options = list(ajax = list(url = action), dom = 't'), escape = FALSE)
    } else {
      df <- values$downstream
      df <- rbind(df, "No streams in this AOI")
      colnames(df) = "Downstream"
      DT::datatable(df, options = list(dom = 't'), escape = FALSE)
    }
    
  })
  
  observe({
    if (is.null(input$switchStream))
      return()
    updateSelectInput(session = session, inputId = "flow_selector", selected = input$switchStream$stream)
  })

  # Data tables
  output$tbl = DT::renderDataTable(server = FALSE, {
    DT::datatable(values$nwm, 
    extensions = 'Buttons', 
    options = list(dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                   scroller = TRUE)
    )}
  )
  
  # Proxy used to manipulate search
  DTproxy <- dataTableProxy("tbl", session)
  
  observeEvent(input$flow_selector, {
    id = unlist(strsplit(input$flow_selector, split='COMID: ', fixed=TRUE))[2]
    updateSearch(proxy = DTproxy, keywords = list(global = id, columns = NULL))
  })
  
  observe({
    if (is.null(input$default_stream))
      return()
    updateSearch(proxy = DTproxy, keywords = list(global = input$default_stream$comid, columns = NULL))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("flowlines", "zip", sep=".")
    },
    content = function(fname) {
      uid = sample(1:1000000, 1)
      dir.create(file.path(tempdir(), as.character(uid)), showWarnings = FALSE)
      temp = paste0(tempdir(),"/",as.character((uid)))
      setwd(temp)
      rgdal::writeOGR(obj=values$flow, dsn= temp, layer="nhd", driver="ESRI Shapefile")
      fs = list.files(path = temp, pattern = 'nhd')
      zip(zipfile = fname, files = fs )
    },
    contentType = "application/zip"
  )
  

  })
