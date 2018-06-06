library(shiny)
library(HydroData)
library(ncdf4)
library(dismo)

source("subset_nomads.R")


shinyServer(function(input, output, session) {

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
  
  new_location = function(LAT, LONG) {
    get_data(LAT, LONG)
    generate_tables(flow, stats)
    update_map(LAT, LONG)
    updateTextInput(session, "place", placeholder = "Search Flowline Finder")
  }
  
  get_data = function(LAT, LONG) {
    # Catch error when no stations are in AOI
    nhd <- tryCatch({
      suppressMessages(findNHD(clip_unit = list(LAT, LONG, 5, 5)))
    },
    error=function(error_message) {
      return(NA)
    }
    )
    
    if (is.na(nhd[1])) {
      error_message("Could not find any features in this region. ")
      return()
    }
    
    flow <<- nhd$flowlines
    ids <<-  flow$comid  ## you'll need this for the NWM subset
    #ids <<-  nhd$ids
    nwm <<- subset_nomads(comids = ids)
    # Catch error when no stations are in AOI
    stats <<- tryCatch({
      suppressMessages(findUSGS(clip_unit = list(LAT, LONG, 5, 5))$nwis)
    },
    error=function(error_message) {
      return(NA)
    }
    )

  }
  
  update_map = function(LAT, LONG) {
    bounds = calc_bounds(LAT, LONG)
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearGroup("NHD Flowlines") %>%
      fitBounds(bounds$west, bounds$south, bounds$east, bounds$north) %>%
      addPolylines(data = flow, color = 'blue', weight = flow$streamorde,
                   popup = paste0(ifelse(is.na(flow@data$gnis_name), "", flow@data$gnis_name),
                                  paste0(" COMID: ", flow$comid)),
                   popupOptions = c(className = "stream_popup"), 
                   group = "NHD Flowlines",
                   
                   highlight = highlightOptions(
                     weight = 10,
                     color = "#666",
                     fillOpacity = 0.7,
                     bringToFront = TRUE)
      ) %>%
      addCircleMarkers(lng = as.numeric(LONG), lat = as.numeric(LAT), radius = 6, color = 'green', stroke = FALSE, fillOpacity = 0.5)
    
    # Don't try and map USGS stations if there are none  
    if(typeof(stats) == "S4") {
      leafletProxy("map") %>%
        addMarkers(data = stats,
                   icon = leaflet::makeIcon(
                     iconUrl= "https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",
                     iconWidth = 40, iconHeight = 20,
                     iconAnchorX = 20, iconAnchorY = 10),
                   
                   group = "USGS Stations",
                   popup = pop <- paste(
                     paste("<strong>Site Number:</strong>",
                       paste0('<a href=',sprintf(
                           "https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",stats$site_no),'>',stats$site_no,"</a>")
                     ),
                     paste("<strong>NHD COMID:ds <<-  flow$comid</strong>", stats$feature_id),
                     paste("<strong>Site Name:</strong>", stats$site_name),
                     sep = "<br/>"
                   ) )
      
    }
  }
  
  error_message <- function(message) {
    output$server_problems <- renderText({ message })
  }
  
  calc_bounds <- function(lat, lon) {
    dl = ((5/2)/69) / cos(lat * pi/180)
    south = lat - ((5/2)/69)
    north = lat + ((5/2)/69)
    west  = lon - dl
    east  = lon + dl
    coords = data.frame(south = south, north = north, west = west, east = east)
  }
  
  choose_flow = function(comid) {
    updateSelectInput(session, inputId = "flow_selector", selected = comid)
  }
  
  generate_tables = function(flowlines, usgs) {
    max_order = max(flowlines@data$streamorde)
    
    table = rbind(cbind("Largest Stream Name: ", flowlines@data$gnis_name[match(max_order, flowlines@data$streamorde)]),
                  cbind("Number of Flowlines: ", length(flowlines)),
                  cbind("Largest Stream Order: ", max_order),
                  cbind("Total Area (SqMi): ", 25),
                  cbind("Unique HUC8 units: ", paste(unique(as.numeric(na.omit(unique(substr(flowlines$reachcode,1,8))))), collapse = ", ")))
    colnames(table) = c('Statistic', 'Value')
    output$Flowlines = renderTable(table, striped = TRUE)
    
    if(typeof(usgs) == "S4") {
      station_data = cbind(usgs$site_name, usgs$site_no, round(usgs$da_sqkm, digits = 0))
    } else {
      station_data = cbind('NA', 'NA', 'NA')
    }
    colnames(station_data) = c("USGS Site", "Site No.", "Drainage Area (SqKm)")
    output$stations = renderTable(station_data, striped = TRUE)
    
    updateSelectInput(session, inputId = "flow_selector", choices = paste0(paste0(ifelse(is.na(flow@data$gnis_name), "", flow@data$gnis_name)),
                                                                           paste0(" COMID: ", flowlines$comid)))
 
  }
  
  # Move to current location when possible
  observe({
    if(!is.null(input$lat)){
      #map <- leafletProxy("map")
      #loc = paste(as.numeric(input$lat), as.numeric(input$long), collapse = "")
      updateTextInput(session, "place", placeholder = "Current Location")
      new_location(input$lat, input$long)
      output$data_loc <- renderText({ "Current Location" })
    } else {
        ip_loc = findLatLong()
        if (!is.null(ip_loc$lat)) {
          new_location(ip_loc$lat, ip_loc$lon)
          updateTextInput(session, "place", placeholder = "IP Based Location")
          output$data_loc <- renderText({ "IP Based Location" })
        }
    }
  })
  
  # Current Location Button
  observeEvent(input$current_loc, {
    if(!is.null(input$lat)){
      new_location(input$lat, input$long)
      updateTextInput(session, "place", value = "Current Location")
    } else {
      error_message("Your current location can't be determined.  
 Make sure you have given your browser the necessarry permissions. ")
    }
  })
    
  # Draw Flow Lines
  observeEvent(input$do, {
    output$data_loc <- renderText({ input$place })
    
    # Catch any errors due to geocode lookup
  target_pos = {trash <-  capture.output(
        suppressMessages( loc <-  dismo::geocode( input$place, output = 'latlon' ) )
      )
      target_pos <<- list(lat = loc$lat, lon = loc$lon)
    }
      
    if (!is.na(target_pos[1])) {
      error_message("")
      new_location(target_pos$lat, target_pos$lon)
    }
  })
  
  observeEvent(input$mark_flowline, {

    text = input$flow_selector
    
    id = unlist(strsplit(text, split='COMID: ', fixed=TRUE))[2]
    
    leafletProxy("map") %>%
      clearGroup("NHD Flowlines") %>%
      setView(lng = mean(flow@lines[flow$comid == id][[1]]@Lines[[1]]@coords[,1]),
              lat = mean(flow@lines[flow$comid == id][[1]]@Lines[[1]]@coords[,2]), 
              zoom = 14) %>% 
      addPolylines(data = flow, color = ~ifelse(flow$comid == id, "red", "blue"), 
                   weight = ~ifelse(flow$comid == id, 15, flow$streamorde),
                   popup = paste0(paste0(flow@data$gnis_name),
                                  paste0(" COMID: ", flow$comid)),
                   popupOptions = c(className = "stream_popup"), 
                   group = "NHD Flowlines",
                   
                   highlight = highlightOptions(
                     weight = 10,
                     color = "#666",
                     fillOpacity = 0.7,
                     bringToFront = TRUE)
      )
  })
  
  observe({
    if (is.null(input$goto))
      return()
    output$stream <- renderText({ input$goto$name })
    choose_flow(input$goto$text)
    output$stream <- renderText({ input$flow_selector })
  })
  
  observe({
    output$stream <- renderText({ input$flow_selector })
  })
  
  updateStreamFlow = function(text) {
    text = input$flow_selector
    id = unlist(strsplit(text, split='COMID: ', fixed=TRUE))[2]
    i = match(id, flow@data$comid)
    data = nwm[nwm$comid == ids[i],]
    

 
   plot( x = data$dateTime,
          y = data$cms,
          type = "b",
          pch = 16,
          col = 'blue',
          lwd =3,
          main = paste0(ifelse(is.na(flow$gnis_name[flow$comid == ids[i]]), "", flow@data$gnis_name[flow$comid == ids[i]]),
                        paste0(" COMID: ", flow$comid[flow$comid == ids[i]],"\nMedium Range National Water Model Forecasts")),
          ylab = "streamflow (cfs)",
          xlab = 'Date and Time', axes = F)
    axis(1, at= seq(min(data$dateTime), max(data$dateTime), 10800), 
         labels= seq(min(nwm$dateTime), max(nwm$dateTime), 10800), 
         cex.axis = .95,
         lwd = 2
         )
    axis(2, at= seq(min(data$cms), max(data$cms), ((max(data$cms) - min(data$cms)) / 10)), 
         labels= round(seq(min(data$cms), max(data$cms), ((max(data$cms) - min(data$cms)) / 10)), 3), 
         las = 2,
         lwd = 2,
         cex.axis = .8)
    
  }
  

  
  output$streamFlow <- renderPlot({
    text = input$flow_selector
    updateStreamFlow(text)
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      loc = input$place
      if (loc == "") {
        loc = "current_location"
      }
      paste(paste(loc, Sys.Date(), sep = '_'), "csv", sep = ".")
    },

    content = function(file) {
      write.table(nwm, file, sep = ",",
                  row.names = FALSE)
    }
  )
  
})
