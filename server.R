library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(sp)
library(rgdal)
library(HydroData)

shinyServer(function(input, output, session) {
  
  # Definie Initial Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addLayersControl(
        baseGroups = c("CartoDB","Imagery"),
        overlayGroups = c("USGS Stations", "NHD Flowlines", "AOI"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addScaleBar("bottomright") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  new_location = function(LAT, LONG) {
    get_data(LAT, LONG)
    generate_tables(flow, stats)
    update_map(LAT, LONG)
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
      error_message(" Could not find any features in this region. ")
      return()
    }
    
    flow <<- nhd$flowlines
    
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
                   popup = paste0(paste0(flow@data$gnis_name),
                                  paste0(" COMID:", flow$comid)),
                   popupOptions = c(className = "stream_popup"), 
                   group = "NHD Flowlines"
      ) %>%
      addCircleMarkers(lng = as.numeric(LONG), lat = as.numeric(LAT), radius = 6, color = 'red', stroke = FALSE, fillOpacity = 0.5)
    
    # Don't try and map USGS stations if there are none  
    if(typeof(stats) == "S4") {
      leafletProxy("map") %>%
        addMarkers(data = stats,
                   icon = usgsIcon,
                   group = "USGS Stations"
        )
    }
  }
  
  error_message <- function(message) {
    output$server_problems <- renderText({ message })
  }
  
  calc_bounds <- function(lat, lon) {
    dl = ((5/2)/69) / cos(lat * pi/180)
    south = lat - df
    north = lat + df
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
                  cbind("Unique HUC6 units: ", length(unique(as.numeric(na.omit(unique(substr(flowlines$reachcode,1,6))))))))
    colnames(table) = c('Statistic', 'Value')
    output$Flowlines = renderTable(table, striped = TRUE)
    
    if(typeof(usgs) == "S4") {
      station_data = cbind(usgs$site_name, usgs$site_no, round(usgs$da_sqkm, digits = 0))
    } else {
      station_data = cbind('NA', 'NA', 'NA')
    }
    colnames(station_data) = c("USGS Site", "Site No.", "Drainage Area (SqKm)")
    output$stations = renderTable(station_data, striped = TRUE)
    
    updateSelectInput(session, inputId = "flow_selector", choices = paste0(paste0(flowlines@data$gnis_name),
                                                                           paste0(" COMID:", flowlines$comid)))
 
  }
  
  # Move to current location when possible
  observe({
    if(!is.null(input$lat)){
      #map <- leafletProxy("map")
      #loc = paste(as.numeric(input$lat), as.numeric(input$long), collapse = "")
      updateTextInput(session, "place", value = "Current Location")
      new_location(input$lat, input$long)
      output$data_loc <- renderText({ "Current Location" })
    } else {
        ip_loc = findLatLong()
        if (!is.null(ip_loc$lat)) {
          new_location(ip_loc$lat, ip_loc$lon)
          updateTextInput(session, "place", value = "IP Based Location")
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
      error_message(" Your current location can't be determined.  
 Make sure you have given your browser the necessarry permissions. ")
    }
  })
    
  # Draw Flow Lines
  observeEvent(input$do, {
    output$data_loc <- renderText({ input$place })
    
    # Catch any errors due to geocode lookup
    target_pos = tryCatch({
      geocode(input$place)
    }, warning = function(w) {
      if (grepl("geocode failed with status ZERO_RESULTS", w[1]$message)) {
        error_message(" Not a valid location. ")
      } else if (grepl("geocode failed with status OVER_QUERY_LIMIT", w[1]$message)) {
        error_message(" Google API is not responding.  
 Please wait a few seconds and try again. ")
      }
      return(NA)
    })
    
    if (!is.na(target_pos[1])) {
      error_message("")
      new_location(target_pos$lat, target_pos$lon)
    }
  })
  
  observeEvent(input$mark_flowline, {
    # text = input$flow_selector
    # id = unlist(strsplit(text, split='COMID:', fixed=TRUE))[2]
    # i = match(id, flow@data$comid)
    # center = round(length(flow@lines[[i]]@Lines[[1]]@coords)/4, digits = 0)
    # 
    # leafletProxy("map") %>%
    #   clearGroup("flowline_mark") %>%
    #   addMarkers(lng = flow@lines[[i]]@Lines[[1]]@coords[center,1], 
    #              lat = flow@lines[[i]]@Lines[[1]]@coords[center,2],
    #              popup = text,
    #              popupOptions = labelOptions(direction = "bottom"),
    #              group = "flowline_mark")
    text = input$flow_selector
    id = unlist(strsplit(text, split='COMID:', fixed=TRUE))[2]
    print(id)
    leafletProxy("map") %>%
      clearGroup("NHD Flowlines") %>%
      addPolylines(data = flow, color = ~ifelse(flow$comid == id, "red", "blue"), 
                   weight = flow$streamorde,
                   popup = paste0(paste0(flow@data$gnis_name),
                                  paste0(" COMID:", flow$comid)),
                   popupOptions = c(className = "stream_popup"), 
                   group = "NHD Flowlines"
      )
  })
  
  observe({
    if (is.null(input$goto))
      return()
    output$stream <- renderText({ input$goto$name })
    choose_flow(input$goto$text)
    #output$stream <- renderText({ input$goto$text })
    output$stream <- renderText({ input$flow_selector })
  })
  
  observe({
    output$stream <- renderText({ input$flow_selector })
  })
  
})
