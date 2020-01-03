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
    
    ######### Downloadable Data #########
    
    # CSV file
    if (input$data_csv) {
      path <- paste(paste(loc, Sys.Date(), sep = '_'), "csv", sep = ".")
      fs <- c(fs, path)
      write.table(NWM$data, path, sep = ",", row.names = FALSE)
    }
    
    # RDA
    if (input$data_rda) {
      path <- paste(paste(loc, Sys.Date(), sep = '_'), "rda", sep = ".")
      fs <- c(fs, path)
      data = NWM$data
      save(data, file = path)
    }
    
    # Shape file
    if (input$data_nhd) {
      uid = sample(1:1000000, 1)
      d = paste0("loc", "_shapefile")
      dir.create(file.path(tempdir(), as.character(uid)), showWarnings = FALSE)
      temp = paste0(tempdir(),"/",as.character((uid)))
      setwd(temp)
      print(temp)
      rgdal::writeOGR(obj=rawData()$nhd, dsn= temp, layer="nhd", driver="ESRI Shapefile")
      ls = list.files(path = temp, pattern = 'nhd')
      ls = paste(as.character(uid),"/",ls, sep="")
      print(ls)
      fs = c(fs, ls)
      setwd(tempdir())
    }
    
    ######### Downloadable Plots #########
    
    if (input$plot_png) {
      path <- paste(paste(rawData()$comid[rawData()$nhd$comid == rawData()$comid[values$i]], Sys.Date(), sep = '_'), "png", sep = ".")
      fs <- c(fs, path)
      device <- function(..., width, height) {
        grDevices::png(..., width = 12, height = 5, units = "in",
                       res = 300)
      }
      #values$data$color <- ifelse(values$data$Q_cfs <= cutoff, '#0069b5', 'red')
      ggsave(path, plot = static_plot(values = NWM, selected = input$flow_selector, id = current_id(), normals = normals()),
             device = device)
    }
    
    if (input$plot_dygraph) {
      # path <- paste(paste(rawData()$nhd$comid[rawData()$nhd$comid == rawData()$nhd$comid[values$i]], Sys.Date(), sep = '_'), "html", sep = ".")
      path <- "dygraph.html"
      fs <- c(fs, path)
      htmlwidgets::saveWidget(dygraph(), file = path)
    }
    
    ######### Downloadable Maps #########
    
    # Map
    if (input$maps_flow) {
      path <- paste0("flow_map_", Sys.Date(), ".html" )
      fs <- c(fs, path)
      map <- basemap() %>% 
        clearGroup(group = list("NHD Flowlines", "Location", "USGS Stations", "Water bodies", "AOI")) %>%
        add_location(values = location()) %>% 
        add_bounds(AOI = rawData()$AOI) %>%
        add_water_bodies(wb = rawData()$waterbodies) %>%
        add_flows(data = rawData()$nhd, color = "blue", opacity = 0.5) %>%
        add_stations(values = rawData()$nwis)
      # graph = add_layers(map = basemap(), values = values)
      htmlwidgets::saveWidget(map, file = path)
    }
    
    # High Flows Map
    if (input$maps_floods) {
      path <- paste0("flood_predections_", Sys.Date(), ".html" )
      fs <- c(fs, path)
      # load('data/current_nc/flood_map.rda')
      htmlwidgets::saveWidget(high_flows_data$map, file = path)
    }
    print(fs)
    zip(zipfile=fname, files=fs)
  },
  contentType = "application/zip"
)