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
    
    ######### Downloadable Plots #########
    
    if (input$plot_png) {
      path <- paste(paste(values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], Sys.Date(), sep = '_'), "png", sep = ".")
      fs <- c(fs, path)
      device <- function(..., width, height) {
        grDevices::png(..., width = 12, height = 5, units = "in",
                       res = 300)
      }
      cutoff = values$normals[,2] * 35.3147
      values$data$color <- ifelse(values$data$Q_cfs <= cutoff, '#0069b5', 'red')
      ggsave(path, plot = static_plot(values = values, selected = input$flow_selector),
             device = device)
    }
    
    if (input$plot_dygraph) {
      path <- paste(paste(values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], Sys.Date(), sep = '_'), "html", sep = ".")
      fs <- c(fs, path)
      graph = dygraph_plot(values = values, selected = input$flow_selector )
      htmlwidgets::saveWidget(graph, file = path)
    }
    
    ######### Downloadable Maps #########
    
    # Map
    if (input$maps_flow) {
      path <- paste0("flow_map_", Sys.Date(), ".html" )
      fs <- c(fs, path)
      graph = add_layers(map = basemap(), values = values)
      htmlwidgets::saveWidget(graph, file = path)
    }
    
    # High Flows Map
    if (input$maps_floods) {
      path <- paste0("flood_predections_", Sys.Date(), ".html" )
      fs <- c(fs, path)
      htmlwidgets::saveWidget(flood_map, file = path)
    }
    
    zip(zipfile=fname, files=fs)
  },
  contentType = "application/zip"
)