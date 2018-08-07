load('data/current_nc/flood_map.rda')

# Generate Dygraph
dygraph_flood <- function(comid, data) {
  
  # Get data for new comid
  subset = subset_nomads(comids = comid)
  
  # First time creating a data frame
  if (is.null(data)){
    df <- data.frame(subset$Q_cfs)
    colnames(df) <- comid
    df = xts::xts(df, order.by = subset$dateTime)
  } else {
    df = data
    prev_names = colnames(df)
    df$new_id <- subset$Q_cfs
    colnames(df) <- c(prev_names, comid)
  }
  
  graph = dygraphs::dygraph(df) %>%
    dyOptions(drawPoints = TRUE, 
              pointSize = 2,
              gridLineColor = "lightblue") %>% 
    dyRangeSelector(height = 20) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 1) %>%
    dyAxis("y", label = "Streamflow (cfs)" )%>%
    dyLegend(show = "onmouseover")
  
  if (ncol(df) == 1) {
    cutoff = norm[norm$COMID == comid,][[2]]
    mn = mean(df[[2]], na.rm = TRUE)
    std = sd(df[[2]], na.rm = TRUE)
    
    graph = graph %>%
      dyOptions(
        drawPoints = TRUE, 
        pointSize = 2,
        gridLineColor = "lightblue",
        fillGraph = TRUE, 
        fillAlpha = 0.1
      )  %>%
      dyLimit(cutoff, 
              strokePattern = "solid", 
              color = "red", 
              label = paste0("Monthly Average (", round(cutoff,2), " cfs)")) %>%
      dyShading(from = mn - std, 
                to = mn + std, 
                axis = "y")
  }
  
  return(list(graph = graph,
              data_set = df))
}