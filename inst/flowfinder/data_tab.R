# Set initial COMID choices - used in drop down selector
set_choices <- function(values) {
  
  names_ids <- values$flow_data$nhd@data %>% 
    select(comid, gnis_name) %>% 
    mutate(name = paste0(
      paste0(ifelse(is.na(gnis_name), "", gnis_name)), 
      paste0(" COMID: ", comid))) %>% 
    select(-gnis_name)
  
  max = values$nwm %>% 
    top_n(1, Q_cfs) %>% 
    .$COMID
  
  default = names_ids %>% 
    filter(comid == max) %>% 
    .$name
  
  positive = values$nwm %>%
    filter(Q_cfs > 0) %>% 
    select(COMID) %>% 
    distinct() %>% 
    .$COMID
  
  non_zeros = purrr::map_lgl(getIDs(names_ids$name), ~(. %in% positive))
  
  return(list(default = default, choices = names_ids$name, non_zeros = non_zeros))
}

# Generate Dygraph
dygraph_plot <- function(values, selected) {
  
  output <- matrix(ncol=length(selected), nrow=length(values$data$dateTime))
  
  ids = c()
  for (j in 1:length(selected)) {
    text = selected[j]
    id = getIDs(text)[1]
    
    data = values$nwm %>% 
      filter(COMID == id)

    output[,j] <- data$Q_cfs
    ids = c(ids, as.character(id))
  }
  
  df <- data.frame(output)
  colnames(df) <- ids

  timeZone = lutz::tz_lookup_coords(values$loc$lat, values$loc$lon, method = "accurate")
  df = xts::xts(df, order.by = lubridate::with_tz(data$dateTime, timeZone), tz = timeZone)

  #title = ifelse((selected) == 1,lengthpaste0(ifelse(is.na(values$flow_data$nhd$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]]), "", paste0(values$flow_data$nhd@data$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], " ")),
  #              paste0("COMID: ", values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]])), "Multiple Reaches Selected")
  graph = dygraphs::dygraph(df) %>%
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
  
  if (length(selected) == 1) {
    cutoff = values$normals[,2] * 35.3147
    mn = mean(df[[2]], na.rm = TRUE)
    std = sd(df[[2]], na.rm = TRUE)
    graph = graph %>%
      dyOptions(
        useDataTimezone = TRUE,
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
  
  return(graph)
}


# Generate Dygraph
static_plot <- function(values, selected) {
  
  #color <- ifelse(values$data$Q_cfs <= cutoff, '#0069b5', 'red')
  
  df = data.frame(time = values$data$dateTime)
  
  for (stream in selected) {
    text = stream
    
    data = values$nwm %>% 
      filter(COMID == getIDs(text)[1])

    df[as.character(id)] = data$Q_cfs
  }

    df.long = reshape2::melt(df, id="time")
  colnames(df.long) <- c("time", "COMID", "value")

  graph = ggplot(data = df.long, aes(time, value, colour = COMID)) +
    theme_bw() + 
    theme(axis.title.x = element_text(margin = unit(c(6, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 6, 0, 0), "mm"))) +
    labs(x = "Date and Time",
         y = "Streamflow (cfs)") +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .05)))
  
 # title = paste0(ifelse(is.na(values$flow_data$nhd$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]]), "", paste0(values$flow_data$nhd@data$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], " ")),
                 #                     paste0("COMID: ", values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]])))
  
  if (length(selected) > 1) {
    graph = graph +
      geom_line() +
      geom_point()
  }

  else{
    cutoff = values$normals[,2] * 35.3147
    mn = mean(df[[2]], na.rm = TRUE)
    std = sd(df[[2]], na.rm = TRUE)
  
    graph = graph +
      geom_rect(aes(ymin=mn - std, ymax=mn + std, xmin=df$time[1], xmax=df$time[length(df$time)]),fill = "#ededed", size = 0, show.legend = FALSE, alpha = .1) +
      geom_hline(aes(yintercept = cutoff), colour = "red", show.legend = FALSE) +
      geom_text(aes(df$time[2],cutoff,label = paste0("Monthly Average (", round(cutoff,2), " cfs)"), vjust = -1), show.legend = FALSE) +
      geom_area(aes(fill="pos"), fill = '#0069b5', size = 0, alpha = .2, show.legend = FALSE) +
      geom_line(color="#0069b5", show.legend = TRUE) +
      geom_point(color = "#0069b5", show.legend = TRUE)
  }

  return(graph)
}




# Generate upstream and downstream tables
stream_table <- function(data = NULL, direction = NULL, values= NULL, session = NULL) {
  if (length(data) > 0) {
    df <- data %>%
      dplyr::mutate(View = paste('<a class="go-stream" href="" data-stream="', data[[1]], '"><i class="fa fa-eye"></i></a>', sep=""))
    
    all = data.frame(paste0("All ", "(",nrow(df), ")"), paste('<a class="go-stream" href="" data-stream="', paste(data[[1]],collapse=","), '"><i class="fa fa-eye"></i></a>', sep=""))
    df = rbind(setNames(all, names(df)), df)
    action <- DT::dataTableAjax(session, df, rownames = FALSE)
    table = DT::datatable(df,
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
    table = DT::datatable(df, options = list(dom = 't'), escape = FALSE, selection = 'none')
  }
  return(table)
}

# Find up/downstreams from current id
find_connecting_streams <- function(values = values) {
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
  return(list(upstream = upstream, downstream = downstream))
}