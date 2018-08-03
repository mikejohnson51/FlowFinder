# Set initial COMID choices - used in drop down selector
set_choices <- function(values) {
  
  # Find comid that has max flow in forcast - used to set default
  max_qcms = values$nwm[match(max(values$nwm$Q_cfs), values$nwm$Q_cfs),]$COMID
  name = values$flow_data$nhd[values$flow_data$nhd$comid == max_qcms,]$gnis_name
  text = paste0(paste0(ifelse(is.na(name), "", name)), paste0(" COMID: ", max_qcms))
  
  choices = as.list(paste0(paste0(ifelse(is.na(values$flow_data$nhd@data$gnis_name), "", values$flow_data$nhd@data$gnis_name)),
                           paste0(" COMID: ", values$flow_data$nhd$comid)))
  
  # Find flows with at least one forcasted point > 0
  non_zero = unique(values$nwm[values$nwm$Q_cfs > 0,]$COMID)
  non_zeros = c()
  for (stream in choices) {
    id = unlist(strsplit(stream, split='COMID: ', fixed=TRUE))[2]
    # Create vec of T/F values - used to determine style of text in selector
    non_zeros = c(non_zeros, id %in% non_zero)
  }
  
  return(list(default = text, choices = choices, non_zeros = non_zeros))
}

# Generate Dygraph
dygraph_plot <- function(values, selected) {
  
    df = data.frame(time = values$data$dateTime)
    
    for (stream in selected) {
      text = stream
      id = unlist(strsplit(text, split='COMID: ', fixed=TRUE))[2]
      i = match(id, values$flow_data$nhd@data$comid)
      data = values$nwm[values$nwm$COMID == values$flow_data$nhd$comid[i],]
      df[as.character(id)] = data$Q_cfs
    }
    rownames(df) = df[[1]]
    #title = ifelse((selected) == 1,lengthpaste0(ifelse(is.na(values$flow_data$nhd$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]]), "", paste0(values$flow_data$nhd@data$gnis_name[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]], " ")),
    #              paste0("COMID: ", values$flow_data$nhd$comid[values$flow_data$nhd$comid == values$flow_data$nhd$comid[values$i]])), "Multiple Reaches Selected")
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
    
    if (length(selected) == 1) {
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