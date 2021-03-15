# Generate static ggplot graph
static_plot <- function(values, selected, id, normals) {
  
  #color <- ifelse(values$data$Q_cfs <= cutoff, '#0069b5', 'red')
  
  time = values$data %>% 
    filter(COMID == id)
  
  df = data.frame(time = time$dateTime)
  
  for (stream in selected) {
    text = stream
    stream_id = getIDs(text)[1]
    data = values$data %>% 
      filter(COMID == stream_id)
    df[as.character(stream_id)] = data$Q_cfs
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
    cutoff <- normals %>% 
      filter(COMID == id)
    cutoff = cutoff[,2] * 35.3147
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
stream_table <- function(data = NULL, direction = NULL, current_id = NULL, session = NULL) {
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
    df <- rbind(df, paste0("No ", direction, " reaches from COMID ", current_id))
    colnames(df) = ifelse(direction == "upstream", "Upstream", "Downstream")
    table = DT::datatable(df, options = list(dom = 't'), escape = FALSE, selection = 'none')
  }
  return(table)
}
