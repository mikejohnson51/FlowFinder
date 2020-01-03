continuous_colors <- function(num) {
  colors = list(
    c('#004a75'),
    c('#004a75', '#ffa600'),
    c('#004a75', '#cc5091', '#ffa600'),
    c('#004a75', '#8f539c', '#f75972', '#ffa600'),
    c('#004a75', '#6e549a', '#cc5091', '#ff665e', '#ffa600'),
    c('#004a75', '#5a5396', '#a9529a', '#e95380', '#ff7151', '#ffa600'),
    c('#004a75', '#4d5392', '#8f539c', '#cc5091', '#f75972', '#ff7947', '#ffa600')
  )
  return (colors[[num]])
}

divergent_colors <- function(num) {
  colors = list(
    c('#004a75'),
    c('#004a75', '#ffa600'),
    c('#004a75', '#c6c6c6', '#ffa600'),
    c('#004a75', '#72859d', '#ebb677', '#ffa600'),
    c('#004a75', '#72859d', '#c6c6c6', '#ebb677', '#ffa600'),
    c('#004a75', '#557190', '#8e9aab', '#e1bb91', '#f3b15a', '#ffa600'),
    c('#004a75', '#557190', '#8e9aab', '#c6c6c6', '#e1bb91', '#f3b15a', '#ffa600')
  )
  return (colors[[num]])
}

create_palette <- function(vals = NULL, continuous = TRUE) {
  
  if (continuous) {
    diff <- max(vals, na.rm = T) - min(vals, na.rm = T)
    bins <- as.double(0:7 %>% purrr::map(function(x) ceiling(x * diff/7)))
  }
  else {
    bins <- as.double(seq(-3.5,3.5,1) %>% 
                        purrr::map(function(x) ifelse(x > 0, ceiling(x * vals/3.5), floor(x * vals/3.5))))
  }
  
  bins = unique(bins)
  num_bins = length(bins)
  
  if (num_bins == 1) {
    bins[2] <- bins[1] * 2 +1
    num_bins = num_bins + 1
  }
  
  if (continuous) { bin_colors <- continuous_colors(num_bins-1) }
  else { bin_colors <- divergent_colors(num_bins-1)  }
  
  pal <- colorBin(bin_colors, bins = bins, domain = NULL)
  return(list(pal = pal, bins = bins))
}

positive_pal <- colorBin(divergent_colors(2), bins = c(-1, 0.000000000000000001, 9000000000000000000000))


add_legend <- function(pal = NULL, colors = NULL, labels = NULL, title = NULL) {
  if(!is.null(pal)) {
    addLegend(map = leafletProxy("map_filter"),
              position = 'topleft',
              layerId = 'legend',
              pal = pal$pal,
              values = pal$bins,
              opacity = 1,
              title = "Q_cfs")
  }
  else {
    addLegend(map = leafletProxy("map_filter"),
              position = 'topleft',
              layerId = 'legend',
              colors = colors,
              labels = labels,
              opacity = 1,
              title = title)
  }
  
  # shinyjs::show("legend_switch")
}

base_table <- function(data) {
  table = DT::datatable(data, 
                        extensions = 'ColReorder',
                        escape = FALSE, 
                        rownames = FALSE, 
                        filter = 'top',
                        options = list(colReorder = TRUE,
                                       scrollX = TRUE))
  return(table)
}

generate_options <- function(href, inputID, choices, selected) {
  ui_element <- list(h4(tags$a(icon("caret-right"), 
                               class = "table_opts", 
                               href = paste0("#",href), 
                               `data-toggle`="collapse", 
                               "Options"
  )),
  tags$div(class="collapse", id = href,
           tags$div(align = 'left', class = 'multicol', 
                    checkboxGroupInput(inputId  = inputID, 
                                       label    = NULL, 
                                       choices  = choices,
                                       selected = selected,
                                       inline   = FALSE))))
  return(ui_element)
}