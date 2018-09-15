add_legend <- function(pal = NULL, colors = NULL, labels = NULL, title = NULL) {
  if(!is.null(pal)) {
    addLegend(map = leafletProxy("map2"),
              position = 'topleft',
              layerId = 'legend',
              pal = pal$pal,
              values = pal$bins,
              opacity = 1,
              title = "Q_cfs")
  }
  else {
    addLegend(map = leafletProxy("map2"),
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