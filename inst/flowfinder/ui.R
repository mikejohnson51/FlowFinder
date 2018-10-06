library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)

#library(shinyjqui)


#### Reusable UI elements ###
download_item <- function(id, label) {
  ui_el = prettyCheckbox(
    inputId = id, 
    label = label, 
    icon = icon("check"), 
    shape = "round", 
    status = "primary"
  )
  return(ui_el)
}

shinyUI(
  tagList(
    useShinyjs(),
    div(class="bg",style="display:none",
        titlePanel(
          title="", windowTitle="FlowFinder"
        )
    ),
    navbarPage(title=div(img(src="logo.png", class="logo"),img(src="logo-small.png", class="logo-small")),
               collapsible = TRUE,
               id="nav",
               tabPanel("Map", icon = icon("map"),
                        # bsModal("filterModal", "NHD Flows", "filter", size = "large",
                        #         DT::DTOutput('nhd_table'),
                        #         bsButton(inputId = "filt", label = "Filter Map", icon = NULL, style = "primary"),
                        #         actionButton(inputId = "reset", label = "Reset Map", icon = NULL)),
                        div(class="outer",
                            tags$head(
                              includeCSS("www/styles.css"),
                              includeScript("www/global.js")
                            ),
                            # Get geolocation if possible
                            tags$script('
                                        $(document).ready(function () {
                                        navigator.geolocation.getCurrentPosition(onSuccess, onError);
                                        function onError (err) {
                                        Shiny.onInputChange("geolocation", false);
                                        }
                                        function onSuccess (position) {
                                        setTimeout(function () {
                                        document.getElementById("current_loc").style.color = "#5896e4";
                                        var coords = position.coords;
                                        Shiny.onInputChange("geolocation", true);
                                        Shiny.onInputChange("lat", coords.latitude);
                                        Shiny.onInputChange("long", coords.longitude);
                                        }, 1100)
                                        }
                                        });
                                        '),
                            # Get geolocation if possible
                            tags$script('
                                        $(document).ready(function(){
                                        $.getJSON("https://json.geoiplookup.io/", function(response) {
                                        Shiny.onInputChange("getIP", response);
                                        }, "json");
                                        });
                                        '),
                            tags$script('
                                        $(document).on("keyup", function(e) {
                                        if(e.keyCode == 13){
                                        Shiny.onInputChange("enterPressed", Math.random());
                                        }
                                        });
                                        '),
                            tags$script('
                                        $(document).on("keyup", function(e) {
                                        if(e.keyCode == 27){
                                        Shiny.onInputChange("escPressed", Math.random());
                                        }
                                        });
                                        '),
                            leafletOutput("map", width="100%", height="100%"),
                            absolutePanel(id = "controls", fixed = TRUE,
                                          top = 60, left = 10, right = "auto", bottom = "auto",
                                          width = 460, height = "auto",
                                          # textInput(inputId = 'place', label = NULL, value = "", placeholder = "Search FlowFinder"),
                                          searchInput(
                                            inputId = "place",
                                            label = NULL,
                                            placeholder = "Search FlowFinder",
                                            btnSearch = icon("search"),
                                            width = "85%"
                                          ),
                                          actionButton("slide", "", icon = icon("caret-left"))
                            ),
                            # absolutePanel(id = "settings", class = "panel panel-default", fixed = TRUE,
                            #               draggable = FALSE, bottom = "70px", right = "auto", left = "10px", top = "auto",
                            #               dropdownButton(inputId = "color_opts",
                            #                  circle = FALSE, icon = icon("tint"), label = NULL, width = "2px",
                            #                  right = FALSE,
                            #                  up = TRUE,
                            #                  h3("Hello"),
                            #                  radioButtons(inputId = "flow_display",
                            #                               label = "FLowlines",
                            #                               choiceNames = c("Default", "Positive", "Gradiant"),
                            #                               choiceValues = c("default", "positive", "gradiant")))
                            #                
                            # ),
                            # absolutePanel(id = "cur_l", class = "panel panel-default", fixed = TRUE,
                            #               draggable = FALSE, bottom = 60, right = 5, left = "auto", top = "auto",
                            #               actionButton("current_loc", "", icon = icon("location-arrow"))
                            # ),
                            absolutePanel(id = "reset_buttons", fixed = TRUE,
                                          draggable = FALSE, top = 60, right = 10, left = "auto", bottom = "auto",
                                          actionButton("reset", "", icon = icon("undo"))
                            )
                            )
                            ),
               tabPanel("Data", icon = icon("line-chart"), value = "data",
                        fluidRow(
                          column(5, 
                                 column(12, pickerInput(inputId = "flow_selector", 
                                                        choices = "", 
                                                        multiple = TRUE, 
                                                        label = NULL,
                                                        options = list(
                                                          `max-options`= 10,
                                                          `selected-text-format` = "count",
                                                          `count-selected-text` = "{0} reaches selected",
                                                          `actions-box` = TRUE
                                                        )
                                 )                                  )
                          ),
                          column(6,
                                 actionButton("prevCOMID", label = "Previous"),
                                 actionButton("nextCOMID", label = "Next"),
                                 actionButton("mark_flowline", "View on Map"),
                                 dropdownButton(inputId = "download_btn",
                                                circle = FALSE, icon = icon("download"), label = "Downloads", width = "300px",
                                                down = TRUE, right = TRUE,
                                                
                                                tags$h3("Data"),
                                                download_item(id = "data_csv", label = "CSV"),
                                                download_item(id = "data_nhd", label = "NHD.shp"),
                                                download_item(id = "data_rda", label = "RDA"),
                                                
                                                tags$h3("Plots"),
                                                download_item(id = "plot_png", label = "Flow Graph (PNG)"),
                                                download_item(id = "plot_dygraph", label = "Flow Dygraph (HTML)"),
                                                
                                                tags$h3("Maps"),
                                                download_item(id = "maps_floods", label = "High Flows (HTML)"),
                                                download_item(id = "maps_flow", label = "Map (HTML)"),
                                                downloadButton(outputId = 'downloadData', label = NULL, class = 'btn-primary')
                                 )
                          )
                        ),
                        br(), br(),
                        dygraphs::dygraphOutput("dygraph") %>% shinycssloaders::withSpinner(type = 7, color.background = "#FFFFFF"),
                        # Keep as easy way to vizualize downloadable ggplot
                        #plotOutput("plot2"),
                        br(), br(),
                        fluidRow(
                          column(6,DT::DTOutput('tbl_up')),
                          column(6,DT::DTOutput('tbl_down'))
                        ),
                        DT::DTOutput('tbl')
               ),
               tabPanel("High Flows", icon = icon("tint"), value = "high_flows",
                        div(class="outer dark_bg",
                            leafletOutput("flood_map", height = "100%"),
                            div(class = "fl_map_div",
                                dygraphs::dygraphOutput("flood_dygraph", height = "200px")
                            ),
                            actionButton("close_fl_gr", "", icon = icon("times"))
                        )
               ),
               tabPanel("Info", icon = icon("info-circle"),
                        textOutput("data_loc"),   
                        tableOutput("stations"),
                        fluidRow(
                          column(5, tableOutput("Flowlines")),
                          column(5, tableOutput("meta"))
                        )
               ),
               tabPanel("Filter", icon = icon("filter"), value = "filter",
                        dashboardPage(
                          dashboardHeader(disable = TRUE),
                          dashboardSidebar(disable = TRUE),
                          dashboardBody(
                            fluidRow(
                              bsModal("modalExample", "Information", "info_modal_button", size = "large",
                                      radioGroupButtons(
                                        inputId = "chart_type",
                                        label = NULL,
                                        justified = TRUE,
                                        choices = c("Static", "Dynamic"),
                                        status = "primary"
                                      ),
                                      htmlOutput("display_info")),
                              column(width = 3,
                                     box(width = NULL, status = "primary",
                                         tags$div(class = "float-left", tags$h4("Display Settings")),
                                         tags$div(class = "float-left", actionButton("info_modal_button", "", icon("info-circle"))),
                                         tags$br(), tags$br(),
                                         radioGroupButtons(
                                           inputId = "display_type",
                                           label = NULL,
                                           justified = TRUE,
                                           choices = c("Static", "Dynamic"),
                                           status = "primary"
                                         ),
                                         tags$br(),
                                         uiOutput("displayOptions")
                                         # tags$div(class = "float-left2", radioButtons(inputId = "flow_display",
                                         #              label = NULL,
                                         #              # direction = "vertical",
                                         #              # status = "primary",
                                         #              choices = list(
                                         #                "Default" = "default",
                                         #                "Positive" = "positive",
                                         #                "Month Average" = "month_avg",
                                         #                "Average" = "mean",
                                         #                "Average Comparison" = "mean_dif",
                                         #                "Peak Flow" = "max_value",
                                         #                "Range" = "range",
                                         #                "Change" = "deriv",
                                         #                "Time Series" = "time_series"
                                         #              ),
                                         #              selected = character(0)
                                         #              # choiceNames = c("Default", "Positive", "Month Average", "Average", "Average Comparison", "Peak Flow", "Range", "Change", "Time Series"),
                                         #              # choiceValues = c("default", "positive", "month_avg" ,"mean", "mean_dif" ,"max_value", "range", "deriv", "time_series")
                                         #              )),
                                         # uiOutput("time_options")
                                     )),
                              column(width = 9,
                                     box(width = NULL,
                                                leafletOutput("map_filter", height = 500) %>% shinycssloaders::withSpinner(type = 7, color.background = "#FFFFFF"),
                                                uiOutput("map_time")
                                            ),
                                            tags$div(id = "table_box",
                                                     tabBox(width = "100%",
                                                            title = NULL,
                                                            # The id lets us use input$tabset1 on the server to find the current tab
                                                            id = "tabset1",
                                                            tabPanel("NHD Flowlines",
                                                                     fluidRow(uiOutput("nhd_options")),
                                                                     DT::DTOutput('nhd_table')
                                                                     )
                                                            ))
                                            
                                            
                                     )
                              )
                            )
                          
                        )
               )
  )))
