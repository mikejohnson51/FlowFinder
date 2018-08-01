library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)


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
                            #Enter button activates search, only on focus
                            tags$script('
                                        document.addEventListener("keypress", function(event) {
                                        if (event.keyCode === 13 || event.which === 13) {
                                        var dummyEl = document.getElementById("place");
                                        var isFocused = (document.activeElement === dummyEl);
                                        if (isFocused) {
                                        document.getElementById("do").click();
                                        }
                                        }
                                        });
                                        '),
                            leafletOutput("map", width="100%", height="100%"),
                            verbatimTextOutput(outputId = "server_problems"),
                            absolutePanel(style="display:inline-block", id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = 10, right = "auto", bottom = "auto",
                                          width = 430, height = "auto",
                                          textInput(inputId = 'place', label = NULL, value = "", placeholder = "Search FlowFinder"),
                                          actionButton("do", "", icon("search")),
                                          actionButton("slide", "", icon = icon("caret-left"))
                            ),
                            absolutePanel(id = "cur_l", class = "panel panel-default", fixed = TRUE,
                                          draggable = FALSE, bottom = 60, right = 5, left = "auto", top = "auto",
                                          actionButton("current_loc", "", icon = icon("location-arrow"))
                            ),
                            absolutePanel(id = "reset_buttons", fixed = TRUE,
                                          draggable = FALSE, top = 60, right = 10, left = "auto", bottom = "auto",
                                          actionButton("reset", "", icon = icon("undo"))
                            )
                            )
                            ),
               tabPanel("Data", icon = icon("line-chart"),
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
                                            )
                                  )
                          ),
                          column(6,
                                 actionButton("prevCOMID", label = "Previous"),
                                 actionButton("nextCOMID", label = "Next"),
                                 actionButton("mark_flowline", "View on Map"),
                                 dropdownButton(
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
                        #plotOutput("fFlow"),
                        br(), br(),
                        dygraphs::dygraphOutput("dygraph"),
                        br(), br(),
                        fluidRow(
                          column(6,DT::DTOutput('tbl_up')),
                          column(6,DT::DTOutput('tbl_down'))
                        ),
                        DT::DTOutput('tbl')
               ),
               tabPanel("High Flows", icon = icon("tint"),
                        div(class="outer",
                            leafletOutput("flood_map", width="100%", height="100%")
                        )
               ),
               tabPanel("Info", icon = icon("info-circle"),
                        textOutput("data_loc"),   
                        tableOutput("stations"),
                        fluidRow(
                          column(5, tableOutput("Flowlines")),
                          column(5, tableOutput("meta"))
                        )
               )
    )))