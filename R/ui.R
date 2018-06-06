library(shiny)
library(leaflet)
library(shinythemes)

shinyUI(navbarPage("Flowline Finder", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        includeCSS("www/styles.css"),
        includeScript("www/gomap.js")
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
                console.log(coords);
                Shiny.onInputChange("geolocation", true);
                Shiny.onInputChange("lat", coords.latitude);
                Shiny.onInputChange("long", coords.longitude);
            }, 1100)
         }
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
        textInput(inputId = 'place', label = NULL, ""),
        actionButton("do", "", icon("search"))
      ),
      absolutePanel(id = "cur_l", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, bottom = 100, right = 10, left = "auto", top = "auto",
                    actionButton("current_loc", "", icon = icon("location-arrow"))
      )
    )
  ),
  tabPanel("Information",
    verbatimTextOutput("data_loc"),   
    tableOutput("stations"),
    tableOutput("Flowlines")
  ),
  tabPanel("Stream Flow",
           textOutput("stream"),
           selectInput(inputId = "flow_selector", label = ,"", choices = ""),
           actionButton("mark_flowline", "View on Map"),
           plotOutput("streamFlow"),
           downloadButton('downloadData', 'Download', icon = icon("download"))
  )
  
))
