#' @export

run_app <- function()
{
  shiny::runApp(appDir = system.file("flowlinefinder", package = "FlowlineFinder"))
}