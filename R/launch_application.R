#' @export

launch_application <- function()
{
  shiny::runApp(appDir = system.file("flowlinefinder", package = "FlowlineFinder"))
}