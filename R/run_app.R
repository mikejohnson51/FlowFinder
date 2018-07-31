#' @export

run_app <- function()
{
  shiny::runApp(appDir = system.file("flowfinder", package = "FlowFinder"))
}