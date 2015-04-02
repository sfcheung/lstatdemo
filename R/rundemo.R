#'@title Run a demo
#'
#'@description This function run a shiny app demo in this package
#'
#'@param demoname The name of the demo
#'
#'@return The app will start in a web browser
#'
#'@examples
#'\dontrun{
#'rundemo("ANOVAResidual")
#'}

rundemo <- function(demoname) {
  shiny::runApp(system.file(demoname, package="lstatdemo"))
}
