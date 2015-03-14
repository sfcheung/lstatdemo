#'@title Run a demo
#'
#'@description This function run a shiny app demo in this package
#'
#'@param demo The name of the demo
#'
#'@return The app will start in a web browser
#'
#'@examples
#'
#'rundemo("ANOVAResidual")
#'

rundemo <- function(demoname) {
  shiny::runApp(system.file(demoname, package="lstatdemo"))
}
