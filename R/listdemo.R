#'@title List the demo and select one to run
#'
#'@description This function shows a list of shiny app demos for user to choose one to run.
#'
#'@return The app will start in a web browser
#'
#'@examples
#'\dontrun{
#'listdemo()
#'}

listdemo <- function() {
  demo_list <- list.dirs(system.file("apps", 
                package="lstatdemo"), full.names=FALSE, 
                recursive=FALSE)
  demo_choice <- menu(demo_list)
  shiny::runApp(system.file(paste("apps/", demo_list[demo_choice], 
                sep=""), package="lstatdemo"))
}
