# [Brief description]
# A template for sidebar panel layout with 
# one description panel below the title,
# and one bottom panel at the end.
# To run in R: runGitHub("statDemos","sfcheung",subdir="[folder name]")

# Global variables

# UI
ui <- fluidPage(
  titlePanel("Title Panel"),
  fluidRow(
    column(12,
      wellPanel(
        h4("Description Panel")
        ),
      fluidRow(
        column(4,
          wellPanel(
            h4("Sibebar Panel"),
            br(),
            sliderInput('sliderName',
              label=h5("[Slide title]"),
              min=-1, max=1, value=0, step=.5,
              ticks=TRUE),
            br(),
            h5("Technical details:"),
            paste("[Technical details]", sep="")
            )
          ),
        column(8,
          plotOutput('plot')
          )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("The latest version of the code can be found at ",
          a("statDemos at GitHub", 
            href="https://github.com/sfcheung/statDemos/...."),
          "."
          ),
        p("The whole repository can be downloaded from GitHub and run in R by",
          code("runGitHub(\"statDemos\",\"sfcheung\",subdir=\"....\")")
          )
        )
      )
    )
  )

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    r <- input$sliderName
    cexAll <- 1.5
    # Don't know why cex cannot control the magnification of all elements
    # So used cexAll here
    # Generate the plot object
    plot(r)
    })
  }

shinyApp(ui=ui, server=server)
