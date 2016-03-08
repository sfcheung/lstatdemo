# [Brief description]
# A template for sidebar panel layout with 
# one description panel below the title,
# and one bottom panel at the end.
# To run in R: runGitHub("lstatdemo","sfcheung",subdir="mediationGraphs")

# Global variables

xmin <- 0
xmax <- 10

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
            sliderInput('bmx',
              label=h5("Effect: X on M"),
              min=-1, max=1, value=.5, step=.1,
              ticks=TRUE),
            br(),
            sliderInput('bym',
              label=h5("Effect: M on Y"),
              min=-1, max=1, value=.5, step=.1,
              ticks=TRUE),
            br(),
            sliderInput('xi',
              label=h5("X"),
              min=xmin, max=xmax, value=5, step=.5,
              ticks=TRUE),
            br(),
            sliderInput('bmx0',
              label=h5("Intercept: X on M"),
              min=-1, max=1, value=0, step=.1,
              ticks=TRUE),
            br(),
            sliderInput('bym0',
              label=h5("Intercept: M on Y"),
              min=-1, max=1, value=0, step=.1,
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
            href="https://github.com/sfcheung/lstatdemo/mediationGraphs"),
          "."
          ),
        p("The whole repository can be downloaded from GitHub and run in R by",
          code("runGitHub(\"lstatdemo\",\"sfcheung\",subdir=\"mediationGraphs\")")
          )
        )
      )
    )
  )

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    bmx <- input$bmx
    bym <- input$bym
    bmx0 <- input$bmx0
    bym0 <- input$bym0
    mmin0 <- bmx0 + bmx*xmin
    mmax0 <- bmx0 + bmx*xmax
    ymin0 <- bym0 + bym*mmin0
    ymax0 <- bym0 + bym*mmax0
    m2 <- sort(c(mmin0, mmax0))
    #mmin <- floor(m2[1]); mmax <- ceiling(m2[2])
    mmin <- (m2[1]); mmax <- (m2[2])
    y2 <- sort(c(ymin0, ymax0))
    #ymin <- floor(y2[1]); ymax <- ceiling(y2[2])
    ymin <- (y2[1]); ymax <- (y2[2])
    xi <- input$xi
    mi <- bmx0 + bmx*xi
    yi <- bym0 + bym*mi
    par(mfrow=c(1, 2))
    plot(x=NULL, y=NULL, xlim=c(xmin, xmax), ylim=c(mmin, mmax), 
          type="n", xlab="X", ylab="M", asp=1)
    abline(bmx0, bmx, lwd=2, col="blue")
    arrows(xi, mmin, xi, mi)
    arrows(xi, mi, xmin, mi)
    plot(x=NULL, y=NULL, xlim=c(mmin, mmax), ylim=c(ymin, ymax), 
          type="n", xlab="M", ylab="Y", asp=1)
    abline(bym0, bym, lwd=2, col="red")
    arrows(mi, ymin, mi, yi)
    arrows(mi, yi, mmin, yi)
    })
  }

shinyApp(ui=ui, server=server)
