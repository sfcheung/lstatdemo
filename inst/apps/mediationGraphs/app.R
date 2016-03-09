# Demonstrate mediation

library(grid)

# Global variables

xmin <- 0
xmax <- 10
x_init <- 5
xh <- x_init

# UI
ui <- fluidPage(
  titlePanel("Mediation: Illustration"),
  fluidRow(
    column(12,
      wellPanel(
        h4("This page illustrates, in a simple mediation model, ",
            "how the mediator (M) and dependent",
            "variable (Y) change as independent variable (X) changes. ",
            "Try to change the value of X and see how what happens. ",
            "You can also change the three effects (paths). For example, ",
            "you can see what if X also has a positive direct effect on Y.")
        ),
      fluidRow(
        column(4,
          wellPanel(
            #h4("Settings"),
            sliderInput('xi',
              label=h5("X"),
              min=xmin, max=xmax, value=x_init, step=.5,
              ticks=TRUE),
            sliderInput('bmx',
              label=h5("Effect: X on M (a path)"),
              min=-2, max=2, value=1, step=.1,
              ticks=TRUE),
            sliderInput('bym',
              label=h5("Effect: M on Y (b path)"),
              min=-2, max=2, value=.5, step=.1,
              ticks=TRUE),
            sliderInput('byx',
              label=h5("Direct Effect: X on Y (c path)"),
              min=-2, max=2, value=0, step=.1,
              ticks=TRUE),
            h4("Miscellaneous"),
            p("The following options are non-essential. They are",
              "included here for completeness."),
            sliderInput('bmx0',
              label=h5("Intercept: X on M"),
              min=-2, max=2, value=0, step=.1,
              ticks=TRUE),
            sliderInput('bym0',
              label=h5("Intercept: M on Y"),
              min=-2, max=2, value=0, step=.1,
              ticks=TRUE)
            #h5("Technical details: [To be added]"),
            #paste("[Technical details: (To be added)]", sep="")
            )
          ),
        column(8,
          plotOutput('plot'),
          br(),
          plotOutput('plot2')
          )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("The latest version of the code can be found at ",
          a("lstatdemo at GitHub", 
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
    byx <- input$byx
    bmx0 <- input$bmx0
    bym0 <- input$bym0
    lm_m <- function(x) {bmx0 + bmx*x}
    lm_y <- function(x, m) {bym0 + bym*m + byx*x}
    mmin0 <- lm_m(xmin)
    mmax0 <- lm_m(xmax)
    ymin0 <- lm_y(xmin, mmin0)
    ymax0 <- lm_y(xmax, mmax0)
    m2 <- sort(c(mmin0, mmax0))
    mmin <- (m2[1]); mmax <- (m2[2])
    y2 <- sort(c(ymin0, ymax0))
    ymin <- (y2[1]); ymax <- (y2[2])
    # Previous values
    mh <- lm_m(xh)
    yh <- lm_y(xh, mh)
    # Current valules
    xi <- input$xi
    mi <- lm_m(xi)
    yi <- lm_y(xi, mi)
    # Changes
    x_change <- xi - xh
    m_change <- mi - mh
    y_change <- yi - yh
    # Plot the graphs
    par(mfrow=c(1, 2))
    #layout(matrix(c(1, 2, 1, 2, 3, 3), 3, 2, byrow=TRUE))
    arrow_len <- .10
    plot(x=NULL, y=NULL, xlim=c(xmin, xmax), ylim=c(mmin, mmax), 
          type="n", xlab="X", ylab="M", asp=1, 
          main=paste("X on M (a path)", "\n",
                     "Change From X: ", 
                     (xi - xh)*bmx, "=", (xi - xh), "*", bmx, sep=""),
          sub=paste("Changes: X: ", x_change, " / ", 
                    "M: ", m_change, sep=""))
    parusr <- par("usr")
    abline(bmx0, bmx, lwd=2, col="blue")
    suppressWarnings(arrows(xi, parusr[3], xi, mi, length=arrow_len, 
                      col="black", lwd=2, lty="dotted"))
    suppressWarnings(arrows(xi, mi, parusr[1], mi, length=arrow_len, 
                      col="blue", lwd=2, lty="dotted"))
    suppressWarnings(arrows(xh, parusr[3], xh, mh, length=arrow_len, 
                      col="black", lty="dotted"))
    suppressWarnings(arrows(xh, mh, parusr[1], mh, length=arrow_len, 
                      col="blue", lty="dotted"))
    suppressWarnings(arrows(xh, mh, xi, mi, length=arrow_len*2, 
                      col="black", lwd=6, lty="dotted"))
    
    plot(x=NULL, y=NULL, xlim=c(mmin, mmax), ylim=c(ymin, ymax), 
          type="n", xlab="M", ylab="Y", asp=1, 
          main=paste("M on Y (b path)", "\n",
                    "Change Directly from X:", 
                    (xi - xh)*byx, "=", (xi - xh), "*", byx, "\n",
                    "Change From M: ", 
                    (mi - mh)*bym, "=", (mi - mh), "*", bym, sep=""),
          sub=paste("Changes: M: ", m_change, " / ",
                    "Y: ", y_change, sep=""))
    parusr <- par("usr")
    abline(bym0 + byx*xi, bym, lwd=2, col="red")
    suppressWarnings(arrows(mi, parusr[3], mi, yi, length=arrow_len, 
                      col="blue", lwd=2, lty="dotted"))
    suppressWarnings(arrows(mi, yi, parusr[1], yi, length=arrow_len, 
                      col="red", lwd=2, lty="dotted"))
    suppressWarnings(arrows(mh, parusr[3], mh, yh, length=arrow_len, 
                      col="blue", lty="dotted"))
    suppressWarnings(arrows(mh, yh, parusr[1], yh, length=arrow_len, 
                      col="red", lty="dotted"))
    suppressWarnings(arrows(mh, yh, mh, lm_y(xi, mh), 
                      length=arrow_len*2, col="black", 
                      lwd=6, lty="dotted"))
    suppressWarnings(arrows(mh, lm_y(xi, mh), mi, yi, 
                      length=arrow_len*2, col="blue", 
                      lwd=6, lty="dotted"))
    xh <<- xi
    })
  output$plot2 <- renderPlot({
    bmx <- input$bmx
    bym <- input$bym
    byx <- input$byx
    bmx0 <- input$bmx0
    bym0 <- input$bym0
    lm_m <- function(x) {bmx0 + bmx*x}
    lm_y <- function(x, m) {bym0 + bym*m + byx*x}
    xi <- input$xi
    mi <- lm_m(xi)
    yi <- lm_y(xi, mi)
    pushViewport(viewport())
    var_fs <- gpar(fontsize=20)
    grid.roundrect(x=.15, y=.25, width=.20, height=.10)
    grid.text(paste("X (= ", xi, ")", sep=""), 
              x=.15, y=.25, gp=var_fs)
    grid.roundrect(x=.50, y=.75, width=.20, height=.10)
    grid.text(paste("M (= ", mi, ")", sep=""), 
              x=.50, y=.75, gp=var_fs)
    grid.roundrect(x=.85, y=.25, width=.20, height=.10)
    grid.text(paste("Y (= ", yi, ")", sep=""), 
              x=.85, y=.25, gp=var_fs)
    dy <- .05
    dx <- .10
    arrowi <- arrow(angle=15)
    grid.lines(x=c(.15, .50 - dx/2), y=c(.25 + dy, .75 - dy), 
               arrow=arrowi)
    grid.lines(x=c(.50 + dx/2, .85), y=c(.75 - dy, .25 + dy), 
               arrow=arrowi)
    grid.lines(x=c(.15 + dx, .85 - dx), y=c(.25, .25), 
               arrow=arrowi)
    path_fs <- gpar(fontsize=18)
    grid.text(paste("a = ", bmx, sep=""), 
              x=(.15 + .50)/2, y=(.25 + .75)/2 + dy*1.5, 
              gp=gpar(fontsize=18, col="blue"))
    grid.text(paste("b = ", bym, sep=""), 
              x=(.50 + .85)/2, y=(.75 + .25)/2 + dy*1.5, 
              gp=gpar(fontsize=18, col="red"))
    grid.text(paste("c = ", byx, sep=""), 
              x=(.15 + .85)/2, y=(.25 + .25)/2 + dy, 
              gp=gpar(fontsize=18, col="black"))
    popViewport()
  })  
  }

shinyApp(ui=ui, server=server)
