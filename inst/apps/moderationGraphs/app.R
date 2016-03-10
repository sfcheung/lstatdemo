# Demonstrate moderation

library(grid)

# Global variables

xmin <-   0
xmax <-  10
xrange <- xmax - xmin
x_init <- xrange/2
xh <- x_init

mmin <-   0
mmax <-   5
mrange <- mmax - mmin
m_init <- mrange/2
mh <- m_init

mlo <-  mmin
mhi <-  mmax

byx_init  <- .5
bym_init  <- 0
byxm_init <- 1
by0_init  <- 0

y_init <- by0_init + byx_init*x_init + bym_init*m_init + byxm_init*x_init*m_init
yh <- y_init

by0h <- by0_init
byxh <- byx_init + byxm_init*m_init

# UI
ui <- fluidPage(
  titlePanel("Moderation: Illustration [Work-in-progress]"),
  fluidRow(
    column(12,
      wellPanel(
        p("This page illustrates, in a simple moderation model, ",
            "how the moderator (M) changes the relationship between",
            "independent variable (X) and the dependent variable (Y). ",
            "Try to change the value of M and see what happens. ",
            "You can also change the three coefficients. For example, ",
            "you can see what happens if the product term is negative.")
        ),
      fluidRow(
        column(4,
          wellPanel(
            #h4("Settings"),
            sliderInput('xi',
              label=h5("X (Independent variable)"),
              min=xmin, max=xmax, value=x_init, step=.5,
              ticks=TRUE),
            sliderInput('mi',
              label=h5("M (Moderator)"),
              min=mmin, max=mmax, value=m_init, step=.5,
              ticks=TRUE),
            sliderInput('byx',
              label=h5("Coefficient of X"),
              min=-2, max=2, value=byx_init, step=.1,
              ticks=TRUE),
            sliderInput('bym',
              label=h5("Coefficient of M"),
              min=-2, max=2, value=bym_init, step=.1,
              ticks=TRUE),
            sliderInput('byxm',
              label=h5("Coefficient of X by M (the product term)"),
              min=-2, max=2, value=byxm_init, step=.1,
              ticks=TRUE),
            h4("Miscellaneous"),
            p("The following options are non-essential. They are",
              "included here for completeness."),
            sliderInput('by0',
              label=h5("Intercept"),
              min=-2, max=2, value=by0_init, step=.1,
              ticks=TRUE),
            h5("Technical details:"),
            p("For simplicity, the value of M is constrianed to be non-negative.")
            )
          ),
        column(8,
          plotOutput('plot', height="300px"),
          plotOutput('plot2', height="300px")
          )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("The latest version of the code can be found at ",
          a("lstatdemo at GitHub", 
            href="https://github.com/sfcheung/lstatdemo/inst/apps/moderationGraphs"),
          "."
          ),
        p("The whole repository can be downloaded from GitHub and run in R by",
          code("runGitHub(\"lstatdemo\",\"sfcheung\",subdir=\"inst/apps/moderationGraphs\")")
          )
        )
      )
    )
  )

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    byx <- input$byx
    bym <- input$bym
    byxm <- input$byxm
    by0 <- input$by0
    lm_y <- function(x, m) {by0 + byx*x + bym*m + byxm*x*m}
    lm_y_range <- c(lm_y(xmin, mmin), lm_y(xmin, mmax), 
                    lm_y(xmax, mmin), lm_y(xmax, mmax))
    ymin0 <- min(lm_y_range)
    ymax0 <- max(lm_y_range)
    y2 <- sort(c(ymin0, ymax0))
    ymin <- (y2[1]); ymax <- (y2[2])
    # Current valules
    xi <- input$xi
    mi <- input$mi
    yi <- lm_y(xi, mi)
    by0i <- by0 + bym*mi 
    byxi <- byx + byxm*mi
    lm_yi <- function(x) {by0i + byxi*x}
    # Changes
    x_change <- xi - xh
    m_change <- mi - mh
    y_change <- yi - yh
    # Plot the graphs
    par(mfrow=c(1, 2))
    #layout(matrix(c(1, 2, 1, 2, 3, 3), 3, 2, byrow=TRUE))
    arrow_len <- .10
    plot(x=NULL, y=NULL, xlim=c(xmin, xmax), ylim=c(ymin, ymax), 
          type="n", xlab="X", ylab="Y", asp=NA, 
          main=paste("X on Y for \nM = ", mlo, " (thin) or ", mhi, " (thick)", sep=""))
    parusr <- par("usr")
    lcol <- switch(sign(byx + byxm*mlo) + 2, "red", "green", "blue")
    abline(by0 + bym*mlo, byx + byxm*mlo, lwd=2, col=lcol)
    lcol <- switch(sign(byx + byxm*mhi) + 2, "red", "green", "blue")
    abline(by0 + bym*mhi, byx + byxm*mhi, lwd=4, col=lcol)
    #suppressWarnings(arrows(xi, parusr[3], xi, mi, length=arrow_len, 
    #                  col="black", lwd=2, lty="dotted"))
    #suppressWarnings(arrows(xi, mi, parusr[1], mi, length=arrow_len, 
    #                  col="blue", lwd=2, lty="dotted"))
    #suppressWarnings(arrows(xh, parusr[3], xh, mh, length=arrow_len, 
    #                  col="black", lty="dotted"))
    #suppressWarnings(arrows(xh, mh, parusr[1], mh, length=arrow_len, 
    #                  col="blue", lty="dotted"))
    #suppressWarnings(arrows(xh, mh, xi, mi, length=arrow_len*2, 
    #                  col="black", lwd=6, lty="dotted"))
    
    plot(x=NULL, y=NULL, xlim=c(xmin, xmax), ylim=c(ymin, ymax), 
          type="n", xlab="X", ylab="Y", asp=NA, 
          main=paste("X on Y for M = ", mi, 
                     "\nCurrent X's Effect = ", byxi, sep=""),
          sub="The dashed lines show the previous line and values")
    parusr <- par("usr")
    lcol <- switch(sign(byxi) + 2, "red", "green", "blue")
    abline(by0i, byxi, lwd=2, col=lcol)
    lcol <- switch(sign(byxh) + 2, "red", "green", "blue")
    abline(by0h, byxh, lwd=.5, col=lcol, lty="longdash")
    suppressWarnings(arrows(xi, parusr[3], xi, yi, length=arrow_len, 
                      col="grey50", lwd=2, lty="dotted"))
    suppressWarnings(arrows(xi, yi, parusr[1], yi, length=arrow_len, 
                      col="grey50", lwd=2, lty="dotted"))
    suppressWarnings(arrows(xh, parusr[3], xh, yh, length=arrow_len, 
                      col="grey50", lty="longdash"))
    suppressWarnings(arrows(xh, yh, parusr[1], yh, length=arrow_len, 
                      col="grey50", lty="longdash"))
    #suppressWarnings(arrows(mh, yh, mh, lm_y(xi, mh), 
    #                  length=arrow_len*2, col="black", 
    #                  lwd=6, lty="dotted"))
    #suppressWarnings(arrows(mh, lm_y(xi, mh), mi, yi, 
    #                  length=arrow_len*2, col="blue", 
    #                  lwd=6, lty="dotted"))
    xh <<- xi
    mh <<- mi
    yh <<- yi
    by0h <<- by0i
    byxh <<- byxi
    })
  output$plot2 <- renderPlot({
    byx <- input$byx
    bym <- input$bym
    byxm <- input$byxm
    by0 <- input$by0
    lm_y <- function(x, m) {by0 + byx*x + bym*m + byxm*x*m}
    # Current valules
    xi <- input$xi
    mi <- input$mi
    yi <- lm_y(xi, mi)
    by0i <- by0 + bym*mi 
    byxi <- byx + byxm*mi
    byxmin0 <- byx + byxm*mmin
    byxmax0 <- byx + byxm*mmax
    byxmin <- min(byxmin0, byxmax0)
    byxmax <- max(byxmin0, byxmax0)
    byxmaxabs <- max(abs(c(byxmin0, byxmax0)))
    lm_yi <- function(x) {by0i + byxi*x}
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
    #grid.lines(x=c(.15, .50 - dx/2), y=c(.25 + dy, .75 - dy), 
    #           arrow=arrowi)
    #grid.lines(x=c(.50 + dx/2, .85), y=c(.75 - dy, .25 + dy), 
    #           arrow=arrowi)
    lcol <- switch(sign(byxi) + 2, "red", "green", "blue")
    grid.lines(x=c(.15 + dx, .85 - dx), y=c(.25, .25), 
               arrow=arrowi, gp=gpar(col=lcol, lwd=.25+4*abs(byxi/byxmaxabs)))
    grid.lines(x=c(.50, .50), y=c(.75 - dy, .25 + dy), 
               arrow=arrowi)
    path_fs <- gpar(fontsize=18)
    #grid.text(paste("a = ", bmx, sep=""), 
    #          x=(.15 + .50)/2, y=(.25 + .75)/2 + dy*1.5, 
    #          gp=gpar(fontsize=18, col="blue"))
    #grid.text(paste("b = ", bym, sep=""), 
    #          x=(.50 + .85)/2, y=(.75 + .25)/2 + dy*1.5, 
    #          gp=gpar(fontsize=18, col="red"))
    grid.text(paste("Current X's Effect  = ", byxi, 
                    " [= ", byx, "+(", byxm, ")*(", mi, ")]", sep=""), 
              x=(.15 + .85)/2, y=(.25 + .25)/2 - dy, 
              gp=gpar(fontsize=18, col=lcol))
    grid.text(paste("How M affects X's Effect\n", 
                    "X's Effect = ", byx, "+(", byxm, ")*M", sep=""), 
              x=.50 + dx/2, y=(.75 + .25)/2, 
              gp=gpar(fontsize=18, col="black"), just="left")
    grid.text(paste("Full Equation: Y=(", by0, ") + (", byx, ")X + (", 
                    bym, ")M + (",
                    byxm, ")X*M + error", sep=""),
              x=.50, y=.90, gp=gpar(fontsize=18))
    popViewport()
  })  
  }

shinyApp(ui=ui, server=server)
