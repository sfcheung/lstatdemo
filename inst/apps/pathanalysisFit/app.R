# Demonstrate Path Analysis (Work-in-progress)

library(grid)
if (!(requireNamespace("lavaan", quietly = TRUE) & 
      requireNamespace("semPlot", quietly = TRUE))) {
  stop("This app requires both the packages lavaan and semPlot.")
  }

# Global variables

modelp <- "
  intention ~ .4*attitude + .3*sbjnorm + .3*pbc
  behavior  ~ .4*intention + .2*attitude + .2*sbjnorm + .4*pbc
  attitude ~~ .2*sbjnorm
  attitude ~~ .2*pbc
  sbjnorm  ~~ .21*pbc
  "
n <- 200
mydata <- lavaan::simulateData(model=modelp, sample.nobs = n, seed = 897981,
                               standardized = TRUE)  
model1 <- "
  intention ~ attitude  + sbjnorm + pbc
  behavior  ~ intention + pbc
  "
#model1m <- lavaan::lavaanify(model1, auto.var=TRUE)                               
model1_fit <- lavaan::sem(model1, mydata)

model2 <- model1
model2_fit <- model1_fit
model3 <- model2
model3_fit <- model2_fit

modelfull <- "
  intention ~ attitude  + sbjnorm + pbc
  behavior  ~ intention + attitude + sbjnorm + pbc
  "
#model1fullm <- lavaan::lavaanify(modelfull, auto.var=TRUE)                               
modelindep <- "
  intention ~ 0*attitude  + 0*sbjnorm + 0*pbc
  behavior  ~ 0*intention + 0*attitude + 0*sbjnorm + 0*pbc
  attitude ~~ 0*sbjnorm
  attitude ~~ 0*pbc
  sbjnorm  ~~ 0*pbc
  "

new_parTable <- function(tobi_choices, tobeh_choices) {
    if (length(tobi_choices) > 0) {
      tobi <- paste("intention ~", paste(tobi_choices, collapse=" + "))
      } else {
      tobi <- "intention ~ 0*attitude + 0*sbjnorm + 0*pbc"
      }
    if (length(tobeh_choices) > 0) {
      tobeh <- paste("behavior ~", paste(tobeh_choices, collapse=" + "))
      } else {
      tobeh <- "behavior ~ 0*intention + 0*attitude + 0*sbjnorm + 0*pbc"
      }
    ivcov <- "attitude ~~ sbjnorm \n attitude ~~ pbc \n sbjnorm ~~ pbc "
    out <- lavaan::lavaanify(paste(tobi, "\n", tobeh, "\n", ivcov), 
                             auto.var=TRUE, fixed.x=FALSE)
  }

my_paths <- function(sem_fit, main) {
  semPlot::semPaths(sem_fit, whatLabels="est",
      sizeMan=12, sizeLat=8, nCharNodes=0, rotation=2,
      edge.label.cex=2.5, edge.color="black", edge.width=4, 
      node.width=1.5, curve=1.75, exoVar=TRUE,
      style="lisrel", fixedStyle=c("white", 0),
      mar=c(3, 10, 5, 10))
  title(main)
  }
  
plot_fit <- function(model_fits, titles, index, main, fFUN=NULL, 
                     txtpos=1, ptcex=1.5, ...) {
  k <- length(model_fits)
  fitmeas <- rep(NA, k)
  dfs     <- rep(NA, k)
  for (i in 1:k) {
    fitmeas[i] <- lavaan::fitMeasures(model_fits[[i]], index)
    dfs[i] <- lavaan::fitMeasures(model_fits[[i]], "df")
    if (!is.null(fFUN)) fitmeas[i] <- fFUN(fitmeas[i], dfs[i])
    }
  plot(dfs, fitmeas, main=main, cex=ptcex, ...)
  if (length(txtpos) == 1) txtpos <- rep(txtpos, k)
  for (i in 1:k) {
    text(dfs[i], fitmeas[i], titles[i], pos=txtpos[i], ...)
    }
  }

plot_fit_bar <- function(model_fits, titles, index, main, ...) {
  k <- length(model_fits)
  fitmeas <- rep(NA, k)
  for (i in 1:k) {
    fitmeas[i] <- lavaan::fitMeasures(model_fits[[i]], index)
    }
  barplot(fitmeas, names.arg=titles, main=main, ...)
  }  
  
# UI
ui <- fluidPage(
  titlePanel("Path Analysis: Illustration"),
  fluidRow(
    column(12,
      wellPanel(
        p("This page illustrates how the various measures of goodness or ",
          "badness of fit changes with model specification. A random dataset of",
          "200 cases are generated and two models, plus the baseline model and ",
          "a saturated model, are fitted to the data. You can see how model",
          "chi-squared, degrees of freedom, and other indices based on these",
          "two numbers changes with models."),
        p("You can use the following check boxes to change the free paths ",
          "in Model 1 and Model 2, and click 'Update the results' to see how ",
          "the goodness/badness of fit and model complexity change.")
        ))),
  fluidRow(
    column(3,
      h4("Model 1"),
      checkboxGroupInput("tobi", "Intention is affected by:",
                         c("attitude" = "attitude",
                           "sbjnorm" = "sbjnorm",
                           "pbc" = "pbc"),
                         selected=c("attitude"))),
    column(3, 
      h4("Model 1"),
      checkboxGroupInput("tobeh", "Behavior is affected by:",
                         c("attitude" = "attitude",
                           "sbjnorm" = "sbjnorm",
                           "pbc" = "pbc",
                           "intention" = "intention"),
                           selected=c("intention"))),
    column(3, 
      h4("Model 2"),
      checkboxGroupInput("tobi2", "Intention is affected by:",
                         c("attitude" = "attitude",
                           "sbjnorm" = "sbjnorm",
                           "pbc" = "pbc"),
                         selected=c("attitude", "sbjnorm", "pbc"))),
    column(3, 
      h4("Model 2"),
      checkboxGroupInput("tobeh2", "Behavior is affected by:",
                         c("attitude" = "attitude",
                           "sbjnorm" = "sbjnorm",
                           "pbc" = "pbc",
                           "intention" = "intention"),
                         selected=c("intention")))
      ),
  fluidRow(
    column(12, submitButton("Update the results"))
    ),
  fluidRow(column(12, plotOutput('plot'))),
  fluidRow(column(12, plotOutput('plot2'))),
  fluidRow(column(12, plotOutput('plot3'))),
#  fluidRow(
#    column(3, dataTableOutput('resindep')),
#    column(3, dataTableOutput('res1')),
#    column(3, dataTableOutput('res2')),
#    column(3, dataTableOutput('resfull'))
#    ),
  fluidRow(
    column(12,
      wellPanel(
        p("Note: Explanations for the graphs will be added later.")
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("The latest version of the code can be found at ",
          a("lstatdemo at GitHub", 
            href="https://github.com/sfcheung/lstatdemo/pathanalysisFit"),
          "."
          ),
        p("The whole repository can be downloaded from GitHub and run in R by",
          code("runGitHub(\"lstatdemo\",\"sfcheung\",subdir=\"inst/apps/pathanalysisFit\")")
          )
        )
      )
    )
  )

# Server
server <- function(input, output) {
  model2 <<- model1
  model2_fit <<- model1_fit
  model3 <<- model2
  model3_fit <<- model2_fit
  getModel  <- reactive(new_parTable(input$tobi, input$tobeh))
  getModel2 <- reactive(new_parTable(input$tobi2, input$tobeh2))
  output$plot <- renderPlot({
    model1 <<- getModel()
    model1_fit <<- lavaan::sem(model1, mydata, fixed.x=FALSE)
    model2 <<- getModel2()
    model2_fit <<- lavaan::sem(model2, mydata, fixed.x=FALSE)
    modelindep_fit <<- lavaan::sem(modelindep, mydata, fixed.x=FALSE)
    modelfull_fit <<- lavaan::sem(modelfull, mydata, fixed.x=FALSE)
    par(mfrow=c(1, 4))
    my_paths(modelindep_fit, "Baseline Model (Independence)")
    my_paths(model1_fit, "Model 1")
    my_paths(model2_fit, "Model 2")
    my_paths(modelfull_fit, "Saturated Model")
    })
  output$plot2 <- renderPlot({
    model1 <<- getModel()
    model1_fit <<- lavaan::sem(model1, mydata, fixed.x=FALSE)
    model2 <<- getModel2()
    model2_fit <<- lavaan::sem(model2, mydata, fixed.x=FALSE)
    modelindep_fit <<- lavaan::sem(modelindep, mydata, fixed.x=FALSE)
    modelfull_fit <<- lavaan::sem(modelfull, mydata, fixed.x=FALSE)
    model_fits <- list(modelindep_fit, model1_fit, model2_fit, modelfull_fit)
    titles <- c("Baseline", "Model 1", "Model 2", "Saturated")
    par(mfrow=c(1, 4))
    plot_fit_bar(model_fits, titles, index="chisq", main="Chi-squared")
    plot_fit_bar(model_fits, titles, index="df", main="Degrees of Freedom (df)")
    plot_fit(model_fits, titles, index="chisq", main="Chi-squared vs. df",
             xlab="Degrees of Freedom", ylab="Chi-squared", txtpos=c(2, 3, 3, 4))
    plot_fit(model_fits, titles, index="RMSEA", main="RMSEA",
             xlab="Degrees of Freedom", ylab="RMSEA", txtpos=c(2, 3, 3, 4))
    abline(h=.05, col="green")
    })
  output$plot3 <- renderPlot({
    model1 <<- getModel()
    model1_fit <<- lavaan::sem(model1, mydata, fixed.x=FALSE)
    model2 <<- getModel2()
    model2_fit <<- lavaan::sem(model2, mydata, fixed.x=FALSE)
    modelindep_fit <<- lavaan::sem(modelindep, mydata, fixed.x=FALSE)
    modelfull_fit <<- lavaan::sem(modelfull, mydata, fixed.x=FALSE)
    model_fits <- list(modelindep_fit, model1_fit, model2_fit, modelfull_fit)
    titles <- c("Baseline", "Model 1", "Model 2", "Saturated")
    chisqbase <- lavaan::fitMeasures(modelindep_fit, "chisq")
    dfbase    <- lavaan::fitMeasures(modelindep_fit, "df")
    chisq1 <- lavaan::fitMeasures(model1_fit, "chisq")
    df1    <- lavaan::fitMeasures(model1_fit, "df")
    chisq2 <- lavaan::fitMeasures(model2_fit, "chisq")
    df2    <- lavaan::fitMeasures(model2_fit, "df")
    par(mfrow=c(1, 4))
    plot_fit(model_fits, titles, index="chisq", main="Difference: Chisq - df", 
             fFUN=function(x, y) {x - y}, txtpos=c(2, 1, 1, 4),
             ylab="Chisq - df", xlab="Degrees of Freedom")
    abline(h=chisqbase - dfbase, col="red")
    abline(h=0, col="blue")
    abline(h=.05*(chisqbase - dfbase), col="green")
    arrows(df1, chisqbase - dfbase, df1, chisq1 - df1, length=.15)
    arrows(df2, chisqbase - dfbase, df2, chisq2 - df2, length=.15)
    plot_fit(model_fits, titles, index="CFI", main="CFI", txtpos=c(2, 1, 1, 4),
             ylab="CFI", xlab="Degrees of Freedom")
    abline(h=0, col="red")
    abline(h=.95, col="green")
    abline(h=1, col="blue")
    chisqdfratio <- function(x, y) {
      if (y == 0) {
        out <- NA
      } else {
        out <- x / y
      }
      out
      }
    plot_fit(model_fits, titles, index="chisq", main="Ratio: Chisq/df",
             fFUN=chisqdfratio, txtpos=c(2, 1, 1, 4), 
             sub="Chisq/df undefined if df = 0", 
             ylim=c(1, max(chisqbase/dfbase, 
                           ifelse(df1 > 0, chisq1/df1, 0), 
                           ifelse(df2 > 0, chisq2/df2, 0))),
             ylab="Chisq/df", 
             xlab="Degrees of Freedom")
    abline(h=chisqbase/dfbase, col="red")
    abline(h=1, col="blue")
    abline(h=.05*(chisqbase/dfbase - 1) + 1, col="green")
    arrows(df1, chisqbase/dfbase, df1, chisq1/df1, length=.15)
    arrows(df2, chisqbase/dfbase, df2, chisq2/df2, length=.15)
    plot_fit(model_fits, titles, index="TLI", main="TLI", txtpos=c(2, 1, 1, 4),
             ylab="TLI", xlab="Degrees of Freedom")
    abline(h=0, col="red")
    abline(h=.95, col="green")
    abline(h=1, col="blue")
    })
#  output$resindep <- renderDataTable(round(lavaan::residuals(modelindep_fit)$cov, 3))
#  output$res1     <- renderDataTable(round(lavaan::residuals(model1_fit)$cov, 3))
#  output$res2     <- renderDataTable(round(lavaan::residuals(model2_fit)$cov, 3))
#  output$resfull  <- renderDataTable(round(lavaan::residuals(modelfull_fit)$cov, 3))
  }

shinyApp(ui=ui, server=server)
