# Demonstrate Path Analysis (Work-in-progress)

library(grid)
if (!(requireNamespace("lavaan", quietly = TRUE) & 
      requireNamespace("semPlot", quietly = TRUE))) {
  stop("This app requires both the packages lavaan and semPlot.")
  }

# Global variables

modelp <- "
  intention ~ .4*attitude + .3*sbjnorm + .3*pbc
  behavior  ~ .4*intention + .3*pbc
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
      sizeMan=8, sizeLat=8, nCharNodes=0, rotation=2,
      edge.label.cex=2, edge.color="black", edge.width=1.5, 
      node.width=1.5, curve=1.75, exoVar=TRUE,
      style="lisrel", fixedStyle=c("white", 0),
      mar=c(3, 10, 10, 10))
  title(main)
  }
  
# UI
ui <- fluidPage(
  titlePanel("Path Analysis: Illustration ) (Work-in-progress)"),
  fluidRow(
    column(12,
      wellPanel(
        p("This page illustrates..."),
        p("If ...")
        ))),
  fluidRow(
    column(3,
      h4("Model 1"),
      checkboxGroupInput("tobi", "Intention is affected by:",
                         c("attitude" = "attitude",
                           "sbjnorm" = "sbjnorm",
                           "pbc" = "pbc"),
                         selected=c("attitude", "sbjnorm", "pbc"))),
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
                         selected=c("pbc", "intention")))
      ),
  fluidRow(
    column(12, submitButton("Update the results"))
    ),
  fluidRow(column(12, plotOutput('plot'))),
  fluidRow(
    column(3, dataTableOutput('resindep')),
    column(3, dataTableOutput('res1')),
    column(3, dataTableOutput('res2')),
    column(3, dataTableOutput('resfull'))
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
    my_paths(modelindep_fit, "Independence Model")
    my_paths(model1_fit, "Model 1")
    my_paths(model2_fit, "Model 2")
    my_paths(modelfull_fit, "Saturated Model")
    })
  output$resindep <- renderDataTable(round(lavaan::residuals(modelindep_fit)$cov, 3))
  output$res1     <- renderDataTable(round(lavaan::residuals(model1_fit)$cov, 3))
  output$res2     <- renderDataTable(round(lavaan::residuals(model2_fit)$cov, 3))
  output$resfull  <- renderDataTable(round(lavaan::residuals(modelfull_fit)$cov, 3))
  }

shinyApp(ui=ui, server=server)
