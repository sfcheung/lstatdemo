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
      
# UI
ui <- fluidPage(
  titlePanel("Path Analysis: Illustration ) (Work-in-progress)"),
  fluidRow(
    column(12,
      wellPanel(
        p("This page illustrates..."),
        p("If ...")
        ),
      fluidRow(
        column(4,
          wellPanel(
            h4("Model"),
            checkboxGroupInput("tobi", "Intention is affected by:",
                               c("attitude" = "attitude",
                                 "sbjnorm" = "sbjnorm",
                                 "pbc" = "pbc"),
                               selected=c("attitude", "sbjnorm", "pbc")),
            checkboxGroupInput("tobeh", "Behavior is affected by:",
                               c("attitude" = "attitude",
                                 "sbjnorm" = "sbjnorm",
                                 "pbc" = "pbc",
                                 "intention" = "intention"),
                               selected="pbc"),
            submitButton("Update the results")
            #h5("Technical details: [To be added]"),
            #paste("[Technical details: (To be added)]", sep="")
            )
          ),
        column(8,
          #textOutput('debugtext'),
          plotOutput('plot')
          )
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
  getModel <- reactive(new_parTable(input$tobi, input$tobeh))
  output$plot <- renderPlot({
    model1 <<- getModel()
    model1_fit <<- lavaan::sem(model1, mydata)
    semPlot::semPaths(model1_fit, whatLabels="est",
        sizeMan=4,
        sizeLat=8,
        nCharNodes=0,
        curve=1.5,
        rotation=2,
        edge.label.cex=1,
        edge.color="black",
        edge.width=1, node.width=1.25,
        style="lisrel", fixedStyle=c("white", 0))
    })
  }

shinyApp(ui=ui, server=server)
