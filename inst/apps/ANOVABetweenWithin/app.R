# Illustrate the idea of residual and sum of squares in one-way ANOVA

# Global variables
anova.data <- read.csv("anova.data.csv", header = TRUE, as.is=1)
gp_means <- tapply(anova.data$score, anova.data$group, mean)
g_mean <- mean(anova.data$score)
gp_f <- factor(levels(anova.data$group))
anova.data$score <- anova.data$score -
                    rep(gp_means, times=table(anova.data$group))
                    
anova.data.wide <- cast(anova.data, person ~ group, value="score")

person_def <- rep(c("Person 1", "Person 2", "Person 3", "Person 4", "Person 5"), times=4)
person_hi <- person_1[c(1:5, 5+c(1,2,3,5,4), 10+c(1,2,5,4,3), 15+c(1,2,5,3,4))]
person_lo <- person_1[c(1:5, 5+c(4,3,2,5,1), 10+c(1,4,5,2,3), 15+c(2,1,5,4,3))]
persons <- list(person_lo, person_def, person_hi)
                    
# Initial gp_means
gp_means <- c(24,16,6,22)
n_total <- nrow(anova.data)
n_gp <- table(anova.data$group)
score_max <- max(anova.data$score)
score_min <- min(anova.data$score)
sdC_max <- 2
sdC_min <- 0
gp_means_max <- max(gp_means) + 5
gp_means_min <- 5
yAxis_max <- sdC_max*score_max + gp_means_max
yAxis_min <- sdC_max*score_min - gp_means_min
per_corr=2

# UI
ui <- fluidPage(
  titlePanel("Illustrate the difference in results between one-way indepdent group ANOVA and repeated measures ANOVA"),
  fluidRow(
    column(12,
      wellPanel(
        p("This demonstration shows ..."),
        p("You can also change ..."),
        p("...")
        ),
      fluidRow(
        column(4,
          wellPanel(
            p("Change the following and see how the residuals change:"),
               h6("Means:"),
               sliderInput('m1', "Group 1",
                  min=gp_means_min, max=gp_means_max, value=gp_means[1], step=1),
               sliderInput('m2', "Group 2",
                  min=gp_means_min, max=gp_means_max, value=gp_means[2], step=1),
               sliderInput('m3', "Group 3",
                  min=gp_means_min, max=gp_means_max, value=gp_means[3], step=1),
               sliderInput('m4', "Group 4",
                  min=gp_means_min, max=gp_means_max, value=gp_means[4], step=1),
               br(),
               h6("Within-Person Correlation"),
               radioButtons('per_corr',
                "Select how correlated a person's scores are",
                c("Low"=1, "Default"=2, "High"=3), selected=2),
               h6("Within Measure Variation"),
               sliderInput('sdC',
                "Increase/decrease variation within a measure by this factor",
                  min=sdC_min, max=sdC_max, value=1, step=.05)
              )
          ),
        column(8,
          plotOutput('plot', height="600px")
          )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("This webpage is included in the package",
          a("lstatdemo",
            href="https://github.com/sfcheung/lstatdemo/"),
          " at GitHub.")
        )
      )
    )
  )



# Server
server <- function(input, output, session) {
  output$plot <- renderPlot({
    gp_means[1] <- input$m1
    gp_means[2] <- input$m2
    gp_means[3] <- input$m3
    gp_means[4] <- input$m4
    sdC <- input$sdC
    per_corr <- input$per_corr
    
    # Create the data frame
    anova.data.i <- anova.data
    anova.data.i$score <- sdC*anova.data.i$score +
                          rep(gp_means, times=n_gp)
    g_mean <- mean(anova.data.i$score)
    anova.data.i$person <- persons[[as.numeric(per_corr)]]
     
    # Conduct the one-way independent group ANOVA
    anova.results <- aov(score ~ group, data=anova.data.i)
    anova.F <- summary(anova.results)[[1]]$F[1]
    dfs <- summary(anova.results)[[1]]$Df
    anova.F.p <- summary(anova.results)[[1]]$Pr[1]
    SSb <- summary(anova.results)[[1]]$Sum[1]
    SSw <- summary(anova.results)[[1]]$Sum[2]
    dfb <- dfs[1]
    dfw <- dfs[2]
    F.cut <- qf(1-.05, dfb, dfw)

    # Conduct the one-way repeated measures ANOVA
    anova_r.results <- aov(score ~ group + Error(person/group), data=anova.data.i)
    anova_r.F <- summary(anova_r.results)[[2]][[1]]$F[1]
    dfs_r <- summary(anova_r.results)[[2]][[1]]$Df
    anova_r.F.p <- summary(anova_r.results)[[2]][[1]]$Pr[1]
    SSb_r <- summary(anova_r.results)[[2]][[1]]$Sum[1]
    SSw_r <- summary(anova_r.results)[[2]][[1]]$Sum[2]
    SSr_r <- summary(anova_r.results)[[1]][[1]]$Sum[1]
    dfb_r <- dfs_r[1]
    dfw_r <- dfs_r[2]
    dfr_r <- summary(anova_r.results)[[1]][[1]]$Df
    F_r.cut <- qf(1-.05, dfb_r, dfw_r)

    # Set graph parameters
    yMin <- min(anova.data.i$score)
    yMax <- max(anova.data.i$score)
    yMin <- yAxis_min
    yMax <- yAxis_max

    Flo <- 0
    Fhi <- max(anova.F*1.25, qf(1-.0005, dfb, dfw))
    FRange <- seq(Flo, Fhi, length.out=100)
    Fd <- df(FRange, dfb, dfw)
    FdMax <- max(Fd)
    FpRange <- c(anova.F, seq(anova.F, Fhi, length.out=50), Fhi)
    Fpd <- df(FpRange, dfb, dfw)

    Flo_r <- 0
    Fhi_r <- max(anova_r.F*1.25, qf(1-.0005, dfb_r, dfw_r))
    FRange_r <- seq(Flo_r, Fhi_r, length.out=100)
    Fd_r <- df(FRange_r, dfb_r, dfw_r)
    FdMax_r <- max(Fd_r)
    FpRange_r <- c(anova_r.F, seq(anova_r.F, Fhi_r, length.out=50), Fhi_r)
    Fpd_r <- df(FpRange_r, dfb_r, dfw_r)    
    
    cexAll <- 1
    cexPt <- 2
    # Don't know why cex cannot control the magnification of all elements
    # So used cexAll here
    # Generate the plot object

    par(mfrow=c(3,2))
    par(mar=c(5,2,3,2))

    # Plot group means
    plot(gp_f, gp_means, border="white",
        ylim=c(yMin, yMax),
        ylab="Score",
        xlab="Group",
        main=c("Group Means"), cex.axis=1.5, cex.main=1.5, cex.lab=1.5)
    points(gp_f, gp_means, cex=cexPt, pch=16)
    lines(gp_f, gp_means, col="blue", lwd=4)
    abline(h=g_mean, lty="dotted", lwd=2, col="red")

    # Plot deviation from grand mean
    #plot(anova.data.i$score,
    #    ylim=c(yMin, yMax),
    #    ylab="Score",
    #    xlab="Case",
    #    pch=16, cex=cexPt,
    #    main=c("Deviation of Each Case from Grand Mean",
    #            "(Total Sum of Squares)"), cex.axis=1.5, cex.main=1.5,
    #    cex.lab=1.5)
    #abline(h=g_mean, lty="dotted", lwd=2, col="grey")
    #segments(1:n_total, g_mean, 1:n_total, anova.data.i$score, lty="dotted",
    #        col="red", lwd=2)

    # Plot person lines
    
    plot(anova.data.i[anova.data.i$person == "Person 1", "score"], 
         type="o", ylim=c(yMin, yMax), axes=FALSE, 
         xlab="Time", ylab="Score", main="Repeated Measures", pch=16,
         col="red", cex=2, lwd=2, cex.axis=1.5, cex.main=1.5, cex.lab=1.5)
    axis(2, at=seq(yMin, yMax, 5), cex.axis=1.5)
    axis(1, at=1:4, labels=c("Time 1", "Time 2", "Time 3", "Time 4"),
         cex.axis=1.5, )
    lines(1:4, anova.data.i[anova.data.i$person == "Person 2", "score"], 
          type="o", col="blue", cex=2, lwd=2)
    lines(1:4, anova.data.i[anova.data.i$person == "Person 3", "score"], 
          type="o", col="darkgreen", cex=2, lwd=2)
    lines(1:4, anova.data.i[anova.data.i$person == "Person 4", "score"], 
          type="o", col="orange", cex=2, lwd=2)
    lines(1:4, anova.data.i[anova.data.i$person == "Person 5", "score"], 
          type="o", col="black", cex=2, lwd=2)
    abline(h=g_mean, lty="dotted", lwd=2, col="red")
    box()

            
    # Plot sums of squares
    anova.SS <- matrix(c(SSb, SSw), 2, 1)
    rownames(anova.SS) <- c("Between", "Within")
    colnames(anova.SS) <- c("SS")
    barplot(anova.SS,
            xlab="", ylab="",
            col=c(rgb(1,0.5,0.5,.25),rgb(0.5,0.5,1,.25)),
            legend=rownames(anova.SS), horiz=FALSE,
            main=c("Independent Group: Partition the Total Sum of Squares",
                    paste("(Between SS: ",sprintf("%8.2f",SSb),
                          " / Within SS: ",sprintf("%8.2f",SSw),")",
                          sep="")), cex.axis=1.5, cex.main=1.5, cex.lab=1.5)

    # Plot sums of squares
    anova_r.SS <- matrix(c(SSb_r, SSw_r, SSr_r), 3, 1)
    rownames(anova_r.SS) <- c("Measures", "Residual", "Person")
    colnames(anova_r.SS) <- c("SS")
    barplot(anova_r.SS,
            xlab="", ylab="",
            col=c(rgb(1,0.5,0.5,.25),rgb(0.5,0.5,1,.25),rgb(0.5,0.5,0.5,.25)),
            legend=rownames(anova_r.SS), horiz=FALSE,
            main=c("Repeated Measures: Partition the Total Sum of Squares",
                    paste("(Measures SS: ",sprintf("%8.2f",SSb_r),
                          " / Residual SS: ",sprintf("%8.2f",SSw_r),
                          " / Person SS: ",sprintf("%8.2f",SSr_r),")",
                          sep="")), cex.axis=1.5, cex.main=1.5, cex.lab=1.5)


    # Plot deviation of group mean from grand mean
    #plot(anova.data.i$score,
    #     ylim=c(yMin, yMax),
    #     ylab="Score",
    #     xlab="Case",
    #     pch=16, cex=cexPt,
    #     main=c("Deviation of Group Mean from Grand Mean",
    #            "(Between-Group Sum of Squares)"),
    #     cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
    #abline(h=g_mean, lty="dotted", lwd=2, col="red")
    #segments(cumsum(n_gp) - n_gp + 1, gp_means, cumsum(n_gp), gp_means,
    #         col="black", lwd=2)
    #segments(1:n_total, g_mean, 1:n_total, rep(gp_means, times=n_gp),
    #         col="red", lty="solid", lwd=2)


    # Plot F distribution
    plot(FRange,Fd,type="l",
          xlab="F statistic",
          ylab="",yaxt="n",
          main=c("Independent Group: Theoretical distribution of F statistic",
                 paste("(df Between =", dfb, ", df Within =", dfw, ")",
                 sep="")), cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
    polygon(FRange,Fd,col=rgb(0,1,0,.5))
    abline(v=F.cut, lwd=1, col="black", lty="dotted")
    text(F.cut, FdMax,
        paste("Critical value\n", sprintf("%3.2f",F.cut), sep=""),
        adj=c(0.5,1), cex=1.5)
    abline(v=anova.F, lwd=1, col="red")
    text(anova.F, FdMax*.5, paste("Sample F\n", sprintf("%3.2f", anova.F),
                                  "\np-value=", sprintf("%5.4f", anova.F.p),
                                  sep=""), cex=1.5)

    # Plot F distribution
    plot(FRange_r,Fd_r,type="l",
          xlab="F statistic",
          ylab="",yaxt="n",
          main=c("Repeated Measures: Theoretical distribution of F statistic",
                 paste("(df Measures =", dfb_r, ", df Residual =", dfw_r, ")",
                 sep="")), cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
    polygon(FRange_r,Fd_r,col=rgb(0,1,0,.5))
    abline(v=F_r.cut, lwd=1, col="black", lty="dotted")
    text(F_r.cut, FdMax_r,
        paste("Critical value\n", sprintf("%3.2f",F_r.cut), sep=""),
        adj=c(0.5,1), cex=1.5)
    abline(v=anova_r.F, lwd=1, col="red")
    text(anova_r.F, FdMax_r*.5, paste("Sample F\n", sprintf("%3.2f", anova_r.F),
                                  "\np-value=", sprintf("%5.4f", anova_r.F.p),
                                  sep=""), cex=1.5)



    # Plot deviation from group mean
    #plot(anova.data.i$score,
    #     ylim=c(yMin, yMax),
    #     ylab="Score",
    #     xlab="Case",
    #     pch=16, cex=cexPt,
    #     main=c("Deviation of Each Case from Group Mean",
    #            "(Within-Group Sum of Squares)"), cex.axis=1.5,
    #     cex.main=1.5, cex.lab=1.5)
    #abline(h=g_mean, lty="dotted", lwd=2, col="red")
    #segments(cumsum(n_gp) - n_gp + 1, gp_means, cumsum(n_gp), gp_means,
    #         col="black", lwd=2)
    #segments(1:n_total, rep(gp_means, times=n_gp), 1:n_total, anova.data.i$score,
    #         col="blue", lty="solid", lwd=2)



  })
}

shinyApp(ui=ui, server=server)
