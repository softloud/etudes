# server for comparison of variance.

# Packages ----------------------------------------------------------------
library(tidyverse)
library(actuar)

# Functions ---------------------------------------------------------------
# source('/home/charles/Dropbox/PhD/paper1/approx_functions.R')
source("approx_functions.R")

# Server ------------------------------------------------------------------

shinyServer(function(input, output) {
  dist.info <- reactive({
    if (input$distribution == "Lognormal") {
      rname <- "lnorm"
      parameters <- c(input$mu, input$sigma)
    } else if (input$distribution == "Weibull") {
      rname <- "weibull"
      parameters <- c(shape = input$shape,
                      scale = input$scale)
    } else if (input$distribution == "Chi-square") {
      rname <- "chisq"
      parameters <- c(df = input$df,
                      ncp = input$ncp)
    } else if (input$distribution == "Exponential") {
      rname <- "exp"
      parameters <- c(rate = input$rate)
    } else if (input$distribution == "Normal") {
      rname <- "norm"
      parameters <- c(mean = input$mu,
                      sd =  input$sigma)
    } else if (input$distribution == "F") {
      rname <- "f"
      parameters <- c(df1 = input$df1,
                      df2 = input$df2,
                      ncp = input$ncp)
    } else if (input$distribution == "Cauchy") {
      rname <- "cauchy"
      parameters <- c(location = input$location,
                      scale = input$scale)
    } else if (input$distribution == "Pareto II") {
      rname <- "pareto2"
      parameters <- c(shape = input$shape,
                      scale = input$scale)
    }
    list(rname = rname, parameters = parameters)
  })

  values <- reactive({
    # Calculate true median.
    true.quants <- DistFn(c(0.25, 0.5, 0.75),
                           "q",
                           dist.info()$rname,
                           dist.info()$parameters)
    # Calculate density at median based on chosen distribution.
    f.m <- DistFn(true.quants[2],
                   "d",
                   dist.info()$rname,
                   dist.info()$parameters)
    iqr <- true.quants[3] - true.quants[1]

    # Calculate density at median with lognormal.
    # Get parameters for lognormal.
    if (input$approx.dist == "Lognormal") {
      approx.dist.par <- LnormPar(true.quants[2], iqr)
      f.approx <-
        dlnorm(true.quants[2],
               approx.dist.par$mu,
               approx.dist.par$sigma)
    } else if (input$approx.dist == "Exponential") {
      approx.dist.par <- expPAR(true.quants[2])
      f.approx <- dexp(true.quants[2], rate = approx.dist.par)
    } else if (input$approx.dist == "Pareto II") {
      approx.dist.par <- paretoPAR(true.quants[2], iqr)
      if (any(approx.dist.par < 0)) {
        approx.dist.par$alpha <- 0.01
        approx.dist.par$lambda <- 0.01
      }
      f.approx <- dpareto2(true.quants[2],
                           shape = approx.dist.par$alpha,
                           scale = approx.dist.par$lambda)
    }

    list(
      true.quants = true.quants,
      iqr = iqr,
      approx.var = 1 / (4 * f.m),
      approx.var.lm = 1 / (4 * f.approx),
      xlim = DistFn(
        c(0.1, 0.9),
        "q",
        dist.info()$rname,
        dist.info()$parameters
      ),
      approx.dist.par = approx.dist.par
    )
  })

  output$median <- renderText({
    paste0("The true median is ", round(values()$true.quants[2], 2), ".")
  })

  output$variances <- renderTable({
    data.frame(var = values()$approx.var,
               var.approx = values()$approx.var.lm)
  })

  output$approx.dist.text <- renderText({
    input$approx.dist
  })
  output$var.text <- renderText({
    paste0(
      "Rounding to 2 decimal places, the variance approximated with the
      true distribution is ",
      round(values()$approx.var, 2),
      " and the variance approximated with the ",
      input$approx.dist,
      " distribution is ",
      round(values()$approx.var.lm, 2),
      "."
    )
  })

  output$plot <- renderPlot({
    # Lognormal parameters
    approx.dist.par <- as.numeric(values()$approx.dist.par)

    # Plot
    plot <- ggplot(data = data.frame(x = values()$xlim),
                   aes(x = x)) +
      geom_vline(
        xintercept = values()$true.quants[2],
        colour = "darkgreen",
        linetype = "dashed"
      ) +
      stat_function(
        fun = paste0("d", dist.info()$rname),
        args = as.list(dist.info()$parameters)
      )  +
      labs(
        x = "quantiles",
        y = "density",
        title = paste0(
          "Plot of the ",
          input$distribution,
          " distribution,
          under chosen parameters"
        )
        )

    if (input$approx.dist == "Lognormal") {
      plot +
        stat_function(
          fun = "dlnorm",
          args = list(meanlog = approx.dist.par[1],
                      sdlog = approx.dist.par[2]),
          colour = "darkblue",
          linetype = "dotted"
        )
    } else if (input$approx.dist == "Exponential") {
      plot +
        stat_function(
          fun = "dexp",
          args = list(rate = approx.dist.par),
          colour = "darkblue",
          linetype = "dotted"
        )

    } else if (input$approx.dist == "Pareto II") {
      plot +
        stat_function(
          fun = "dpareto2",
          args = list(shape = approx.dist.par[1],
                      scale = approx.dist.par[2]),
          colour = "darkblue",
          linetype = "dotted"
        ) +
        labs(x = as.character(approx.dist.par[1]))
    }
  })

})
