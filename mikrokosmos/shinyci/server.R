# server for medians.
# 
# Packages ----------------------------------------------------------------
# library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(modeest) # For calculating modes.
library(shinythemes) # For gussying up the interface.
library(actuar) # For pareto distributions.

# Functions ---------------------------------------------------------------
lnormPAR <- function(m, iqr) {
  # This function estimates the parameters, mu and sigma, of a lognormal distribution given a sample median
  # and sample interquartile range.
  #
  # This function takes two arguments:
  #  m: median of some vector.
  #  iqr: interquartile range of some vector.
  #
  #  Returns a list with the two parameters of the underpinning normal
  #  distribution; i.e., the parameters of the normal distribution that the
  #  log-transformation of the response has. TODO: check this is correct.
  
  # Calculate mu as the log of the median.
  mu <- log(m)
  
  # Calculate sigma. 
  sigma <- (mu +
              log(1 / 2 *
                    (iqr +
                       sqrt(
                         iqr ^ 2 + 4 * exp(2 * mu)
                       )) /
                    exp(2 * mu))) /
    qnorm(0.75)
  
  # Return list with mu and sigma.
  list(mu = mu, sigma = sigma)
}

medianInt <- function(m, iqr, n) {
  # This function calculates a 95% confidence interval for the median,
  #  given a sample median, sample interquartile range, and sample size.
  #
  #  Arguments:
  #   m: sample median.
  #   iqr: sample interquartile range.
  #   n: sample size.
  #
  #  Returns:
  #   A confidence interval (numeric vector).
  par <- lnormPAR(m, iqr)
  V <- 1 / 4 / dlnorm(m, par$mu, par$sigma) ^ 2 / n
  
  ci <- m + c(-1, 1) * qnorm(0.975) * sqrt(V)
  
  ci
}

# Server ------------------------------------------------------------------
# Calculate true median of the chosen distribution.
shinyServer(
  function(input, output) {
    # Calculate true median based on chosen distribution.
    dist <- reactive({
      if (input$distribution == "Lognormal") {
        get.quant <- function(this.quant) {
          qlnorm(this.quant,
                 meanlog = input$mu,
                 sdlog =  input$sigma)
        }
        rname <- "lnorm"
        args <- list(meanlog = input$mu,
                     sdlog =  input$sigma)
        max.y <- dlnorm(
          lnormMode(meanlog = input$mu,
                    sdlog = input$sigma),
          meanlog = input$mu,
          sdlog = input$sigma
        )
      } else if (input$distribution == "Weibull") {
        get.quant <- function(this.quant) {
          qweibull(this.quant,
                   shape = input$shape,
                   scale = input$scale)
        }
        rname <- "weibull"
        args <- list(shape = input$shape,
                     scale = input$scale)
        max.y <- dweibull(0.5,
                          shape = input$shape,
                          scale = input$scale)
      } else if (input$distribution == "Chi-square") {
        get.quant <- function(this.quant) {
          qchisq(this.quant,
                 df = input$df,
                 ncp = input$ncp)
        }
        rname <- "chisq"
        args <- list(df = input$df,
                     ncp = input$ncp)
        max.y <- dchisq(0.5,
                        df = input$df,
                        ncp = input$ncp)
      } else if (input$distribution == "Exponential") {
        get.quant <- function(this.quant) {
          qexp(this.quant,
               rate = input$rate)
        }
        rname <- "exp"
        args <- list(rate = input$rate)
        max.y <- dexp(0.5, rate = input$rate)
        
      } else if (input$distribution == "Normal") {
        get.quant <- function(this.quant) {
          qnorm(this.quant,
                mean = input$mu,
                sd = input$sigma)
        }
        rname <- "norm"
        args <- list(mean = input$mu,
                     sd =  input$sigma)
        max.y <- dnorm(
          normMode(mean = input$mu,
                   sd = input$sigma),
          mean = input$mu,
          sd = input$sigma
        )
      } else if (input$distribution == "F") {
        get.quant <- function(this.quant) {
          qf(
            this.quant,
            df1 = input$df1,
            df2 = input$df2,
            ncp = input$ncp
          )
        }
        rname <- "f"
        args <- list(df1 = input$df1,
                     df2 = input$df2,
                     ncp = input$ncp)
        max.y <- df(
          fMode(df1 = input$df1,
                df2 = input$df2),
          df1 = input$df1,
          df2 = input$df2,
          ncp = input$ncp
        )
      } else if (input$distribution == "Cauchy") {
        get.quant <- function(this.quant) {
          qcauchy(this.quant,
                  location = input$location,
                  scale = input$scale)
        }
        rname <- "cauchy"
        args <- list(location = input$location,
                     scale = input$scale)
        max.y <- dcauchy(
          cauchyMode(location = input$location,
                     scale = input$scale),
          location = input$location,
          scale = input$scale
        )
      } else if (input$distribution == "Pareto") {
        get.quant <- function(this.quant) {
          qpareto(this.quant,
                  shape = input$shape,
                  scale = input$scale)
        }
          rname <- "pareto"
          args <- list(shape = input$shape,
                       scale = input$scale)
          max.y <- dpareto(get.quant(0.5),
            shape = input$shape,
            scale = input$scale
          )
        }
        
        list(
          true.m = get.quant(0.5),
          rname = rname,
          xlim = get.quant(input$xlim),
          args = args,
          max.y = max.y
        )
        
      })
    
    # Create data frame of confidence intervals.
    conf.ints <- reactive({
      # This function returns a ci for a
      get.ci <- function(i) {
        if (input$distribution == "Lognormal") {
          x <- rlnorm(input$n.sample,
                      meanlog = input$mu,
                      sdlog = input$sigma)
        } else if (input$distribution == "Weibull") {
          x <- rweibull(input$n.sample,
                        scale = input$scale,
                        shape = input$shape)
        } else if (input$distribution == "Chi-square") {
          x <- rchisq(input$n.sample,
                      df = input$df,
                      ncp = input$ncp)
        } else if (input$distribution == "Normal") {
          x <- rnorm(input$n.sample,
                     mean = input$mu,
                     sd = input$sigma)
        } else if (input$distribution == "F") {
          x <- rf(
            input$n.sample,
            df1 = input$df1,
            df2 = input$df2,
            ncp = input$ncp
          )
        } else if (input$distribution == "Exponential") {
          x <- rexp(input$n.sample,
                    rate = input$rate)
        } else if (input$distribution == "Cauchy") {
          x <- rcauchy(input$n.sample,
                       location = input$location,
                       scale = input$scale)
        } else if (input$distribution == "Pareto") {
          x <- rpareto(input$n.sample,
                       shape = input$shape,
                       scale = input$scale)
        }
        m <- median(x)
        iqr <- IQR(x)
        return(c(m, iqr, medianInt(m, iqr, input$n.sample)))
      }
      
      # Simulate confidence intervals.
      cis <- t(apply(data.frame(x = seq(1, input$trials)),
                     MARGIN = 1,
                     FUN = get.ci))
      
      # Add column names.
      colnames(cis) <- c("m", "iqr", "ci.lb", "ci.ub")
      
      # Calculate coverage.
      cis <- cis %>% as.data.frame() %>%
        mutate(m.in.ci = (dist()$true.m > ci.lb) &
                 (dist()$true.m < ci.ub))
      
      cis
    })
    
    # Calculate coverage.
    coverage <- reactive({
      sum(conf.ints()$m.in.ci) / input$trials
    })
    #
    # Data table.
    output$data <- renderTable({
      conf.ints() %>% head(input$show.rows)
    })
    
    # Select rows text.
    output$show.rows <- renderText({
      paste("The first",
            input$show.rows,
            "results of the simulation.")
    })
    
    # Plot.
    output$plot <- renderPlot({
      # Specify distribution to be plotted for chosen distribution.
      plot.fun <- paste0("d", dist()$rname)
      
      # Specify title of plot for chosen distribution.
      title.text <- paste0(input$distribution, " distribution")
      
      # Wrangle confidence intervals into plottable points.
      cis <- conf.ints() %>%
        mutate(plot.y = seq(1, input$trials) /
                 input$trials * dist()$max.y) %>%
        gather(key = "bound",
               value = "ci",
               ci.lb,
               ci.ub)
      # Optional filter.
      if (input$hide.true == T) {
        cis <- cis %>% filter(m.in.ci == F)
      }
      
      
      # Plot.
      ggplot(data.frame(x = dist()$xlim),
             aes(x = x)) +
        # scale_fill_manual(values = c("#CC6666", "#9999CC")) +
        stat_function(
          fun = plot.fun,
          args = dist()$args,
          colour = "darkgreen",
          size = 1
        ) +
        geom_line(
          data = cis,
          aes(
            x = ci,
            y = plot.y,
            group = plot.y,
            colour = m.in.ci
          ),
          alpha = 0.5,
          size = 2
        ) +
        geom_vline(
          xintercept = dist()$true.m,
          colour = "darkblue",
          linetype = "dashed",
          size = 1
        ) +
        scale_color_brewer(palette = "Set1") +
        # scale_fill_hue(l = 80) +
        labs(title = title.text,
             x = "x",
             y = "Density")
    })
    #
    # Coverage text.
    output$coverage.text <- renderText({
      paste0("Coverage: ", round(coverage(), digits = 4) * 100, "%")
    })
    
    # True median text.
    output$median.text <- renderText({
      paste0("The true median is ", round(dist()$true.m, digit = 2), ".")
    })
    })
