# ui for comparison of variance.


# Packages ----------------------------------------------------------------
library(shinythemes)

# UI ----------------------------------------------------------------------
shinyUI(fluidPage(
  theme = shinytheme("journal"),
  
  fluidRow(
    column(12,
           titlePanel("Comparison of variance")),
    
    column(
      4,
      h1("Distribution"),
      
      
      helpText("Specify true distribution and parameters."),
      
      selectInput(
        inputId = "distribution",
        label = "Select distribution:",
        selected = "Lognormal distribution",
        choices = c(
          "Lognormal",
          "Weibull",
          "Chi-square",
          "F",
          "Exponential",
          "Cauchy",
          "Normal",
          "Pareto II"
        )
      ),
      
      # Scale parameter.
      conditionalPanel(
        condition =
          "input.distribution == 'Weibull' ||
        input.distribution == 'Pareto II' ||
        input.distribution == 'Cauchy'",
        
        # Select scale parameter.
        numericInput(
          inputId = "scale",
          label = "Select scale parameter:",
          min = 0.1,
          value = 2,
          step = 0.5
        )
      ),
      
      # Shape parameter.
      conditionalPanel(
        condition = "input.distribution == 'Weibull' ||
        input.distribution == 'Pareto II'",
        
        # Select shape parameter.
        numericInput(
          inputId = "shape",
          label = "Select shape parameter:",
          min = 0.1,
          value = 1,
          step = 0.5
        )
      ),
      
      # Mu parameter.
      conditionalPanel(
        condition = "input.distribution == 'Lognormal' ||
        input.distribution == 'Normal'",
        
        # Select mu.
        numericInput(
          inputId = "mu",
          label = "Select mu:",
          value = 1
        )
      ),
      
      # Sigma parameter.
      conditionalPanel(
        condition = "input.distribution == 'Lognormal' ||
        input.distribution == 'Normal'",
        
        # Select sigma.
        numericInput(
          inputId = "sigma",
          label = "Select sigma:",
          min = 0,
          value = 1,
          step = 0.1
        )
      ),
      
      # First df.
      conditionalPanel(
        condition = "input.distribution == 'F'",
        
        # Select df1.
        numericInput(
          inputId = "df1",
          label = "Select first degrees of freedom:",
          value = 3,
          min = 0,
          step = 1
        )
        
      ),
      
      # Second df.
      conditionalPanel(
        condition = "input.distribution == 'F'",
        
        # Select df2.
        numericInput(
          inputId = "df2",
          label = "Select second degrees of freedom:",
          value = 3,
          min = 0,
          step = 1
        )
      ),
      
      # Non-centrality parameter.
      conditionalPanel(
        condition = "input.distribution == 'F' ||
        input.distribution == 'Chi-square'",
        
        # Select ncp.
        numericInput(
          inputId = "ncp",
          label = "Select non-centrality parameter:",
          value = 0
        )
      ),
      
      # Location parameter.
      conditionalPanel(
        condition = "input.distribution == 'Cauchy'",
        
        # Select location parameter.
        numericInput(
          inputId = "location",
          label = "Select location parameter:",
          value = 1
        )
      ),
      
      # Rate parameter.
      conditionalPanel(
        condition = "input.distribution == 'Exponential'",
        
        # Select rate parameter.
        numericInput(
          inputId = "rate",
          label = "Select rate parameter:",
          value = 1,
          min = 1
        )
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'Chi-square'",
        
        # Select degrees of freedom.
        numericInput(
          inputId = "df",
          label = "Select degrees of freedom:",
          min = 0,
          value = 2,
          step = 1
        )
      ),
      
      withMathJax(),
      
      h1("Choose \\(f\\)"),
      
      selectInput(
        inputId = "approx.dist",
        label = "Select distribution to approximate the variance with",
        choices = c("Lognormal",
                    "Exponential",
                    "Pareto II"),
        selected = "Lognormal"
      )
    )
    ,
    
    column(
      4,
      h1("Results"),
      helpText(
        "The true median is rounded to 2 decimal places
        from the chosen distribution and parameters."
      ),
      textOutput("median"),
      p(
        "The first variance is approximated with the true
        distribution. The second variance is approximated with the",
         # span(textOutput("approx.dist.text")),
        " distribution."
      ),
      tableOutput("variances"),
      textOutput("var.text")
      ),
    
    column(
      4,
      h2("Visualisation of the distribution"),
      plotOutput("plot"),
      helpText(
        "The",
        span("green", style = "color:darkgreen"),
        "dashed line is the true median and the",
        span("blue", style = "color:darkblue"),
        "dotted line is the",
        textOutput("approx.dist.text"),
        " distribution, with the
        parameters estimated from the chosen distribution."
      )
      )
    )
  ))


