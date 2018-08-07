# ui for medians.

library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("journal"),
  
  fluidRow(
    column(
      12,
      titlePanel("Coverage of confidence intervals for true median 
                 using sample median estimator")
      ),
    
    column(
      3,
      h3("Choose simulation to run"),
      helpText(
        "Choose number of trials for the simulation,
        and how big a sample
        for each iteration."
      ),
      
      # Select number of trials.
      numericInput(
        inputId = "trials",
        label = "Select number of trials:",
        min = 10,
        value = 1000
      ),
      
      helpText(
        "Increasing the number of trials might affect the speed with which
        the visualisation compiles."
      ),
      
      # Select sample size.
      numericInput(
        inputId = "n.sample",
        label = "Select sample size:",
        min = 10,
        value = 1000
      ),
      
      helpText("Specify distribution and parameters."),
      
      selectInput(
        inputId = "distribution",
        label = "Select distribution to sample from:",
        selected = "Lognormal distribution",
        choices = c(
          "Lognormal",
          "Weibull",
          "Chi-square",
          "F",
          "Exponential",
          "Cauchy",
          "Normal",
          "Pareto"
        )
      ),
      
      # Scale parameter.
      conditionalPanel(
        condition =
          "input.distribution == 'Weibull' ||
        input.distribution == 'Pareto' ||
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
        input.distribution == 'Pareto'",
        
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
      
      conditionalPanel(condition = "input.distribution == 'placeholder'")
      )
    ,
    
    column(
      3,
      h2("Results of simulation"),
      verbatimTextOutput("median.text"),
      h3("Coverage"),
      verbatimTextOutput("coverage.text"),
      h3("Tabulated results"),
      dataTableOutput("show.rows"),
      numericInput(
        inputId = "show.rows",
        label = "Change number of simulation results displayed:",
        min = 0,
        value = 3
      ),
      tableOutput("data")
    ),
    
    column(
      6,
      h3("Visualisation of results"),
      helpText(
        "The",
        span("green", style = "color:green"),
        "line is the distribution under chosen parameters,
        the vertical",
        span("blue", style = "color:darkblue"),
        "dotted line is the distribution's true median, and the",
        span("blue", style = "color:blue") ,
        "and",
        span("red", style = "color:red"),
        "horizontal lines are the confidence intervals produced by the simulation."
      ),
      plotOutput("plot"),
      sliderInput(
        inputId = "xlim",
        label = "Zoom x-axis:",
        min = 0.1,
        max = 0.9,
        value = c(0.01, 0.99),
        step = 0.1,
        ticks = F
      ),
      checkboxInput(
        inputId = "hide.true",
        label = "Show only the bad confidence intervals",
        value = F
      )
    )
    )
  ))