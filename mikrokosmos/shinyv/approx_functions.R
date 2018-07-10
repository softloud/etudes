library(Hmisc)

DistFn <- 
  function(x,
           type = "q",
           distribution = "norm",
           parameters = NULL) {
    # This function lets the user specify quantile, density, probability, and
    # distribution, as well as the parameters.
    # Arguments:
    #   x: So that this can be treated as a distribution function in plots and
    #   so forth.
    #   type: Character: q, p, d - as in qnorm, pnorm, dnorm - for quantile, 
    #   probability, and density, respectively.
    #   distribution: Character string that R uses for the distribution, e.g., 
    #   norm, pois, exp, etc.
    #   parameters: Specific parameters for the chosen distribution, 
    #   such as df, shape, scale, mean, etc.
    # Returns:
    #   Depending on the type chosen, a probability, quantile, or density.
    eval(parse(
      text = paste0(
        type,
        distribution,
        "(",
        "c(",
        paste(x, collapse = ","),
        ")",
        ",",
        paste(parameters, collapse = ","),
        ")"
      )
    ))
  }

# Parameter functions for approximating variance. 

LnormPar <- function(m, iqr) {
  # This function estimates the parameters, mu and sigma, of a lognormal
  # distribution given a sample median
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
  
  # Calculate sigma - is this sigma squared or the square root of variance?
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

ExpPar <- function(m) {
  # This function takes a median and returns the rate parameter for the 
  # exponential distribution.
  # Arguments: 
  #   m: A median.
  # Returns:
  #   lambda: rate parameter for the exponential distribution.
  return(list(lambda = log(2) / m))
}

CauchyPar <- function(m, iqr) {
  # This function takes the median and the IQR and returns estimated parameters
  # for the Cauchy distribution.
  # 
  # Arguments:
  #   m: Median.
  #   iqr: Interquartile range.
  # Returns:
  #   location: 
  #   scale: 
  
  return(list(location = m, scale = iqr / 2)) 
}

NormPar <- function(m, iqr) {
  # This function takes the median and the IQR and returns estimated parameters
  # for the Cauchy distribution.
  # 
  # Arguments:
  #   m: Median.
  #   iqr: Interquartile range.
  # Returns:
  #   mean: 
  #   sd: 
  
  return(list(mean = m - qnorm(0.5) * iqr / (2 * qnorm(0.75)),
              sd = iqr / (2 * qnorm(0.75))))
}

ParetoPar <- function(m, iqr) {
  # This function takes the median and the IQR and returns estimated parameters
  # for the Pareto II distribution.
  # 
  # Arguments:
  #   m: Median.
  #   iqr: Interquartile range.
  # Returns:
  #   alpha: 
  #   lambda:
  f <- function(x, m, iqr) { # This function estimates alpha.
    (2 ^ (1 / x) - 1) / (4 ^ (1 / x) * (1 - 3 ^ (-1 / x))) -  m / iqr
  }
  alpha <- uniroot(
    f,
    interval = c(0.1, 10),
    extendInt = "yes",
    m = m,
    iqr = iqr
  )$root
  
  # Now that we have alpha, we can calculate the other parameter, lambda.
  lambda <- m / (2 ^ (1 / alpha) - 1)
  
  # Return the two estimated parameters for the Pareto.
  return(list(alpha = alpha, lambda = lambda))
}

GetSample <- function(sample.size = 100, 
                      dist
) {
  this.par <- GetPar(dist)
  this.sample <- eval(parse(text =
                              paste0(
                                "r",
                                dist,
                                "(",
                                sample.size,
                                ",",
                                paste0(this.par, collapse = ","),
                                ")"
                              )))
  return(this.sample)
}

GetSd <- function(approx.dist = "lnorm",
                  true.dist = "lnorm",
                  par = NULL
) {
  # Returns the sd approximated by sqrt(1 / (4 * f(m)^2)) where
  # f is the density function and m is the median. Option to estimate the
  # parameters of the density function from a different distribution.
  #
  # Arguments:
  #   dist: The true distribution to calculate the variance for.
  #   par: Any parameters for the true distribution.
  #   approx.dist: Which density to approximate with. If "true.dist", then
  #   the distribution is not approximated.
  # Returns:
  #   The approximate variance for the chosen distribution.
  
  #   Calculate the median and IQR.
  quartiles <- DistFn(
    c(0.25, 0.5, 0.75),
    type = "q",
    distribution = true.dist,
    parameters = par
  )
  true.median <- quartiles[2]
  true.iqr <- quartiles[3] - quartiles[1]
  
  # Calculate variance based on whether it's to be approximated using a 
  # different distribution.
  if (approx.dist == "true.dist") {
    # Calculate the approximate variance.
    approx.var <- 1 / (4 *
                         DistFn(
                           true.median,
                           type = "d",
                           distribution = true.dist,
                           parameters = par
                         ) ^ 2)
    
  } else {
    # First need to approximate the parameters of the other distribution.
    if (approx.dist == "lnorm") {
      approx.par <- LnormPar(true.median, true.iqr)
    } else if (approx.dist == "exp") {
      approx.par <- ExpPar(true.median) 
    } else if (approx.dist == "norm") {
      approx.par <- NormPar(true.median, true.iqr)
    } else if (approx.dist == "cauchy") {
      approx.par <- CauchyPar(true.median, true.iqr)
    }
    
    # Calculate the approximate variance.
    approx.var <- 1 / (4 *
                         DistFn(
                           true.median,
                           type = "d",
                           distribution = approx.dist,
                           parameters = approx.par
                         ) ^ 2)
  }
  
  # Return the approximated variance.
  return(sqrt(approx.var))
}

ApproxVar <- function(this.median,
                      this.iqr,
                      n) {
  # This function returns an approximated variance using the lognormal distribution.
  #
  # Arguments:
  #   this.median: sample median.
  #   this.iqr: sample iqr.
  #   approx.par: list of parameters of lognormal distribution.
  #   n: sample size.
  #
  #   Returns:
  #     Approximated variance.
  #
  #     Calculate parameters using sample median and iqr.
  approx.par <- LnormPar(this.median, this.iqr)
  # Return the approximated variance.
  return(1 / 4 / dlnorm(this.median, approx.par$mu, approx.par$sigma) ^ 2 / n)
}

GetCovOne <- function(dist = "lnorm",
                      n = 100,
                      trials = 1000) {
  # This function calculates coverage for the confidence interval using variance estimated with the lognormal density.
  #
  # Arguments:
  #   dist: True distribution.
  #   n: Size of sample.
  #   trials: Number of trials.
  #
  # Returns:
  #   coverage
  #
  # Get parameters for chosen distribution.
  # this.par <- eval(parse(text = paste0(dist, ".parameters")))
  this.par <- GetPar(dist)
  # Function to run one trial.
  GetCI <- function(index) {
    # Get sample.
    this.sample <- GetSample(n, dist)                              
    # Calculate sample median and iqr.
    this.median <- median(this.sample)
    this.iqr <- IQR(this.sample)
    # Calculate variance.
    this.variance <- ApproxVar(this.median,
                               this.iqr,
                               n)
    # 1 / 4 / dlnorm(this.median, approx.par$mu, approx.par$sigma) ^ 2 / n
    return(this.median + c(-1, 1) * qnorm(0.975) * sqrt(this.variance))
  }
  GetCI <- Vectorize(GetCI)
  
  # Calculate all confidence intervals.
  CIs <- GetCI(seq(1, trials)) %>% t() %>% as.data.frame()
  colnames(CIs) <- c("lower", "upper")
  
  # Calculate true median.
  true.median <- DistFn(
    0.5,
    type = "q",
    distribution = dist,
    parameters = as.character(this.par)
  )
  
  # Assess whether the true median falls within each confidence interval.
  CIs <- CIs %>% mutate(true.m.in.ci = true.median > lower &
                          true.median < upper)
  
  # Return coverage.
  return(sum(CIs$true.m.in.ci) / trials)
}

# Now extending this to the two samples.
GetCovTwo <- function(dist = "lnorm",
                      n1 = 100,
                      n2 = 100,
                      par1 = lnorm.parameters,
                      par2 = lnorm.parameters,
                      trials = 1000,
                      type = "diff") {
  # This function calculates coverage for the confidence interval using variance estimated with the lognormal density.
  #
  # Arguments:
  #   dist: True distribution.
  #   n: Size of sample.
  #   trials: Number of trials.
  #
  # Returns:
  #   coverage
  #
  # Get parameters for chosen distribution.
  # this.par <- GetPar("dist")
  # eval(parse(text = paste0(dist, ".parameters")))
  #
  # Function to run one trial.
  GetCI <- function(index) {
    # Get two samples.
    sample.one <- GetSample(n1, dist)
    sample.two <- GetSample(n2, dist)
    
    # Approximate variance of both samples using lognormal distribution.
    var.one <-
      ApproxVar(median(sample.one), IQR(sample.one), n = n1)
    var.two <-
      ApproxVar(median(sample.two), IQR(sample.two), n = n2)
    
    # Approximate the lognormal paramters from the samples:
    lnorm.par.1 <- LnormPar(median(sample.one), IQR(sample.one))
    lnorm.par.2 <- LnormPar(median(sample.two), IQR(sample.two))
    
    # Approximate the variance of the two-sample statistic.
    if (type == "diff") {
      two.var <- var.one + var.two
      m <- median(sample.one) - median(sample.two)
    } else if (type == "ratio") {
      two.var <-
        var.one / DistFn(0.5,
                         type = "q",
                         distribution = "lnorm",
                         parameters = lnorm.par.1)^2 + 
        var.two / DistFn(0.5,
                         type = "q",
                         distribution = "lnorm",
                         parameters = lnorm.par.2)^2      
      m <- log(median(sample.one) / median(sample.two))
    }
    return(
      m + c(-1, 1) * qnorm(0.975) * sqrt(two.var)
    )
  }
  GetCI <- Vectorize(GetCI)
  
  # Calculate all confidence intervals.
  CIs <- GetCI(seq(1, trials)) %>% t() %>% as.data.frame()
  colnames(CIs) <- c("lower", "upper")
  
  # Set true median value. #TODO: Is this the correct value?
  if (type == "diff") {
    true.median <- 0
  } else if (type == "ratio") {
    true.median <- 0
  }
  # Assess whether the true median falls within each confidence interval.
  CIs <- CIs %>% mutate(true.m.in.ci = true.median > lower &
                          true.median < upper)
  # Return coverage.
  return(sum(CIs$true.m.in.ci) / trials)
}

GetDistName <- function(rname, cap = F) {
  # Convert rname to the proper name of the distribution.
  this.dist <- switch(as.character(rname),
         lnorm = "lognormal",
         cauchy = "Cauchy",
         exp = "exponential",
         weibull = "Weibull",
         f = "F",
         unif = "uniform",
         beta = "beta",
         pareto2 = "Pareto II",
         norm = "normal",
         chisq = "chi-squared",
         gamma = "gamma")
  if (cap == T) {
    this.dist <- capitalize(this.dist)
  }
  return(this.dist)
}

GetVarType <- function(a.var.type) {
  # Takes a variance type of single, diff, ratio, 
  # and returns a string that finishes the sentence:
  # ...observed for  
  return(switch(a.var.type, 
                "a single sample median",
                "a difference of two medians",
                "a log ratio of two median"
                ))
}
