library(numDeriv)
library(plyr)

# S = sqrt(N)*delta (i.e. the non-central t-distributed effect sizes)
# X = covariate vector
# N = Study sample sizes

sim <- F

mr.mle.fixed <- function(S, X, N, method = "nlminb"){
  
  p <- ncol(X)-1
  K <- length(S)
  if(length(N) == 1) N <- rep(N, K)
  
  objective <- function(par, S, X, N) {      
      b <- par
      -sum(log(dt(S, df = N - 1, ncp = sqrt(N)*(X%*%b))))          
  }
    

  parameters.0 <- coef(lm(S/sqrt(N)~X[,-1]))
  
  if(method == "nlminb"){
    results <- nlminb(parameters.0, objective, S = S, X = X, N = N)
    hessian.matrix <- numDeriv::hessian(objective, x = parameters.0, S = S, X = X, N = N)
  }
  
  if (any(hessian.matrix == "NaN") | any(hessian.matrix == "Inf") | any(hessian.matrix == "-Inf")) {  
    list(estimates = results$par, se = NA)    
  } else {    
    list(estimates = results$par, se = sqrt(diag(solve(hessian.matrix))))    
  }  
}

mr.mle.random <- function(S, X, N, method = "nlminb"){
  
  p <- ncol(X)-1
  K <- length(S)
  if(length(N) == 1) N <- rep(N, K)
  
  objective <- function(par, S, X, N) {    
    b <- par[1:(p+1)]
    tau <- par[p+2]
    -sum(log(1/sqrt(1 + N*tau^2)*dt(S/sqrt(1 + N*tau^2), df = N - 1, ncp = sqrt(N)*(X%*%b)/sqrt(1 + N*tau^2))))    
  }
  
  parameters.0 <- c(coef(lm(S/sqrt(N)~X[,-1])), 0.5)
  
  if(method == "nlminb"){
    results <- nlminb(parameters.0, objective, S = S, X = X, N = N)
    hessian.matrix <- numDeriv::hessian(objective, x = parameters.0, S = S, X = X, N = N)
  }
  
  if (any(hessian.matrix == "NaN") | any(hessian.matrix == "Inf") | any(hessian.matrix == "-Inf")) {  
    list(estimates = results$par, se = NA)    
  } else {    
    list(estimates = results$par, se = sqrt(diag(solve(hessian.matrix))))    
  }  
}


# Random effects model simulation

K <- 10
Ns <- seq(5, 50, by = 5)
N <- sample(Ns, K, replace = TRUE)

beta.0 <- 0
beta.1.vector <- seq(-2,2,by=0.2)
sd <- 1
tau.vector <- seq(0,1,by=0.1)

trials <- 1000  	# 1000

results.random <- matrix(0, length(beta.1.vector), length(tau.vector))
results.random.t <- results.random 
didnt.work <- matrix(0, length(beta.1.vector), length(tau.vector))

results.fixed <- matrix(0, length(beta.1.vector), length(tau.vector))
results.fixed.t <- results.fixed
didnt.work.fixed <- matrix(0, length(beta.1.vector), length(tau.vector))

results.mixed <- matrix(0, length(beta.1.vector), length(tau.vector))
results.mixed.t <- results.mixed
didnt.work.mixed <- matrix(0, length(beta.1.vector), length(tau.vector))

row <- 1

for(beta.1 in beta.1.vector) {

	column <- 1

  	for(tau in tau.vector) {

		cat(row, column, dim(results.random), "\n")
		delta.star <- rep(0, K)

		x <- runif(K, 0, 2)
		X <- cbind(rep(1, K), x)

		beta.1.ci.random <- matrix(0, trials, 2)
		beta.1.ci.random.t <- matrix(0, trials, 2)
		beta.1.ci.fixed <- matrix(0, trials, 2)
		beta.1.ci.fixed.t <- matrix(0, trials, 2)
		beta.1.ci.mixed <- matrix(0, trials, 2)
		beta.1.ci.mixed.t <- matrix(0, trials, 2)

		for(i in 1:trials) {
      cat("beta.1 =",beta.1, " tau =", tau, " i =", i, "\n")
			# Randomly generate the data

			for(k in 1:K) {
				Delta.k <- rnorm(1, beta.0+beta.1*x[k], tau)
				data <- rnorm(N[k], Delta.k, sd)
				m <- mean(data)
				s <- sd(data)
				delta.star[k] <- m/s
			}

			mr.mle.random.results <- mr.mle.random(sqrt(N)*delta.star, X = X, N = N)
      mr.mle.mixed.results <- mr.mle.random.results
      cutoff <- sum(1/N)/K
			mr.mle.fixed.results <- mr.mle.fixed(sqrt(N)*delta.star, X = X, N = N)
      if (mr.mle.mixed.results$estimates[3] <= cutoff)  mr.mle.mixed.results <- mr.mle.fixed.results
      
      t.alpha.2 <- qt(0.975, df = K - 1)
      
			if (is.na(mr.mle.random.results$se)) { 
				beta.1.ci.random[i, ] <- c(0, 0)
				beta.1.ci.random.t[i, ] <- c(0, 0)
				didnt.work[row, column] <- didnt.work[row, column] + 1

			} else{  
			  beta.1.ci.random[i, ] <- mr.mle.random.results$estimates[2] + c(-1, 1)*1.96*mr.mle.random.results$se[2]
			  beta.1.ci.random.t[i, ] <- mr.mle.random.results$estimates[2] + c(-1, 1)*t.alpha.2*mr.mle.random.results$se[2]
			}  
			if (is.na(mr.mle.fixed.results$se)) { 

				beta.1.ci.fixed[i, ] <- c(0, 0)				
				beta.1.ci.fixed.t[i, ] <- c(0, 0)
				didnt.work.fixed[row, column] <- didnt.work.fixed[row, column] + 1

			} else{ 
        beta.1.ci.fixed[i, ] <- mr.mle.fixed.results$estimates[2] + c(-1, 1)*1.96*mr.mle.fixed.results$se[2]
        beta.1.ci.fixed.t[i, ] <- mr.mle.fixed.results$estimates[2] + c(-1, 1)*t.alpha.2*mr.mle.fixed.results$se[2]
			}
      if (is.na(mr.mle.fixed.results$se)) { 
        
        beta.1.ci.mixed[i, ] <- c(0, 0)
        beta.1.ci.mixed.t[i, ] <- c(0, 0)
        didnt.work.mixed[row, column] <- didnt.work.mixed[row, column] + 1
        
      } else{
        beta.1.ci.mixed[i, ] <- mr.mle.mixed.results$estimates[2] + c(-1, 1)*1.96*mr.mle.mixed.results$se[2]
        beta.1.ci.mixed.t[i, ] <- mr.mle.mixed.results$estimates[2] + c(-1, 1)*t.alpha.2*mr.mle.mixed.results$se[2]
      }
		}

		# in.0 <- sapply(1:trials, function(i) beta.0 > beta.0.ci[i,1] & beta.0 < beta.0.ci[i,2])
		in.1 <- sapply(1:trials, function(i) beta.1 > beta.1.ci.random[i,1] & beta.1 < beta.1.ci.random[i,2])
		results.random[row, column] <- mean(in.1, na.rm = TRUE)

    in.1 <- sapply(1:trials, function(i) beta.1 > beta.1.ci.random.t[i,1] & beta.1 < beta.1.ci.random.t[i,2])
    results.random.t[row, column] <- mean(in.1, na.rm = TRUE)  
      
		in.1 <- sapply(1:trials, function(i) beta.1 > beta.1.ci.fixed[i,1] & beta.1 < beta.1.ci.fixed[i,2])
		results.fixed[row, column] <- mean(in.1, na.rm = TRUE)
      
    in.1 <- sapply(1:trials, function(i) beta.1 > beta.1.ci.fixed.t[i,1] & beta.1 < beta.1.ci.fixed.t[i,2])
    results.fixed.t[row, column] <- mean(in.1, na.rm = TRUE)  

		in.1 <- sapply(1:trials, function(i) beta.1 > beta.1.ci.mixed[i,1] & beta.1 < beta.1.ci.mixed[i,2])
		results.mixed[row, column] <- mean(in.1, na.rm = TRUE)
      
    in.1 <- sapply(1:trials, function(i) beta.1 > beta.1.ci.mixed.t[i,1] & beta.1 < beta.1.ci.mixed.t[i,2])
    results.mixed.t[row, column] <- mean(in.1, na.rm = TRUE)
    
		column <- column + 1

	}

	row <- row + 1

}



results.random
results.fixed
results.mixed
zlim <- 100*c(min(results.random,results.fixed, results.mixed), 1)


library(lattice)

windows(width = 13, height = 4, pointsize = 16)

c1 <- contourplot(100*results.random, row.values = beta.1.vector, column.values = tau.vector, labels = FALSE, cex = 0.01,
                  at = c(85,seq(91,98,1),100), colorkey = FALSE, xlab = expression(beta[1]), ylab = expression(tau),
                  col.regions=c("black", "darkred", "red", "orange", "yellow", "green", "skyblue", "deepskyblue", "blue"),
                  region = TRUE, pretty = TRUE, main = "A: Random effect MLE coverage %", xlim = range(beta.1.vector), 
                  ylim = range(tau.vector), aspect = "fill")
print(c1, split = c(1, 1, 3, 1), position = c(0, -0.03, 0.9, 1), more = TRUE, cex  = 0.1, col.key = FALSE)

c2 <- contourplot(100*results.fixed, row.values = beta.1.vector, column.values = tau.vector, labels = FALSE, cex = 0.01,
                  at = c(85,seq(91,98,1),100), colorkey = FALSE, xlab = expression(beta[1]), ylab = expression(delta),
                  col.regions=c("black", "darkred", "red", "orange", "yellow", "green", "skyblue", "deepskyblue", "blue"),
                  region = TRUE, pretty = TRUE, main = "B: Fixed effect MLE coverage %", xlim = range(beta.1.vector), 
                  ylim = range(tau.vector), aspect = "fill")
print(c2, split = c(2, 1, 3, 1), position = c(-0.03, -0.03, 0.9, 1), more = TRUE, cex  = 0.1, col.key = FALSE)

c3 <- contourplot(100*results.mixed, row.values = beta.1.vector, column.values = tau.vector, labels = FALSE, cex = 0.01,
                  at = c(85,seq(91,98,1),100), colorkey = TRUE, xlab = expression(beta[1]), ylab = expression(delta),
                  col.regions=c("black", "darkred", "red", "orange", "yellow", "green", "skyblue", "deepskyblue", "blue"),
                  region = TRUE, pretty = TRUE, main = "C: Random/Fixed effect MLE coverage %", xlim = range(beta.1.vector), 
                  ylim = range(tau.vector), mex = 1.2, cex =2, aspect = "fill")
print(c3, split = c(3, 1, 3, 1), position = c(-0.1, -0.03, 0.92, 1), more = FALSE, cex  = 0.1, col.key = FALSE)

  
	windows(width = 13, height = 4, pointsize = 16)
	
	c1 <- contourplot(100*results.random.t, row.values = beta.1.vector, column.values = tau.vector, labels = FALSE, cex = 0.01,
	                  at = c(85,seq(91,98,1),100), colorkey = FALSE, xlab = expression(beta[1]), ylab = expression(tau),
	                  col.regions=c("black", "darkred", "red", "orange", "yellow", "green", "skyblue", "deepskyblue", "blue"),
	                  region = TRUE, pretty = TRUE, main = "A: Random effect MLE(t) coverage %", xlim = range(beta.1.vector), 
	                  ylim = range(tau.vector), aspect = "fill")
	print(c1, split = c(1, 1, 3, 1), position = c(0, -0.03, 0.9, 1), more = TRUE, cex  = 0.1, col.key = FALSE)
	
	c2 <- contourplot(100*results.fixed.t, row.values = beta.1.vector, column.values = tau.vector, labels = FALSE, cex = 0.01,
	                  at = c(85,seq(91,98,1),100), colorkey = FALSE, xlab = expression(beta[1]), ylab = expression(delta),
	                  col.regions=c("black", "darkred", "red", "orange", "yellow", "green", "skyblue", "deepskyblue", "blue"),
	                  region = TRUE, pretty = TRUE, main = "B: Fixed effect MLE(t) coverage %", xlim = range(beta.1.vector), 
	                  ylim = range(tau.vector), aspect = "fill")
	print(c2, split = c(2, 1, 3, 1), position = c(-0.03, -0.03, 0.9, 1), more = TRUE, cex  = 0.1, col.key = FALSE)
	
	c3 <- contourplot(100*results.mixed.t, row.values = beta.1.vector, column.values = tau.vector, labels = FALSE, cex = 0.01,
	                  at = c(85,seq(91,98,1),100), colorkey = TRUE, xlab = expression(beta[1]), ylab = expression(delta),
	                  col.regions=c("black", "darkred", "red", "orange", "yellow", "green", "skyblue", "deepskyblue", "blue"),
	                  region = TRUE, pretty = TRUE, main = "C: Random/Fixed effect MLE(t) coverage %", xlim = range(beta.1.vector), 
	                  ylim = range(tau.vector), mex = 1.2, cex =2, aspect = "fill")
	print(c3, split = c(3, 1, 3, 1), position = c(-0.1, -0.03, 0.92, 1), more = FALSE, cex  = 0.1, col.key = FALSE)
	