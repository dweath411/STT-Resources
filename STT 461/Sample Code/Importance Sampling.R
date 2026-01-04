# Importance Sampling Example
#this is on homework

# Integrand is e^(-x)/(1+x^2)
# bounds are 0 and 1

n <- 1000
f_int <- function(x) {
  result <- exp(-x)/(1+x^2)
  return(result)
}
# Standard Monte Carlo
samp <- runif(n)
MC_est <- mean(f_int(samp))
MC_est

# MC with Importance Sampling
# Importance function g(x) = e^-x/(1 - e^-1)
# Importance function sampled with inverse cdf method
u <- runif(n)
samp <- log(1 - u * (1 - exp(-1)))
f_g <- f_int(samp) / (exp(-samp) / (1 - exp(-1)))
mean(f_g)

# Second example with infinite domain and sampling with rnorm
# integrand is e^(-x^3) from 0 to infinity
f_infinite <- function(x){
  result <- exp(-1*x^3)
  return(result)
}
n <- 1000
samp <- abs(rnorm(n))
f_g <- f_infinite(samp)/(2*dnorm(samp))
mean(f_g)

# Truncated Monte Carlo
mean(f_infinite(runif(n,0,10))) * 10