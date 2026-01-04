

x <- c(0, 2, 1, 0, 3, 0, 1)
hist(x)

mean(x)

lik <- function(lambda) {
  loglik <- dpois(x, lambda, log=TRUE)
  -sum(loglik)
}

op <- nlminb(5, lik)
op$par


d <- read.csv("Data/SwedishMotorInsurance.csv")
x <- d$Claims
lik <- function(lambda) {
  loglik <- dpois(x, lambda, log=TRUE)
  -sum(loglik)
}
op <- nlminb(15, lik)
op$par


library(numDeriv)

sqrt(solve(hessian(lik, op$par)))
op$par



