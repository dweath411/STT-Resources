
n <- 1000

mu <- 3
sd <- 1
Y <- rnorm(n, mean=mu, sd=sd)
hist(Y,100)
X <- exp(Y)
hist(X, 100)

lik <- function(param) {
  mu <- param[1]
  sig <- param[2]
  likelihood <- exp(-(log(X)-mu)^2/(2*sig^2)) / (sig*sqrt(2*pi)*X)
  return(-sum(log(likelihood)))
}

op <- nlminb(c(0,0.5), lik, lower=c(-Inf, 0.00000000001))

op$par

library(numDeriv)

Info <- hessian(lik, op$par)

InfoInv <- solve(Info)

diagElm <- diag(InfoInv)

StdErr <- sqrt(diagElm)

StdErr

c( op$par[1] - StdErr[1] * 1.96,
   op$par[1] + StdErr[1] * 1.96)
