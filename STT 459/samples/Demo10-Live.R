
d <- read.csv("Data/SwedishMotorInsurance.csv")

head(d)

plot(d$Insured, d$Claims)
plot(log(d$Insured), log(d$Claims+1))

lik <- function(param) {
  b0 <- param[1]
  b1 <- param[2]
  lambda <- exp(b0 + b1 * log(d$Insured))
  loglik <- dpois(d$Claims, lambda=lambda, log=TRUE)
  return(-sum(loglik))
}

myobj <- nlminb(c(2, 1), lik)
myobj$par

pred <- exp( myobj$par[1] + myobj$par[2] * log(d$Insured))

plot(log(d$Insured), log(d$Claims+1))
mesh <- seq(1, exp(10), length.out=100)
pred2 <- exp( myobj$par[1] + myobj$par[2] * log(mesh))
lines(log(mesh), log(pred2), col="red", lwd=3)
