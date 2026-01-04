# Homework 3, Question 10
library(numDeriv)

# Read data.
dd <- read.csv("Data/SwedishMotorInsurance.csv")
head(dd)
y <- dd$Payment[dd$Payment>0] / dd$Claims[dd$Payment>0]
n <- length(y)
summary(y)
n

# MLE for exponential.
lik <- function(param) {
  theta <- param[1]
  LogLik <- -log(theta) - y/theta
  return(-sum(LogLik))
}
op <- nlminb(1, lik, lower=c(0.0000001))
op$par
theta <- op$par[1]
negativeHessian <- hessian(lik,theta) # 0.00006630264
sd <- sqrt(solve(negativeHessian))
c(theta-sd*1.96 , theta+sd*1.96)
# 4965.345 5446.761


# See how good the fit is.
hist(y,100,freq=FALSE, main="Distribution fit")
lines(density(rexp(10^6,rate=1/theta)),col="blue",lwd=2)


# Try fitting a gamma distribution to get a better fit.
lik2 <- function(param) {
  shape <- param[1]
  scale <- param[2]
  LogLik <- dgamma(y, shape=shape, scale=scale, log=TRUE)
  return(-sum(LogLik))
}
op2 <- nlminb(c(1,1), lik2, lower=c(0.0000001,0.0000001))
op2$par


# See how good the fit is for the gamma.
lines(density(rgamma(10^6, shape=op2$par[1], 
                     scale=op2$par[2])),col="red",lwd=2)
legend("topright", c("Exponential","Gamma"), 
       col=c("blue","red"), lwd=c(2,2))

