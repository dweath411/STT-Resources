
x <- c(4.9, 1.8, 3.4, 6.9, 4.0)
x

lik <- function(beta) {
  likval <- -10*log(beta) + 
    log(prod(x)) - (1/(2*beta^2))*sum(x^2)
  return(-likval)
}

op <- nlminb(1, lik)
op$par


d <- read.csv("Data/SwedishMotorInsurance.csv")

head(d)

dsub <- subset(d, Payment>0)

nrow(d)
nrow(dsub)


dsub$Avg <- dsub$Payment / dsub$Claims

hist(dsub$Avg, 30)

lik2 <- function(beta) {
  likval <- -10*log(beta) + 
    sum(log(dsub$Avg)) - 
    (1/(2*beta^2))*sum(dsub$Avg^2)
  return(-likval)
}

op2 <- nlminb(1, lik2)
op2$par


lik3 <- function(theta) {
  likval <- dexp(dsub$Avg, rate=1/theta, log=TRUE)
  return(-likval)
}

op3 <- nlminb(1, lik3)
op3$par

hist(dsub$Avg, 30, freq=FALSE)
lines(density(rexp(100000, rate=1/op3$par)), col="red",
      lwd=2)

