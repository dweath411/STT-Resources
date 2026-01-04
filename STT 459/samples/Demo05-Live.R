

set.seed(1)
n <- 1000
mu <- 3
sd <- 1
y <- rnorm(n, mean=mu, sd=sd)
hist(y, 50)
x <- exp(y)
hist(x, 500)

model <- lm(log(x) ~ 1)
model$coefficients

lik <- function(param) {
  mu <- param[1]
  sigma <- param[2]
  likelihood <- exp(-(log(x)-mu)^2/(2*sigma^2)) / 
    (sigma*sqrt(2*pi)*x) 
  return(-sum(log(likelihood)))
}

op <- nlminb(c(1, 1), lik)
op$par


d <- read.csv("Data/SwedishMotorInsurance.csv")

head(d)

dsub <- subset(d, Payment>0)

dsub$Avg <- dsub$Payment / dsub$Claims

likexp <- function(theta) {
  loglik <- dexp(dsub$Avg, rate=1/theta, log=TRUE)
  return(-sum(loglik))
}

opexp <- nlminb(1, likexp)
opexp$par

hist(dsub$Avg, 40, freq=FALSE)

lines(density(rexp(50000, rate=1/opexp$par)),
      col="blue", lwd=2)


liklnorm <- function(param) {
  mu <- param[1]
  sigma <- param[2]
  likelihood <- exp(-(log(dsub$Avg)-mu)^2/(2*sigma^2)) / 
    (sigma*sqrt(2*pi)*dsub$Avg)
  return(-sum(log(likelihood)))
}

oplnorm <- nlminb(c(1,1), liklnorm,
                  lower=c(-Inf, 0.00000000001))
oplnorm$par

lines(density(exp(rnorm(50000, mean=oplnorm$par[1],
                        sd=oplnorm$par[2]))),
      col="red", lwd=2)




