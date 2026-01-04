library(numDeriv)

# Read in the data.
d <- read.csv("SwedishMotorInsurance.csv")
head(d)

# negative log-likelihood function.
loglik <- function(param) { 
  b0 <- param[1]
  b1 <- param[2]
  lambda <- exp(b0 + log(d$Insured) * b1)
  loglik <- dpois(d$Claims, lambda=lambda, log=TRUE)
  return(-sum(loglik))
}

# Minimize the negative log-likelihood
op <- nlminb(c(1,1), loglik)
op$par

# Estimate the standard errors.
I <- hessian(loglik, op$par) 
sd <- sqrt(diag(solve(I)))

# 95% confidence intervals.
c(op$par[1]-sd[1]*1.96 , op$par[1]+sd[1]*1.96)
c(op$par[2]-sd[2]*1.96 , op$par[2]+sd[2]*1.96)

# You can obtain the same result using the GLM routine.
model <- glm(Claims ~ log(Insured), data=d, family=poisson(link="log"))
summary(model)
c(model$coefficients[1]-sqrt(vcov(model)[1,1])*1.96,
  model$coefficients[1]+sqrt(vcov(model)[1,1])*1.96)
c(model$coefficients[2]-sqrt(vcov(model)[2,2])*1.96,
  model$coefficients[2]+sqrt(vcov(model)[2,2])*1.96)

