
d <- read.csv("Data/SwedishMotorInsurance.csv")

head(d)

lik <- function(param) {
  b0 <- param[1]
  b1 <- param[2]
  lambda <- exp(b0 + log(d$Insured)*b1)
  loglik <- dpois(d$Claim, lambda=lambda, log=TRUE)
  return(-sum(loglik))
}

op <- nlminb(c(1,1), lik)
op$par
b0 <- op$par[1]
b1 <- op$par[2]

plot( log(d$Insured) , log(d$Claims+1) ) 
mesh <- seq(1, exp(10), length.out=100)
lambda_mesh <- exp(b0 + b1 * log(mesh))
lines( log(mesh), log(lambda_mesh), type="l", lwd=4,
       col="red")

fit <- glm(Claims ~ log(Insured), data=d, 
    family=poisson(link="log"))

summary(fit)
op$par

head(d)
table(d$Zone)

lik2 <- function(param) {
  b0 <- param[1]
  b1 <- param[2]
  a2 <- param[3]
  a3 <- param[4]
  a4 <- param[5]
  a5 <- param[6]
  a6 <- param[7]
  a7 <- param[8]
  
  lambda <- exp(
    b0 + log(d$Insured) * b1 +
      a2 * (d$Zone==2) +
      a3 * (d$Zone==3) +
      a4 * (d$Zone==4) +
      a5 * (d$Zone==5) +
      a6 * (d$Zone==6) +
      a7 * (d$Zone==7))

  loglik <- dpois(d$Claims, 
                  lambda=lambda, log=TRUE)
  
  return(-sum(loglik))
  
}

op2 <- nlminb(c(1,1,1,1,1,1,1,1), lik2)
op2$par
b0 <- op2$par[1]
b1 <- op2$par[2]
a2 <- op2$par[3]
a3 <- op2$par[4]
a4 <- op2$par[5]
a5 <- op2$par[6]
a6 <- op2$par[7]
a7 <- op2$par[8]

lambda_mesh2 <-
  exp(b0 + b1 * log(d$Insured) +
        a2 * (d$Zone==2) +
        a3 * (d$Zone==3) +
        a4 * (d$Zone==4) +
        a5 * (d$Zone==5) +
        a6 * (d$Zone==6) +
        a7 * (d$Zone==7))

par(mfrow=c(2,2))
lambda_mesh1 <- exp(op2$par[1] + op2$par[2] * log(mesh))
lambda_mesh2 <- exp(op2$par[1] + op2$par[2] * log(mesh) + op2$par[3])
lambda_mesh3 <- exp(op2$par[1] + op2$par[2] * log(mesh) + op2$par[4])
lambda_mesh4 <- exp(op2$par[1] + op2$par[2] * log(mesh) + op2$par[5])
plot(log(subset(d,Zone==1)$Insured+1), log(subset(d,Zone==1)$Claims+1))
lines(log(mesh), log(lambda_mesh1), col="red", lwd=3)
plot(log(subset(d,Zone==2)$Insured+1), log(subset(d,Zone==2)$Claims+1))
lines(log(mesh), log(lambda_mesh2), col="green", lwd=3)
plot(log(subset(d,Zone==3)$Insured+1), log(subset(d,Zone==3)$Claims+1))
lines(log(mesh), log(lambda_mesh3), col="blue", lwd=3)
plot(log(subset(d,Zone==4)$Insured+1), log(subset(d,Zone==4)$Claims+1))
lines(log(mesh), log(lambda_mesh4), col="cyan", lwd=3)


d$Zone <- factor(d$Zone)
d$ZoneCat <- relevel(d$Zone, ref=1)
model2 <- glm(Claims ~ log(Insured) + ZoneCat, data=d,
              family=poisson(link="log"))
summary(model2)
op2$par
model2$coefficients

