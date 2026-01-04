
rm(list=ls())

######################################################
# Fit an exponential distribution.
######################################################
d <- read.csv("Data/SOA_CaseStudyChallenge_Data.csv")
head(d)
dsub <- subset(d, FreqBodilyInjury>0)
dsub$yAvg <- dsub$ClaimsBodilyInjury / dsub$FreqBodilyInjury
n <- nrow(dsub)
y <- dsub$yAvg

# find the cdfs
theta <- mean(y)
Fx_star <- ecdf(y)(y)
Fx <- pexp(y,rate=1/theta)

# D-plot.
plot(y, Fx - Fx_star)
abline(h=0, col="red")

# PP-plot
plot(Fx, Fx_star)
abline(0,1, col="red")

# Kolmogorov-Smirnov test.
max(abs(Fx_star - Fx))
ks.test(Fx_star, Fx, alternative="greater")



######################################################
# Fit a Gamma distribution.
######################################################
lik <- function(param) {
  shape <- param[1]
  scale <- param[2]
  loglik <- dgamma(y, shape=shape, scale=scale, log=TRUE)
  return(-sum(loglik))
}

op <- nlminb(c(10,1), lik, lower=c(0.0001, 0.0001))
op$par

hist(pgamma(y, shape=op$par[1], scale=op$par[2]), 100)

Fx2 <- pgamma(y, shape=op$par[1], scale=op$par[2])

# D-plot
plot(y, Fx2 - Fx_star)
abline(h=0, col="red")

# PP-plot
plot(Fx2, Fx_star)
abline(0,1, col="red")

# Kolmogorov-Smirnov test.
max(abs(Fx_star - Fx2))
ks.test(Fx_star, Fx2, alternative="greater")



######################################################
# Fit a Gb2 distribution.
######################################################
library(GB2)
likGB2 <- function(param) {
  shape1 <- param[1]
  scale <- param[2]
  shape2 <- param[3]
  shape3 <- param[4]
  loglik <- log(dgb2(y, shape1, scale, shape2, shape3))
  return(-sum(loglik))
}

opGB2 <- nlminb(c(1,1,1,1), likGB2, lower=c(0.0001, 0.0001))
opGB2$par

hist(pgb2(y, shape1=opGB2$par[1], scale=opGB2$par[2], 
          shape2=opGB2$par[3], shape3=opGB2$par[4]), 100)

Fx3 <- pgb2(y, shape1=opGB2$par[1], scale=opGB2$par[2], 
              shape2=opGB2$par[3], shape3=opGB2$par[4])

# D-plot
plot(y, Fx3 - Fx_star)
abline(h=0, col="red")

# PP-plot
plot(Fx3, Fx_star)
abline(0,1, col="red")

# Kolmogorov-Smirnov test.
max(abs(Fx_star - Fx3))
ks.test(Fx_star, Fx3, alternative="greater")

