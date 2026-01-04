# regular bootstrap 
mpgmean <- rep(0,10000)
mpgmedian <- rep(0,10000)
mpg90 <- rep(0,10000)
mpgsd <- rep(0,10000)
for(i in 1:10000){
  mpgsamp <- sample(mtcars$mpg, length(mtcars$mpg), replace = TRUE)
  mpgmean[i] <- mean(mpgsamp)
  mpgmedian[i] <- median(mpgsamp)
  mpg90[i] <- quantile(mpgsamp, 0.9)
  mpgsd[i] <- sd(mpgsamp)
}
quantile(mpgmean, c(0.025, 0.975))
quantile(mpgmedian, c(0.025, 0.975))
quantile(mpg90, c(0.025, 0.975))
quantile(mpgsd, c(0.025, 0.975))
hist(mpgsd, breaks = 30)

# Bayesian bootstrap 
library(DirichletReg)
  weight <- rep(0,length(mtcars$mpg))
mpgmean <- rep(0,10000)
mpgmedian <- rep(0,10000)
mpg90 <- rep(0,10000)
mpgsd <- rep(0,10000)
for(i in 1:10000){
  weight <- rdirichlet(1, rep(1,length(mtcars$mpg)))
  mpgsamp <- sample(mtcars$mpg, length(mtcars$mpg), prob = weight, replace = TRUE)
  mpgmean[i] <- mean(mpgsamp)
  mpgmedian[i] <- median(mpgsamp)
  mpg90[i] <- quantile(mpgsamp, 0.9)
  mpgsd[i] <- sd(mpgsamp)
}
quantile(mpgmean, c(0.025, 0.975))
quantile(mpgmedian, c(0.025, 0.975))
quantile(mpg90, c(0.025, 0.975))
quantile(mpgsd, c(0.025, 0.975))
