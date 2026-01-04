
library(rstan)

set.seed(1)
y <- c(2,0,0)
data <- list(n = length(y),
             y = y)
data
fit <- stan(file="Demo17-Poisson.stan", 
            data=data, seed=459, iter=2000,
            control = list(adapt_delta = 0.8, stepsize = 0.5, 
                           max_treedepth = 15))

ms <- extract(fit)

str(ms)

hist(ms$lambda,50, xlim=c(0,10), ylim=c(0,2),
     freq=FALSE)

mean(ms$lambda)

4/3 # theoretical

hist(ms$ypred,100)

mean(ms$ypred)

