# Monte Carlo Interference

# Type I errors
# Normal Distribution
# H0 mean = 30 (2-sided test)
# Sample mean is 32
# Sample standard deviation is 8
# Sample size is 100
trials <- 100000
sampmean <- 32
H0mean <- 30
sampsd <- 8
n <- 100
p_value <- 0
sampse <- sampsd/sqrt(n)
simmean <- rep(0,trials)
for(i in 1:trials){
  simsamp <- H0mean + sampsd*rt(n, n - 1) 
  simmean[i] <- mean(simsamp)
}
p_value <- sum(abs(simmean - H0mean) > abs(sampmean - H0mean))/trials
p_value


trials <- 100000
sampmean <- 32
n <- 100
H0mean <- 30
simmean <- rep(0,trials)
for(i in 1:trials){
  simsamp <- rexp(n, 1/H0mean) # rpois(n, 2*2)
  simmean[i] <- mean(simsamp)
}
p_value <- sum(simmean >= sampmean)/trials
p_value

# Power curve example
# H0 mean is 30
# Standard Deviation is 4
# Looking at Effect Sizes from 0 to 1.00
# 95% Confidence Level
# Sample Size is 100
stddev <- 4
H0mean <- 30
truemean <- seq(H0mean, H0mean + stddev, by = 0.001*stddev)
n <- 50
alpha <- 1 - .95
stderror <- stddev/sqrt(n)
pwr <- rep(0,length(truemean))
for(i in 1:length(truemean)){
  meansim <- rep(0,1000)
  for(j in 1:1000){
    simsamp <- truemean[i] + stddev * rt(n, n - 1)
    meansim[j] <- mean(simsamp)
  }
  pwr[i] <- 1 - sum(pt(abs(meansim - H0mean)/stderror, n - 1) < (1-alpha)/2 + 0.5)/1000
}
powerdf <- data.frame('TrueMean' = truemean, 'Power' = pwr)
ggplot(data = powerdf, aes(x = TrueMean, y = Power)) +geom_point()
