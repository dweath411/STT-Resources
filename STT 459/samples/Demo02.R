

X <- rgamma(1000, shape=2, scale=100)

hist(X, 100, freq=FALSE)

lines(density(rgamma(1000000,shape=2,scale=100)),
      col="red",lwd=2)
