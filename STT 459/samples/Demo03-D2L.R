# Basic algebra. Variables. Loops.
# CLT demo.

# Simulate ten data points from the exponential distribution.
set.seed(1)
x <- numeric(10)
for (i in 1:10) {
  x[i] <- round(rexp(1,rate=0.001))
}
x

# Do the same thing using vectors.
y <- round(rexp(10,rate=0.001))
y

# Using a loop
s <- numeric(10)
for (i in 1:10) {
  s[i] <- x[i] + y[i]
}
s

# is equivalent to
s <- x + y
s

# Vector addition, subtraction, multiplication, dot product
x + y
x - y
x * y
x %*% y


# CLT demo
rm(list=ls())
set.seed(1)
B <- 10000
xbars <- numeric(B)
ns <- c(1,2,5,10,50)
plot(NULL,xlim=c(-10,100), ylim=c(0,0.3), 
     xlab="x", ylab="Density", main="Sampling distributions of gamma sample mean")
for (i in 1:length(ns)) {
  n <- ns[i]
  for (b in 1:B) {
    xbars[b] <- mean(rgamma(n,shape=2,scale=10))
  }
  lines(density(xbars), col=i)
}
legend("topright",c("n = 1","n = 2","n = 5","n = 10","n = 50"), col=1:5, lwd=1)



