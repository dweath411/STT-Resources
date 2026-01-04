
Surv <- function(x) {
  (100-x)^(1/2)/10
}

uu <- runif(10000)

hist(uu, 100)

X <- numeric(10000)

for (i in 1:10000) {
  X[i] <- uniroot(function(x) { Surv(x) - uu[i] }, lower=0, upper=100)$root
}

hist(X,100)

median(X)



# Per-payment variable.
n <- 10000
X <- rgamma(n, shape=5, scale=10)
hist(X, 100, xlim=c(0,200), ylim=c(0,0.05), freq=FALSE)

d <- 50
XL <- ifelse(X>d, X-d, 0)
hist(XL,100, xlim=c(0,200), ylim=c(0,0.05), freq=FALSE)
XL
Xp <- XL[XL>0]
Xp
hist(Xp, 100, xlim=c(0,200), ylim=c(0,0.05), freq=FALSE)


n <- 10000
X <- rexp(n, rate=1/10)
hist(X, 100, xlim=c(0,120), ylim=c(0,0.15), freq=FALSE)
d <-20
XL <- ifelse(X>d, X-d, 0)
XL
Xp <- XL[XL>0]
Xp
hist(Xp, 100, xlim=c(0,120), ylim=c(0,0.15), freq=FALSE)









