
lambda <- 100
theta <- 1277

set.seed(1)

B <- 10000

N <- rpois(B, lambda=lambda)

S <- numeric(B)

for (b in 1:B) {
  if(N[b]>0) {
    S[b] <- S[b] + sum(rexp(N[b], rate=1/theta))
  }
}

hist(S, 100)
mean(S)


# Apply deductible, upper-limit, coinsurance.
d <- 100
u <- 2000
alpha <- 0.90

g <- function(x, d, u, c) {
  dd <- ifelse(x>d, d, 0)
  ifelse(x>u, c*(u-dd), c*(x-dd))
}

g(5000, d, u, alpha)

Sg <- numeric(B)

for (b in 1:B) {
  if (N[b]>0) {
    X <- rexp(N[b], rate=1/theta)
    Xg <- g(X,d,u,alpha)
    Sg[b] <- Sg[b] + sum(Xg)
  }
}

hist(Sg,100)
mean(Sg)

mean(S)

quantile(Sg, 0.95)

