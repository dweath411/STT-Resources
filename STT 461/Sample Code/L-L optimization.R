# log-like for exponential
samp <- c(1, 4, 2, 6, 4, 8)
f <- function (lamb) length(samp)*log(lamb)-1*lamb*sum(samp)
optimize(f,c(0,3),maximum=TRUE)

# log-like for binomial (estimate p only, n = 15)
samp <- c(3,4,1,5,6,6,7,1,0,10,5,6,5,13,12,12,6)
n <- 15
g <- function(p) sum(log(dbinom(samp,n,p)))
optimize(g,c(0,1),maximum = TRUE)

# log-like for normal (estimate sigma only, mu = 3.6)
samp <- c(3,4,5,3.4,4.5)
mu <- 3.6
h <- function(sig) sum(log(dnorm(samp,mu,sig)))
optimize(h,c(0,4), maximum=TRUE)

# log-like for normal (estimate both mu and sigma)
samp <- c(0.5,1.5, 1,4,5,3.4,4.5, 6, 7, 4.5, 4.3, 4.2, 4.6, 4.8)
log_lik <- function(para) -1*sum(log(dnorm(samp,para[1],para[2])))
optim(c(5, 2), log_lik, samp)

# log-like for normal iris sepal length (estimate both mu and sigma, using dnorm)
samp <- iris$Sepal.Length
log_lik <- function(para) -1*sum(log(dnorm(samp, para[1],para[2])))
optim(c(5, 2), log_lik, samp)

log_bs <- function(para) -1 * sum(log(dbeta(bs,para[1],para[2])))
optim(c(5,5), log_bs)