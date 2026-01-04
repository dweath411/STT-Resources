

SampleOneClaim <- function() {
  S <- 0
  N <- rpois(1, lambda=5)
  if (N >=1) {
    for (j in 1:N) {
      S <- S + rexp(1, rate=1/10)
    }
  }
  return(S)
}

SampleOneClaim()

Ss <- numeric(1000)
for (i in 1:1000) {
  Ss[i] <- SampleOneClaim()
}
Ss
hist(Ss, 100)


