# Single-variable optimization

f <- function(x) x^2 - 4 * x - 2

plot(seq(-10,10, by = 0.1), f(seq(-10,10, by = 0.1)))
optimize(f, c(-10,10),maximum=TRUE)

fa <- function(x) abs(x)
optimize(fa, c(-2,2))

#3-d plot of z 
fm <- function(x) x[1]^2 + x[2]^2 - 3 * x[1] + sin(2*x[1]) + exp(-0.4*x[1]^2*x[2]^2)
x_min <- -5
x_max <- 5
y_min <- -5
y_max <- 5
x <- seq(-5, 5, by = 0.05)
y <- seq(-5, 5, by = 0.05)
z <- matrix(nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j] <- fm(c(x[i],y[j]))
  }
}
contour(x,y,z)

# Multi-variable optimization
optim(c(0,0),fm)

f3 <- function(x) x[1]^2 -3*x[1] + x[2]^4  - 3*x[2] + x[3]^2 + 10*x[3] + cos(x[1]*x[2]*x[3])
optim(c(23,-23,15), f3)

f2 <- function(x=0,y=0) x^2+y^2
optim(c(10,10),f2)