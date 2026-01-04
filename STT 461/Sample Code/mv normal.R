library(mvtnorm)
min <- -3
max <- 3
var_x <- 1.5
var_y <- 1.5
cor_xy <- 0.25
Sigma <- cbind(c(var_x,cor_xy*sqrt(var_x)*sqrt(var_y)),c(cor_xy*sqrt(var_x)*sqrt(var_y),var_y))
x <- seq(min, max, by = 0.1)
y <- seq(min, max, by = 0.1)

ggplot(co_df, aes(x = x, y = y, z = z)) + geom_contour()
df.grad <- expand.grid(x = seq(-4,4, by = 0.1),y = seq(-4,4, by = 0.1))
dens <- cbind(df.grad, z = dmvnorm(df.grad,c(0,0), Sigma))
ggplot(dens, aes(x = x, y = y, z = z)) + geom_contour_filled()

library(MASS)
plot(mvrnorm(1000, mu = c(0,0), Sigma))