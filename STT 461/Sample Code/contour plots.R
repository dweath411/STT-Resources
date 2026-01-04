library(ggplot2)

# with contour
fn <- function(x,y){
#  z = x^2 + y^2 - -2*x + 3*y+ sin(x^2+y^2)
  z = 2*x^2 +y^4 - 4*x*y + .5*sin(x*y)
    return(z)
}
x <- seq(-3,3, by = 0.1)
y <- seq(-3,3, by = 0.1)
z <- matrix(nrow = length(y),ncol = length(x))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[j,i] = fn(x[i],y[j])
  }
}
contour(x,y,z, nlevels = 8)

# ggplot
contour_data <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    contour_data[nrow(contour_data) + 1,] = c(x=x[i],y=y[j],z=fn(x[i],y[j]))
  }
}

ggplot(data = contour_data, aes(x = x, y = y)) + geom_contour(aes(z = z))


# ggplot with more options
ggplot(data = contour_data, aes(x = x, y = y)) + geom_contour(aes(z = z, color = stat(level)))

ggplot(data = contour_data, aes(x = x, y = y)) + geom_contour(aes(z = z, color = factor(..level..)))
