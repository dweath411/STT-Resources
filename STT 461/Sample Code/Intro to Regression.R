# setting up normal equations
A = cbind(1,real_Est$X2.house.age)
b = real_Est$Y.house.price.of.unit.area
x = solve(t(A) %*% A) %*% t(A) %*% b
resid = b - x[1] - x[2]*real_Est$X2.house.age  
resid_naive = real_Est$Y.house.price.of.unit.area - mean(real_Est$Y.house.price.of.unit.area)
sqrt(mean(resid_naive^2))
sqrt(mean(resid^2))
x

#comparing to lm
real_mod <- (lm(data = real_Est, real_Est$Y.house.price.of.unit.area ~ real_Est$X2.house.age))
summary(real_mod)

# E(price | age = 10)
real_mod$coefficients[1]+10*real_mod$coefficients[2]

# 95% CI for value
real_mod$coefficients[1]+10*real_mod$coefficients[2] + sd(real_mod$residuals)*qt(c(0.025,0.975),length(real_mod$residuals)-2)

# EDA with ggpairs
ggpairs(iris, mapping = ggplot2::aes(color=Species))