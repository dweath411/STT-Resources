# train-test split example
# pizza
split_pct <- 0.7
n <- length(pizza$cal)*split_pct # train size
row_samp <- sample(1:length(pizza$cal), n, replace = FALSE)
train <- pizza[row_samp,]
test <- pizza[-row_samp,]
pizza_train_mod <- lm(data = train, cal ~ fat + mois)
test_pred <- predict(pizza_train_mod,test)
test_error <- test$cal - test_pred
rmse_train <- sqrt(mean(pizza_train_mod$residuals^2))
rmse_test <- sqrt(mean(test_error^2))
rmse_train
rmse_test

# train-test split example
# mtcars
split_pct <- 0.7
n <- length(mtcars$mpg)*split_pct # train size
row_samp <- sample(1:length(mtcars$mpg), n, replace = FALSE)
train <- mtcars[row_samp,]
test <- mtcars[-row_samp,]
mtcars_train_mod <- lm(data = train, mpg ~ cyl + disp + hp +drat+wt+qsec+vs+am+gear+carb)
test_pred <- predict(mtcars_train_mod,test)
test_error <- test$mpg - test_pred
rmse_train <- sqrt(mean(mtcars_train_mod$residuals^2))
rmse_test <- sqrt(mean(test_error^2))
rmse_train
rmse_test

# Bootstrap regression
library(MASS)
library(DirichletReg)
diam_mod <- lm(data = diamonds, price ~ carat)
summary(diam_mod)
coeff1 <- rep(0, 100)
coeff2 <- rep(0, 100)
weight <- rep(0,length(diamonds$carat))
for(i in 1:100){
  n <- length(diamonds$carat)
  # weight <- rdirichlet(1, rep(1,n))
  row_samp <- sample(1:n, n, replace = TRUE)
  diam_samp <- diamonds[row_samp,]
  temp_mod <- lm(data = diam_samp, price ~ carat)
  coeff1[i] <- temp_mod$coefficients[1]
  coeff2[i] <- temp_mod$coefficients[2]
}
quantile(coeff1, c(0.025, 0.975))
quantile(coeff2, c(0.025, 0.975))
plot(seq(0, 3, by = 0.05), coeff1[1]+seq(0, 3, by = 0.05)*coeff2[1], type = 'l')
for(i in 2:100){
  lines(seq(0, 3, by = 0.05), coeff1[i]+seq(0, 3, by = 0.05)*coeff2[i])
}

diam_mod <- lm(data = iris, Petal.Length ~ Petal.Width)
summary(diam_mod)

# bootstrap regression with iris
coeff1 <- rep(0, 1000)
coeff2 <- rep(0, 1000)
n <- length(iris$sepal.length)
weight <- rep(0,n)
for(i in 1:1000){
  weight <- rdirichlet(1, rep(1,n))
  row_samp <- sample(1:n, n, prob = weight, replace = TRUE)
  iris_samp <- iris[row_samp,]
  temp_mod <- lm(data = iris_samp, petal.length ~ petal.width)
  coeff1[i] <- temp_mod$coefficients[1]
  coeff2[i] <- temp_mod$coefficients[2]
}
quantile(coeff1, c(0.025, 0.975))
quantile(coeff2, c(0.025, 0.975))
plot(seq(0, 3, by = 0.05), coeff1[1]+seq(0, 3, by = 0.05)*coeff2[1], type = 'l')
for(i in 2:100){
  lines(seq(0, 3, by = 0.05), coeff1[i]+seq(0, 3, by = 0.05)*coeff2[i])
}

# Sensitivity Analysis
pizza_mod <- lm(data = pizza, cal ~ mois + fat + sodium)
sum_std <- sd(pizza$mois)*abs(pizza_mod$coefficients[2]) + sd(pizza$fat)*abs(pizza_mod$coefficients[3]) + sd(pizza$sodium)*abs(pizza_mod$coefficients[4])
mois_sens <- sd(pizza$mois)*pizza_mod$coefficients[2]/sum_std
fat_sens <- sd(pizza$fat)*pizza_mod$coefficients[3]/sum_std
sodium_sens <- sd(pizza$sodium)*pizza_mod$coefficients[4]/sum_std
mois_sens 
fat_sens  
sodium_sens 

barplot(c(sodium_sens, mois_sens,fat_sens), horiz = TRUE)