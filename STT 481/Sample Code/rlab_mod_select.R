

###  SS23 STT481: Lab: Subset Selection and Shrinkage Methods


### DataSet Hitters: We wish to predict Baseball players Salaries using 
### several predictors associated with performance in the previous year.

library(ISLR)
data("Hitters")
head(Hitters)

### **Preprocessing**: remove missing observations. This is not the 
### best way to deal with NA's. Sometime setting missing values to
### some value (the mean, mode or median)is best - imputation

which(is.na(Hitters$Salary))   # it returns the index of missing Salary's
head(Hitters[which(is.na(Hitters$Salary)),]) # Salary's are NA's
Hitters <- Hitters[!is.na(Hitters$Salary),] # We remove those rows which contain NA's
which(is.na(Hitters$Salary)) 

# Subset Selection Methods

## Best Subset Selection

library(leaps)
regfit.full <- regsubsets(Salary ~ . , data = Hitters)
#
summary(regfit.full)

### Note that The default of `nvmax` option in `regsubsets` so it 
### only reports up to the best 8-variable model. 
### We can change it to `nvmax=19` to fit up to a 19-variable model.
### Note when `nvmax` is moderate large (say, 50), 
### it might take a long time to fit the model.

regfit.full <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)

### It returns training errors and estimated test errors.

reg.summary$rsq  ## training errors for each of the 19 models

### As expected, $R^2$ statistic increases monotonically 
### as more variables are included.

reg.summary$rss ### Conversely `rss` decreases with more variables

### Plotting RSS, adjusted R2, Cp, and BIC for all of the models 
### at once will help us decide which model to select.

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ", ylab="RSS", type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq", type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables ", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables ", ylab = "BIC", type = "l")
points(6,reg.summary$bic[6], col = "red", cex = 2, pch = 20)


### The `regsubsets()` function has a built-in `plot()` command which
### can be used to display the selected variables for the best model
### with a given number of predictors, ranked according to the BIC, 
### Cp, adjusted R2, or AIC.

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

### We can use the `coef()` function to see the coefficient estimates
### associated with this model.

coef(regfit.full, 6)

## Forward and Backward Step-wise Selection

regfit.fwd <- regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "forward")
#summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "backward")
#summary(regfit.bwd)

#### we can plot RSS, adjusted R2, Cp, and BIC for all of the models 
### seleced by forward stepwise selection

par(mfrow=c(2,2))
reg.fwd.summary <- summary(regfit.fwd)
plot(reg.fwd.summary$rss ,xlab="Number of Variables ", ylab="RSS", type="l")
plot(reg.fwd.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq", type="l")
which.max(reg.fwd.summary$adjr2)
points(11, reg.fwd.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.fwd.summary$cp, xlab = "Number of Variables ", ylab = "Cp", type = "l")
which.min(reg.fwd.summary$cp)
points(10,reg.fwd.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.fwd.summary$bic)
plot(reg.fwd.summary$bic, xlab = "Number of Variables ", ylab = "BIC", type = "l")
points(6,reg.fwd.summary$bic[6], col = "red", cex = 2, pch = 20)

### Observe that the selected models identified by forward, 
### backward stepwise selection and best subset selection are different.

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

## Choosing Among Models using Cross-Validation

### We may also use the CV approach for model selection
### But because there is no `predict()` method for `regsubsets()`, 
### we can write our own predict method.

predict.regsubsets <- function (object, newdata , id, ...){
  form <- as.formula(object$call[[2]])  # formula of null model
  mat <- model.matrix(form, newdata)    # building an "X" matrix from newdata
  coefi <- coef(object, id = id)        # coefficient estimates associated with the object model containing id non-zero variables
  xvars <- names(coefi)            # names of the non-zero coefficient estimates
  return(mat[,xvars] %*% coefi)    # X[,non-zero variables] %*% Coefficients[non-zero variables]
}

## For example, suppose we want to predict the newdata 
### below using the models containing 6 variables selected by 
### best subset selection.

newdata <- Hitters[1:2, ] ## I made up a newdata
newdata[,"Hits"] <- newdata[,"Hits"] + rep(60,2) ## I made up a newdata
newdata[,"HmRun"] <- newdata[,"HmRun"] + rep(30,2) ## I made up a newdata
predict.regsubsets(regfit.full, newdata = newdata, id = 6)


## Now we are ready to perform a 10-fold cross-validation.

fold.index <- cut(sample(1:nrow(Hitters)), breaks=10, labels=FALSE)
cv.error.best.fit <- rep(0,19)
for(i in 1:19){   # try different numbers of variables
  cat("i=", i,"\n")
  error <- rep(0, 10)
  for (k in 1:10){
    Hitters.train <- Hitters[fold.index != k,]
    Hitters.test <- Hitters[fold.index == k,]
    true.y <- Hitters.test[,"Salary"]
    best.fit <- regsubsets(Salary ~ ., data = Hitters.train, nvmax = 19)
    pred <- predict(best.fit, Hitters.test, id = i)
    error[k] <- mean((pred - true.y)^2)
  }
  print(mean(error))
  cv.error.best.fit[i] <- mean(error)
}


par(mfrow=c(1,1))
plot(cv.error.best.fit, type = "b")
points(which.min(cv.error.best.fit), cv.error.best.fit[which.min(cv.error.best.fit)], 
       col = "red", cex = 2, pch = 20)


## We see that cross-validation selects an 
### `r which.min(cv.error.best.fit)`-variable model. 
### We now perform best subset selection on the full data set in 
### order to obtain the `r which.min(cv.error.best.fit)`-variable 
### model.

reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, which.min(cv.error.best.fit))

## Notice that the data = Hitters, which is the full data set. 
## Don't miss this step after performing cross-validation methods.


## Shrinkage Methods

### We will use the `glmnet` package in order to perform ridge 
### regression and the lasso.  The main function in this package 
### is `glmnet()`, which can be used to fit ridge regression models,
### lasso models, and more. This function has slightly different 
### syntax from other model-fitting functions that we have 
### encountered thus far. In particular, we must pass in an `x` 
### matrix as well as a `y` vector, and we do not use the `y ~ x` 
### syntax. In order to do so, we use `model.matrix()` to create `x`, 
### which not only produces a matrix corresponding to the 19 
### predictors but it also automatically transforms any qualitative 
### variables into dummy variables.


X <- model.matrix(Salary ~., Hitters)[,-1]  # the first column is the intercept
y <- Hitters$Salary


## Ridge Regression

library(glmnet)
ridge.mod <- glmnet(X, y, alpha = 0)  # 0 indicates ridge regression

### you can also assign the values of lambda to perform the ridge 
### regression.

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(X, y, alpha = 0, lambda = grid)


### Associated with each value of lambda is a vector of ridge 
### regression coefficients, stored in a matrix that can be 
### accessed by `coef()`. In this case, it is a 20 times 100 matrix, with 20 rows (one for each predictor, plus an intercept) and 100 columns (one for each value of $\lambda$).


dim(coef(ridge.mod))


ridge.mod$lambda[50]
coef(ridge.mod)[,50] # this indicates the coef estimates when lambda = 11498


### In general, we would like to use cross-validation to choose 
### the tuning parameter $\lambda$. We can do this using the 
### built-in cross-validation function, `cv.glmnet()`. 
### By default, the function performs 10-fold cross-validation, 
### though this can be changed using the argument `nfolds`. 


cv.out <- cv.glmnet(X, y, alpha = 0, nfolds = 10)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

### We can examine the coefficient estimates when lambda
### `bestlam` using `coef()`.

coef(ridge.mod, s = bestlam)

### As expected, none of the coefficients are exactly zero
### ridge regression does not perform variable selection!

### create new data and suppose I want to predict their responses (Salary).

newdata <- Hitters[1:2, ] ## I made up a newdata
newdata[,"Hits"] <- newdata[,"Hits"] + rep(60,2) ## I made up a newdata
newdata[,"HmRun"] <- newdata[,"HmRun"] + rep(30,2) ## I made up a newdata
test.X <- model.matrix(Salary ~., newdata)[,-1] # same form as X
ridge.pred <- predict(ridge.mod, s = bestlam, newx = test.X)
ridge.pred

### Lasso
## In order to fit a lasso model, we once again use the 
## glmnet() function; 
## however, this time we use the argument alpha=1.

lasso.mod <- glmnet(X, y, alpha = 1, lambda = grid)

### use cross-validation to choose the tuning parameter $\lambda$.

cv.out <- cv.glmnet(X, y, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
coef(lasso.mod, s = bestlam)

### some coefficient estimates with the best lambda are exactly zero!

### make prediction using the best lambda.

lasso.pred <- predict(lasso.mod, s = bestlam, newx = test.X)
lasso.pred

