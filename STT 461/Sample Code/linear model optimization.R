# optimization for linear regression

# standard linear regression
# residual form is diamonds$price - (coeff[1] + coeff[2] * diamonds$carat)

# linear regression with optim
coeff <- rep(0,2)
lin_reg <- function(coeff) sum((diamonds$price - (coeff[1] + coeff[2] * diamonds$carat))^2)
RMSE_fit <- optim(c(100, -1), lin_reg, diamonds)
RMSE_fit$par

# MAE fit with optim
coeff_MAE <- rep(0,2)
MAE_reg <- function(coeff_MAE) sum(abs(diamonds$price - (coeff_MAE[1] + coeff_MAE[2] * diamonds$carat)))
MAE_fit <- optim(c(100, -1), MAE_reg, diamonds)
MAE_fit$par

# Median fit with optim
coeff_med <- rep(0,2)
med_reg <- function(coeff_med) median(abs(diamonds$price - (coeff_med[1] + coeff_med[2] * diamonds$carat)))
med_fit <- optim(c(100, -1), med_reg, diamonds)
med_fit$par

plot(diamonds$carat, diamonds$price)
lines(diamonds$carat, diamonds$carat * RMSE_fit$par[2] + RMSE_fit$par[1], col = 'Blue')
lines(diamonds$carat, diamonds$carat * MAE_fit$par[2] + MAE_fit$par[1], col = 'Red')
lines(diamonds$carat, diamonds$carat * med_fit$par[2] + med_fit$par[1], col = 'Green')

# Max likelihood optimization
ll_coeff <- c(0,0,0)
LLoptim <- function(ll_coeff) -1*sum(log(dnorm(diamonds$price - (ll_coeff[1] + ll_coeff[2]*diamonds$carat), 0, ll_coeff[3])))
LL_fit <- optim(c(100,-1, 2000), LLoptim, diamonds)
LL_fit


# Ridge regression

# First, standardize variables
diamonds_std <- as.data.frame(scale(diamonds[c(1,5,6,7)]))
lm(data = diamonds_std, price ~ carat + depth+ table)

# Define ridge function with hyperparameter
h <- length(diamonds$price)/3
ridge_par <- rep(0,4)
ridge_loss <- function(ridge_par) sum((diamonds_std$price - (ridge_par[1] + ridge_par[2] * diamonds_std$carat + ridge_par[3] * diamonds_std$table + ridge_par[4] * diamonds_std$price))^2) + h * (sum(ridge_par^2) - ridge_par[1])
ridge_model <- optim(c(1,-1,1,1), ridge_loss)
ridge_model$par