library(readr)
library(stats)


#1. Select the CSV file
file_path <- file.choose()
#Read the second column of the CSV file
data <- read_csv(file_path)

# 2. Preprocess the data
rownames(data) <- data$`Country or region`
data <- data[, c('Score', 'GDP per capita', 'Healthy life expectancy', 'Freedom to make life choices', 'Generosity')]

# 3. Conduct regression analyses

# Simple Linear Regression (SLR) using OLS
slr_model <- lm(Score ~ `GDP per capita`, data = data)
summary(slr_model)

# Multiple Linear Regression (MLR) using OLS
mlr_model_ols <- lm(Score ~ ., data = data)
summary(mlr_model_ols)


# Jarque-Bera Test for normality
install.packages("moments")
library(moments)
res <- residuals(mlr_model_ols)
n <- length(res)
S <- skewness(res)
K <- kurtosis(res)
JB <- (n/6) * (S^2 + (1/4) * ((K - 3)^2))
p_value <- 1 - pchisq(JB, df = 2)
print(JB)
print(p_value)

install.packages("car")
install.packages("tseries")
install.packages("lmtest")
library(tseries)
library(lmtest)
library(car)

# Durbin-Watson Test for autocorrelation
dw_test <- dwtest(mlr_model_ols)

# Breusch-Pagan Test for heteroskedasticity
bp_test <- bptest(mlr_model_ols)

# White's Test for heteroskedasticity
white_test <- lmtest::coeftest(mlr_model_ols, vcov = sandwich::vcovHC(mlr_model_ols, type = "HC"))

# Variance Inflation Factor (VIF) to check for multicollinearity
vif_values <- vif(mlr_model_ols)

# Output the test results
print(list(
  Durbin_Watson_Test = dw_test,
  Breusch_Pagan_Test = bp_test,
  White_Test = white_test,
  VIF = vif_values
))

# Weighted Least Squares (WLS)
wls_weights <- 1 / (1 + data$`GDP per capita`)
# Multiple Linear Regression (MLR) using WLS
mlr_model_wls <- lm(Score ~ ., data = data, weights = wls_weights)
summary(mlr_model_wls)

# Ridge Regression (using manual implementation)
lambda_ridge <- 0.1 # Set lambda value for Ridge
model_ridge <- lm(Score ~ ., data = data)
coef_ridge <- coef(model_ridge)
coef_ridge <- coef_ridge / (1 + lambda_ridge)
coef_ridge




