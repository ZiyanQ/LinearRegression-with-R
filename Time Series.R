library(dplyr)
library(readr)
library(stats)
file_path <- '5205 data.csv'
df <- read_csv(file_path)
all_stats <- sapply(df, function(x) {
  if(is.numeric(x)) {
    c(count = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      `25%` = quantile(x, 0.25, na.rm = TRUE),
      `50%` = median(x, na.rm = TRUE),
      `75%` = quantile(x, 0.75, na.rm = TRUE),
      max = max(x, na.rm = TRUE))
  } else {
    NULL
  }
})
all_stats_df <- as.data.frame(t(all_stats))
all_stats_rounded <- round(all_stats_df, 3)
all_stats_rounded
X <- df[, c('temperature', 'windSpeed', 'visibility', 'distance', 'humidity')]
y <- df$ride_count
model <- lm(y ~ ., data = cbind(y, X))
model_summary <- summary(model)
print(model_summary)

y <- data$ride_count
X <- cbind(1, data[, c('temperature', 'windSpeed', 'visibility', 'distance', 'humidity')]) 
ols_model <- lm(y ~ . - 1, data = as.data.frame(X))  
residuals <- residuals(ols_model)
weights <- 1 / (residuals^2)
wls_model <- lm(y ~ . - 1, data = as.data.frame(X), weights = weights)
wls_model_summary <- summary(wls_model)
print(wls_model_summary)

# Install GGally if it's not already installed
if (!require(GGally)) {
  install.packages("GGally")
  library(GGally)
}
library(ggplot2)
library(readr)
file_path <- '5205 data.csv'
data <- read_csv(file_path)
pair_plot <- ggpairs(data)
print(pair_plot)
ggsave('pair_plot.png', plot = pair_plot, width = 10, height = 10)

if (!require(reshape2)) {
  install.packages("reshape2")
  library(reshape2)
}
library(ggplot2)
library(readr)
file_path <- '5205 data.csv'
data <- read_csv(file_path)
correlation_matrix <- cor(data)
melted_correlation_matrix <- melt(correlation_matrix)
heatmap <- ggplot(melted_correlation_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  labs(title = 'Correlation Matrix Heatmap', x = '', y = '') +
  geom_text(aes(label = sprintf("%.2f", value)), size = 4, color = "black")
print(heatmap)
ggsave('correlation_matrix_heatmap.png', plot = heatmap, width = 10, height = 8)


install.packages("moments")
library(moments)
install.packages("tseries")
install.packages("lmtest")
install.packages("car")
library(tseries)
library(lmtest)
library(car)

# Perform the Jarque-Bera test for normality
jb_test_result <- jarque.bera.test(model)
print(jb_test_result)

# Durbin-Watson Test for autocorrelation
dw_test_result <- dwtest(model)
print(dw_test_result)

# Breusch-Pagan Test for heteroskedasticity
bp_test_result <- bptest(model)
print(bp_test_result)

# White's Test for heteroskedasticity
white_test_result <- bptest(model, studentize = FALSE)
print(white_test_result)

# Variance Inflation Factor (VIF) to check for multicollinearity
vif_values <- vif(model)
print(vif_values)


#back-testing
library(pROC)

# Assuming 'df' is your data frame
X <- df[, c('temperature', 'windSpeed', 'visibility', 'distance', 'humidity')]
y <- df$ride_count

# Combine the independent and dependent variables into a single data frame
model_data <- cbind(y, X)
model <- glm(factor(y > median(y)) ~ ., data = model_data, family = "binomial")

# Extract predicted probabilities
predicted_probs <- predict(model, type = "response")

# Assuming 'actual_labels' are the true labels (binary)
actual_labels <- factor(y > median(y))

# Calculate ROC curve
roc_curve <- roc(actual_labels, predicted_probs)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Add diagonal reference line (random classifier)
abline(a = 0, b = 1, col = "red", lty = 2)

# Add AUC to the plot
text(0.8, 0.2, paste("AUC =", round(auc(roc_curve), 3)), col = "blue", cex = 1.2)

# Show the plot


