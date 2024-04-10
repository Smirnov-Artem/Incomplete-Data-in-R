# Smirnov Artem, Tukhbatullin Artem
# Trier University
# Comparing Methods for Tree-Based Multiple Imputation
# Script for the poster 

# the required package
library(mice)
library(miceRanger)

library(caret)
library(ggplot2)

#test vif
library(car)

#model's quality checking
library(lmtest)
library(tseries)

# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 1000

# Beta coefficients
beta <- c(0.5, 0.5, 0.5, 0.5, -0.5, 1, 1, 1, 1, -1, 2, 2, 2, 2, 2, 2, -2, -2, 3, -3)

# Create independent variables
set.seed(123)
X <- matrix(rnorm(n * length(beta)), ncol = length(beta))

# Create random errors
errors <- rnorm(n)

# Create a response variable
y <- 0 + X %*% beta + errors

# Create a dataframe
Y = as.data.frame(y)
names(Y)[names(Y) == 'V1'] <- 'Y'
simulated_data <- data.frame(cbind(Y, X))

# Display the first few rows of the simulated dataset
head(simulated_data)

# Let's perform simple OLS regression
model_init_data_ols <- lm(Y ~ X1 + X2 + X3 + X4 + X5 +
                            X6 + X7 + X8 + X9 + X10 +
                            X11 + X12 + X13 + X14 + X15 + X16 +
                            X17 + X18 + X19 + X20, data=simulated_data)
vif_values <- vif(model_init_data_ols)
vif_values # as expected, there is not multicollinearity, so the x_i are independent from each other

# Summary of the regression
summary(model_init_data_ols)

# Display AIC and BIC
cat("AIC, OLS for the initial data:", AIC(model_init_data_ols), "\n")
cat("BIC, OLS for the initial data:", BIC(model_init_data_ols), "\n")

preds = predict(model_init_data_ols)
# Calculate the RMSE for each prediction
rmse_values <- sqrt(mean((y - preds)^2))
# Calculate the average RMSE
average_rmse <- mean(rmse_values)
# Print the result
print("ARMSE, OLS for the initial data:")
print(average_rmse)



residuals <- resid(model_init_data_ols)
# Normality tests
# Shapiro-Wilk
shapiro.test(residuals)
# Kolmogorov-Smirnov
ks.test(residuals, "pnorm")
# Jarque-Bera
jarque.bera.test(residuals)

# Homoscedasticity tests
# Breusch-Pagan
bptest(model_init_data_ols)






####################################################################################################






# Now we start to modify the dataset
df_for_missing_values <- simulated_data

# Set the columns where N/A values should not be introduced
columns_to_keep <- c("Y", "X11", "X16", "X18")

# Generate random missing percentages for each column
num_columns <- ncol(df_for_missing_values)
missing_percentages <- runif(num_columns, 0.05, 0.5)
# Randomly introduce N/A values for each column
real_missing_percentages <- vector("numeric", length = ncol(df_for_missing_values))

for (col in names(df_for_missing_values)) {
  if (!(col %in% columns_to_keep)) {
    missing_percentage <- missing_percentages[which(names(df_for_missing_values) == col)]
    real_missing_percentages[which(names(df_for_missing_values) == col)] <- missing_percentage
    missing_count <- round(nrow(df_for_missing_values) * missing_percentage)
    missing_indices <- sample(1:nrow(df_for_missing_values), missing_count)
    df_for_missing_values[missing_indices, col] <- NA
  }
}
real_missing_percentages

####################################################################################################
#mice
####################################################################################################


# cart
data <- df_for_missing_values

# Set the seed for reproducibility
set.seed(123)

time_imputed_data_cart_mice <- system.time({
  # Create an imputation model using cart
  imputation_model <- mice(data, method = "cart")
  # Perform imputation
  imputed_data_cart_mice <- complete(imputation_model)
})

# Display the imputed dataset
print(imputed_data_cart_mice)

# Let's perform simple OLS regression
model_imputed_data_cart_mice <- lm(Y ~ X1 + X2 + X3 + X4 + X5 +
                                     X6 + X7 + X8 + X9 + X10 +
                                     X11 + X12 + X13 + X14 + X15 + X16 +
                                     X17 + X18 + X19 + X20, data=imputed_data_cart_mice)

# Summary of the regression
summary(model_imputed_data_cart_mice)

# Display AIC and BIC
cat("AIC:", AIC(model_imputed_data_cart_mice), "\n")
cat("BIC:", BIC(model_imputed_data_cart_mice), "\n")

preds = predict(model_imputed_data_cart_mice)
# Calculate the RMSE for each prediction
rmse_values <- sqrt(mean((y - preds)^2))
# Calculate the average RMSE
average_rmse <- mean(rmse_values)
# Print the result
print("ARMSE, OLS for the imputed_data_cart_mice:")
print(average_rmse)



residuals <- resid(model_imputed_data_cart_mice)
# Normality tests
# Shapiro-Wilk
shapiro.test(residuals)
# Kolmogorov-Smirnov
ks.test(residuals, "pnorm")
# Jarque-Bera
jarque.bera.test(residuals)

# Homoscedasticity tests
# Breusch-Pagan
bptest(model_imputed_data_cart_mice)







# rf
data <- df_for_missing_values

# Set the seed for reproducibility
set.seed(123)


time_imputed_data_rf_mice <- system.time({
  # Create an imputation model using cart
  imputation_model <- mice(data, ntree = 100, method = "rf")
  # Perform imputation
  imputed_data_rf_mice <- complete(imputation_model)
})

# Display the imputed dataset
print(imputed_data_rf_mice)

# Let's perform simple OLS regression
model_imputed_data_rf_mice <- lm(Y ~ X1 + X2 + X3 + X4 + X5 +
                                   X6 + X7 + X8 + X9 + X10 +
                                   X11 + X12 + X13 + X14 + X15 + X16 +
                                   X17 + X18 + X19 + X20, data=imputed_data_rf_mice)

# Summary of the regression
summary(model_imputed_data_rf_mice)

# Display AIC and BIC
cat("AIC:", AIC(model_imputed_data_rf_mice), "\n")
cat("BIC:", BIC(model_imputed_data_rf_mice), "\n")

preds = predict(model_imputed_data_rf_mice)
# Calculate the RMSE for each prediction
rmse_values <- sqrt(mean((y - preds)^2))
# Calculate the average RMSE
average_rmse <- mean(rmse_values)
# Print the result
print("ARMSE, OLS for the imputed_data_rf_mice:")
print(average_rmse)




residuals <- resid(model_imputed_data_rf_mice)
# Normality tests
# Shapiro-Wilk
shapiro.test(residuals)
# Kolmogorov-Smirnov
ks.test(residuals, "pnorm")
# Jarque-Bera
jarque.bera.test(residuals)

# Homoscedasticity tests
# Breusch-Pagan
bptest(model_imputed_data_rf_mice)


####################################################################################################
#miceRanger - 10 trees
####################################################################################################

data <- df_for_missing_values

# Set the seed for reproducibility
set.seed(123)

# Create an imputation model using cart
imputation_model <- miceRanger(data, num.trees = 100, returnModels=TRUE)
# Perform imputation
time_imputed_data_miceranger_100 <- system.time({
  imputed_data_miceranger_100 <- impute(data,imputation_model,verbose=FALSE)$imputedData$Dataset_5
})
# Display the imputed dataset
print(imputed_data_miceranger_100)
# Let's perform simple OLS regression
model_imputed_data_miceranger_100 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 +
                                      X6 + X7 + X8 + X9 + X10 +
                                      X11 + X12 + X13 + X14 + X15 + X16 +
                                      X17 + X18 + X19 + X20, data=imputed_data_miceranger_100)

# Summary of the regression
summary(model_imputed_data_miceranger_100)

# Display AIC and BIC
cat("AIC:", AIC(model_imputed_data_miceranger_100), "\n")
cat("BIC:", BIC(model_imputed_data_miceranger_100), "\n")

preds = predict(model_imputed_data_miceranger_100)
# Calculate the RMSE for each prediction
rmse_values <- sqrt(mean((y - preds)^2))
# Calculate the average RMSE
average_rmse <- mean(rmse_values)
# Print the result
print("ARMSE, OLS for the imputed_data_miceranger_100:")
print(average_rmse)



residuals <- resid(model_imputed_data_miceranger_100)
# Normality tests
# Shapiro-Wilk
shapiro.test(residuals)
# Kolmogorov-Smirnov
ks.test(residuals, "pnorm")
# Jarque-Bera
jarque.bera.test(residuals)

# Homoscedasticity tests
# Breusch-Pagan
bptest(model_imputed_data_miceranger_100)


####################################################################################################
#miceRanger - 1 tree
####################################################################################################

data <- df_for_missing_values

# Set the seed for reproducibility
set.seed(123)
miceRanger
# Create an imputation model using cart
imputation_model <- miceRanger(data, num.trees = 1,  maxiter = 1, m = 1, returnModels=TRUE)
# Perform imputation
time_imputed_data_miceranger_1 <- system.time({
  imputed_data_miceranger_1 <- impute(data,imputation_model,verbose=FALSE)$imputedData$Dataset_1
})
# Display the imputed dataset
print(imputed_data_miceranger_1)
# Let's perform simple OLS regression
model_imputed_data_miceranger_1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 +
                                      X6 + X7 + X8 + X9 + X10 +
                                      X11 + X12 + X13 + X14 + X15 + X16 +
                                      X17 + X18 + X19 + X20, data=imputed_data_miceranger_1)

# Summary of the regression
summary(model_imputed_data_miceranger_1)

# Display AIC and BIC
cat("AIC:", AIC(model_imputed_data_miceranger_1), "\n")
cat("BIC:", BIC(model_imputed_data_miceranger_1), "\n")

preds = predict(model_imputed_data_miceranger_1)
# Calculate the RMSE for each prediction
rmse_values <- sqrt(mean((y - preds)^2))
# Calculate the average RMSE
average_rmse <- mean(rmse_values)
# Print the result
print("ARMSE, OLS for the imputed_data_miceranger_1:")
print(average_rmse)




residuals <- resid(model_imputed_data_miceranger_1)
# Normality tests
# Shapiro-Wilk
shapiro.test(residuals)
# Kolmogorov-Smirnov
ks.test(residuals, "pnorm")
# Jarque-Bera
jarque.bera.test(residuals)

# Homoscedasticity tests
# Breusch-Pagan
bptest(model_imputed_data_miceranger_1)


##############################################################################################
#Visualization (based on https://github.com/FarrellDay/miceRanger/blob/master/README.md)
#############################################################################################


# Function to calculate MSE for each variable
calculate_mae <- function(real, imputed) {
  # Initialize an empty vector to store MAE values
  mae_values <- numeric(ncol(real))
  
  # Calculate MAE for each variable
  for (i in 1:ncol(real)) {
    rel_var <- real[, i]
    imputed_var <- imputed[, i]
    abs_diff <- abs(rel_var - imputed_var)
    mae_values[i] <- mean(abs_diff, na.rm = TRUE)
  }
  
  result <- setNames(mae_values, colnames(real))
  
  return(result)
}

# Calculate MSE values for each imputation method
mae_rf <- calculate_mae(simulated_data, imputed_data_rf_mice)
mae_cart <- calculate_mae(simulated_data, imputed_data_cart_mice)
mae_miceranger <- calculate_mae(simulated_data, as.matrix(imputed_data_miceranger_100))
mae_miceranger_1 <- calculate_mae(simulated_data, as.matrix(imputed_data_miceranger_1))

# Combine the MAE values into a dataframe
mae_data <- data.frame(
  Variable = names(simulated_data),
  MiceRF100 = mae_rf,
  MiceCART = mae_cart,
  MiceRangerRF100 = mae_miceranger,
  MiceRangerRF1 = mae_miceranger_1
)

# Reshape data for plotting
my_colors <- c("grey75", "#6E7183", "#7CB0D0", "#007AC4")
# Filter out rows with missing percentages equal to 0.0000
real_missing_percentages <- real_missing_percentages[real_missing_percentages != 0.0000]
round(real_missing_percentages,2)
# Melting the data
mae_data_long <- reshape2::melt(mae_data, id.vars = "Variable")
mae_data_long
# Subset the data where value > 0.0
mae_data_long <- subset(mae_data_long, value > 0.0)
# Define custom order for the Variable column

# Create a vall column using the corresponding missing percentages
mae_data_long$vall <- rep(real_missing_percentages[rep(1:length(real_missing_percentages))])
mae_data_long$Variable = paste(mae_data_long$Variable, "(", round(mae_data_long$vall, 2), ")")
mae_data_long
mae_data_long$Variable <- gsub(" ", "", mae_data_long$Variable)
custom_order <- mae_data_long$Variable[1:17]
mae_data_long$Variable <- factor(mae_data_long$Variable, levels = custom_order)
mae_data_long
# Plotting using ggplot2
ggplot(mae_data_long, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
       x = "Variable",
       y = "MAE",
       fill = "Imputation Method") +
  scale_fill_manual(values = my_colors) +  # Set custom colors
  theme_minimal() +
  theme(text = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11,angle = 0))  # Adjust angle and hjust as needed
mae_data_long






beta_full <- c(0, 0.5, 0.5, 0.5, 0.5, -0.5, 1, 1, 1, 1, -1, 2, 2, 2, 2, 2, 2, -2, -2, 3, -3)

# Calculate biases for each imputation method
bias_init <- beta_full-coef(model_init_data_ols)
bias_rf <- beta_full-coef(model_imputed_data_rf_mice)
bias_cart <- beta_full-coef(model_imputed_data_cart_mice)
bias_miceranger <- beta_full-coef(model_imputed_data_miceranger_100)
bias_miceranger_1 <- beta_full-coef(model_imputed_data_miceranger_1)

# Combine the bias values into a dataframe
bias_data <- data.frame(
  Variable = names(simulated_data),
  InitialData = bias_init,
  MiceRF100 = bias_rf,
  MiceCART = bias_cart,
  MiceRangerRF100 = bias_miceranger,
  MiceRangerRF1 = bias_miceranger_1
)
bias_data <- subset(bias_data, !(Variable %in% columns_to_keep))
# Reshape data for plotting
my_colors <- c("black", "grey75", "#6E7183", "#7CB0D0", "#007AC4")

# Melting the data
bias_data_long <- reshape2::melt(bias_data, id.vars = "Variable")

# Subset the data where value > 0.0
bias_data_long <- subset(bias_data_long, value != 0.0)

# Create a vall column using the corresponding missing percentages
bias_data_long$vall <- rep(real_missing_percentages[rep(1:length(real_missing_percentages))])
bias_data_long$Variable <- paste(bias_data_long$Variable, "(", round(bias_data_long$vall, 2), ")")
bias_data_long$Variable <- gsub(" ", "", bias_data_long$Variable)

# Define custom order for the Variable column
custom_order <- bias_data_long$Variable[1:17]
bias_data_long$Variable <- factor(bias_data_long$Variable, levels = custom_order)

# Plotting using ggplot2
ggplot(bias_data_long, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
       x = "Variable",
       y = "Bias",
       fill = "Data") +
  scale_fill_manual(values = my_colors) +  # Set custom colors
  theme_minimal() +
  theme(text = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11,angle = 0))  # Adjust angle and hjust as needed










calculate_rmse <- function(real, imputed) {
  # Initialize an empty vector to store RMSE values
  rmse_values <- numeric(ncol(real))
  # Calculate RMSE for each variable
  for (i in 1:ncol(real)) {
    rel_var <- real[, i]
    imputed_var <- imputed[, i]
    squared_diff <- (rel_var - imputed_var)^2
    mse <- mean(squared_diff, na.rm = TRUE)
    rmse_values[i] <- sqrt(mse)
  }
  result <- setNames(rmse_values, colnames(real))
  
  return(result)
}
rmse_result_rf <- calculate_rmse(simulated_data, imputed_data_rf_mice)
rmse_result_cart <- calculate_rmse(simulated_data, imputed_data_cart_mice)
rmse_result_miceranger <- calculate_rmse(simulated_data, as.matrix(imputed_data_miceranger_100))
rmse_result_miceranger_1 <- calculate_rmse(simulated_data, as.matrix(imputed_data_miceranger_1))

# Combine the MAE values into a dataframe
rmse_data <- data.frame(
  Variable = names(simulated_data),
  RF = round(rmse_result_rf,3),
  CART = round(rmse_result_cart,3),
  MiceRanger = round(rmse_result_miceranger,3),
  MiceRanger_1 = round(rmse_result_miceranger_1,3)
)
rmse_data <- subset(rmse_data, RF > 0)
rmse_data

# Timing
time_imputed_data_cart_mice
time_imputed_data_rf_mice
time_imputed_data_miceranger_100
time_imputed_data_miceranger_1