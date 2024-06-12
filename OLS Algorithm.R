# Assuming your data is stored in a data frame called data1
data1 <- read.csv("C:\\GIS\\QuickMart Project\\Records\\After pruning data\\RF PRUNED DATA.csv")

# Fit linear regression model
model <- lm(X20 ~ ., data = data1)
summary(model)
#write csv
coefficients <- coef(summary(model))
significance <- coefficients[, "Pr(>|t|)"]

# Create a dataframe with variable names and significance values
significance_df <- data.frame(Variable = names(significance), Significance = significance)

# Write to CSV file
write.csv(significance_df, file = "OLS pruned results.csv", row.names = TRUE)

# Print the dataframe
print(significance_df)


#evaluation metrics
# Assuming 'model' is the linear regression model obtained earlier

# Make predictions using the model
predictions <- predict(model, data1)

# Calculate Mean Squared Error (MSE)
mse <- mean((data1$X20 - predictions)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(data1$X15 - predictions))

# Extract R-squared value
rsquared <- summary(model)$r.squared

# Print the results
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", rsquared, "\n")

#PRUNNED DATA
data1 <- read.csv("C:\\GIS\\QuickMart Project\\Records\\Regression data.csv")
# Fit linear regression model
model2 <- lm(X15 ~ ., data = data1)
# Make predictions using the model
predictions <- predict(model, data1)

# Calculate Mean Squared Error (MSE)
mse <- mean((data1$X20 - predictions)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(data1$X20 - predictions))

# Extract R-squared value
rsquared <- summary(model)$r.squared

# Print the results
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", rsquared, "\n")
# Plotting the results
par(mfrow = c(2, 2))  # Set up a 2x2 grid of plots

# Residuals vs Fitted
plot(model, which = 1)

# Normal Q-Q plot
plot(model, which = 2)

# Scale-Location (Square Root of Standardized Residuals vs Fitted)
plot(model, which = 3)

# Residuals vs Leverage
plot(model, which = 5)
