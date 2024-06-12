# Load the dataset
data1 <- read.csv("C:\\GIS\\QuickMart Project\\Records\\Before pruning data\\Regression data.csv")


# Standardize the features (optional but recommended for gradient descent)
standardize <- function(x) {
  return((x - mean(x)) / sd(x))
}

# Assuming you have features X1 through X19
for (i in 1:19) {
  col_name <- paste0("X", i)
  data1[[col_name]] <- standardize(data1[[col_name]])
}

# dependent and independent variables
X <- data1[, grep("^X", names(data1))][-ncol(data1)]  # Exclude X20
X
Y <- data1$X20
Y
# Standardize the target variable (X20)
#data$X15 <- standardize(data$X15)

# Hypothesis Function for Multiple Linear Regression
hypothesis <- function(theta, X) {
  return(sum(theta * X))
}

# Cost Function (Mean Squared Error)
cost_function <- function(theta, X, Y) {
  m <- length(Y)
  h <- hypothesis(theta, X)
  return((1/(2*m)) * sum((h - Y)^2))
}

# Gradient Descent Function for Multiple Linear Regression
gradient_descent_multiple <- function(X, Y, learning_rate, num_iterations) {
  m <- length(Y)
  n <- ncol(X)
  theta <- rep(0, n)  # Initial parameters
  
  for (iteration in 1:num_iterations) {
    h <- hypothesis(theta, X)
    
    gradients <- rep(0, n)
    for (j in 1:n) {
      gradients[j] <- (1/m) * sum((h - Y) * X[, j])
    }
    
    # Update parameters
    theta <- theta - learning_rate * gradients
  }
  
  return(theta)
}

# Set hyperparameters
learning_rate <- 0.01
num_iterations <- 1000


# Apply gradient descent
theta <- gradient_descent_multiple(X, Y, learning_rate, num_iterations)

# Create a data frame with variable names and their respective theta
theta_df <- data.frame(Variable = names(X), Theta = theta)

# Write to CSV file
write.csv(theta_df, file = "learned_parameters.csv", row.names = FALSE)

# Print the learned parameters
cat("Learned parameters (theta):\n")
print(theta_df)

# Print the learned parameters
cat("Learned parameters (theta):", theta, "\n")
predictions <- hypothesis(theta, X)

# Mean Squared Error (MSE)
mse <- mean((predictions - Y)^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Error (MAE)
mae <- mean(abs(predictions - Y))

# R-squared (R²)
r_squared <- 1 - sum((predictions - Y)^2) / sum((Y - mean(Y))^2)

# Print the metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared (R^2):", r_squared, "\n")

