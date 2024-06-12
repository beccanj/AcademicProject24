library(randomForest)
data3 = read.csv("C:\\GIS\\QuickMart Project\\Records\\centres\\road_kde.csv")

str(data3)
set.seed(4543)
rf.fit <- randomForest(Supermarkets ~ ., data=data3, ntree=1000,
                       keep.forest=TRUE, importance=TRUE)
rf.fit

# Make predictions on the same dataset (for demonstration purposes; not recommended for final evaluation)
predictions <- predict(rf.fit, newdata = data3)
pa <- data.frame(name = data3$Name, actual = data3$Supermarkets, predicted = predictions)
#difference btw p and a
write.csv(pa, file = "pa_diff2.csv", row.names = TRUE)
# Model evaluation metrics
mse <- mean((data3$X20 - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(data3$X20 - predictions))
r_squared <- 1 - (sum((data3$X20 - predictions)^2) / sum((data3$X20 - mean(data3$X20))^2))

# Print or use the metrics as needed
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared (R^2):", r_squared, "\n")


#Save importance values
importance_values=importance(rf.fit)
varImpPlot(rf.fit)
#write.csv(importance_values, file = "new regression rf.csv", row.names = TRUE)
