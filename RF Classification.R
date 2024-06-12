#RF CLASSIFICATION
#read data
data1 <- read.csv("C:\\GIS\\QuickMart Project\\Records\\Classification data.csv")

#explore data
str(data1)
dim(data1)
summary(data1)

#making supermarket as dependent variable
data1$X20 <- as.factor(data1$X20)
table(data1$X20)

#creating training and testing datasets
set.seed(123)
ind <- sample(2, nrow(data1), replace = TRUE, prob = c(0.7, 0.3))
train <- data1[ind==1,]
test <- data1[ind==2,]

#Getting variable of importance
library(randomForest)
set.seed(222)
rf<-randomForest(X20~.,data=train)
print(rf)

#Plot of significance of variables
#imp = importance(rf)
#varImpPlot(rf)

#ACCURACY OF BINARY CLASSIFICATION
#Predict - Test
predictions <- predict(rf, newdata = test)

# Model evaluation metrics
#install.packages("caret")
library(caret)
conf_matrix <- confusionMatrix(predictions, test$X20)
conf_matrix
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1 <- conf_matrix$byClass["F1"]

# Print or use the metrics as needed
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1, "\n")

#PREDICT - Training
predictions <- predict(rf, newdata = train)

# Model evaluation metrics
#install.packages("caret")
library(caret)
conf_matrix1 <- confusionMatrix(predictions, train$X20)
conf_matrix1
accuracy1 <- conf_matrix1$overall["Accuracy"]
precision1 <- conf_matrix1$byClass["Precision"]
recall1 <- conf_matrix1$byClass["Recall"]
f11 <- conf_matrix1$byClass["F1"]

# Print or use the metrics as needed
cat("Accuracy:", accuracy1, "\n")
cat("Precision:", precision1, "\n")
cat("Recall:", recall1, "\n")
cat("F1 Score:", f11, "\n")





# Getting variable importance
importance_values <- importance(rf)
varImpPlot(rf)
# Print the variable importance
#print(importance_values)

# Save the variable importance results as a CSV file
#write.csv(importance_values, file = "variable_importance_results.csv", row.names = FALSE)




