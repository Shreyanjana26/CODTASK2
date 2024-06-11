# Install necessary packages if not already installed
if (!require("readxl")) {
  install.packages("readxl")
}
if (!require("caret")) {
  install.packages("caret")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

# Load required libraries
library(readxl)
library(caret)
library(ggplot2)

# Read data
housing<-read_excel("C:\\Users\\Shreyanjana\\Downloads\\Housing.xlsx")

# Data preprocessing 
housing <- na.omit(housing)
housing <- unique(housing)

check_outliers <- c("price", "area")

for (col in check_outliers) {
  z_scores <- scale(housing[[col]])
  outliers <- abs(z_scores) > 3
  
  housing <- housing[!outliers, ]
}

# Splitting into training and testing sets, separating the target variable
target_variable <- housing$price
features_only <- housing[, !names(housing) %in% c("price")]
set.seed(123)
index <- sample(nrow(housing), 0.7 * nrow(housing))
training_data <- features_only[index, ]
testing_data <- features_only[-index, , drop = FALSE]  
training_data$price <- target_variable[index]
testing_t <- c(target_variable[-index])
testing_target <- data.frame(price = testing_t)

# Regression model
lm_model <- lm(price ~ ., data = training_data)
summary(lm_model)

# Predictions
pred <- predict(lm_model, newdata = testing_data)
predictions <- data.frame(predicted_price = pred)
View(predictions)

# Calculating errors
mse <- mean((predictions - testing_target$price)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - testing_target$price))

cat("Mean Squared Error (MSE): ", mse, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("Mean Absolute Error (MAE): ", mae, "\n")

# Actual vs Predicted comparison
predicted_values <- predictions$predicted_price
actual_values <- testing_target$price
comparison <- data.frame(Actual = actual_values, Predicted = predicted_values)
View(comparsion)

# Scatterplot of Actual vs Predicted Values
plot(comparison$Actual, comparison$Predicted, 
     xlab = "Actual Values", ylab = "Predicted Values", 
     main = "Actual vs Predicted Values", col = "blue", pch = 16)

# Adding a diagonal line for reference
abline(0, 1, col = "red")  

# Adding text labels for better readability
text(comparison$Actual, comparison$Predicted, labels = comparison$Actual, pos = 3, col = "black", cex = 0.8)

# Legend
legend("bottomright", legend = "Diagonal Line", col = c("red"), lty = 1, cex = 0.8)
legend("bottomleft", legend = "Data Points", col = c("blue"), pch = 16, cex = 0.8)

# Visualization of Residuals
# Residuals vs Fitted Values
plot(lm_model, which = 1)  
# This plot helps in identifying if residuals have a pattern or systematic deviation from zero across the range of fitted values.

# Q-Q plot of Residuals
plot(lm_model, which = 2) 
# This plot assesses if the residuals follow a normal distribution; ideally, points should fall along the diagonal line.

# Assuming there is a "locality" column in your dataset
if ("locality" %in% colnames(housing)) {
  localities <- unique(housing$locality)
  
  # Create a new data frame to store locality-wise predictions
  locality_predictions <- data.frame(Localities = character(0), RMSE = numeric(0))
  
  # Create a list to store ggplot objects for each locality
  ggplots <- list()
  
  # Create a data frame to store all predicted values
  all_predictions <- data.frame(Predicted = numeric(0), Locality = character(0))
  
  for (loc in localities) {
    # Subset data for the current locality
    subset_data <- testing_data[testing_data$locality == loc, ]
    
    # Predictions for the current locality
    pred_loc <- predict(lm_model, newdata = subset_data)
    
    # Check for missing or non-numeric values
    if (any(is.na(pred_loc)) || any(!is.finite(pred_loc))) {
      cat("Skipping locality:", loc, "due to missing or non-numeric values in predictions.\n")
    } else {
      # Append results to the all_predictions data frame
      all_predictions <- rbind(all_predictions, data.frame(Predicted = pred_loc, Locality = loc))
    }
  }
  
  # Check if there are valid data points for the ggplot
  if (nrow(all_predictions) > 0) {
    # Create a ggplot for all predicted values by locality
    ggplot(all_predictions, aes(x = Locality, y = Predicted)) +
      geom_point(color = "blue", size = 3) +
      labs(title = "Predicted Values by Locality",
           x = "Localities", y = "Predicted Values") +
      geom_hline(yintercept = mean(all_predictions$Predicted, na.rm = TRUE), linetype = "dashed", color = "red") +
      theme_minimal() -> ggplots[["Predicted by Locality"]]
  } else {
    cat("No valid data points for the ggplot.\n")
  }
  
  # Create scatterplots for each feature against predicted values
  for (feature in colnames(testing_data)) {
    if (feature != "locality") {
      ggplot(testing_data, aes_string(x = feature, y = "pred")) +
        geom_point(color = "blue", size = 3) +
        labs(title = paste("Predicted Values by", feature),
             x = feature, y = "Predicted Values") +
        geom_hline(yintercept = mean(all_predictions$Predicted, na.rm = TRUE), linetype = "dashed", color = "red") +
        theme_minimal() -> ggplots[[paste("Predicted by", feature)]]
    }
  }
  
  # Display all the ggplots
  for (name in names(ggplots)) {
    print(ggplots[[name]])
  }
  
} else {
  cat("The 'locality' column is not present in the dataset.\n")
}
