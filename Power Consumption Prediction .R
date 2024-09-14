#Exploring Steel_industry_data
#Data link: https://archive.ics.uci.edu/dataset/851/steel+industry+energy+consumption

# Clear the R workspace/environment
rm(list = ls())

#load data
s_data=read.csv("Steel_industry_data.csv")
names(s_data)

# Display the dimensions (number of rows and columns) of the dataset.
dim(data)

#exclude load_type feature
data = s_data[, c("date","Usage_kWh" ,"Lagging_Current_Reactive.Power_kVarh", "Leading_Current_Reactive_Power_kVarh", 
                  "CO2.tCO2.", "Lagging_Current_Power_Factor", "Leading_Current_Power_Factor" , "NSM", "WeekStatus", 
                  "Day_of_week")]

sum(is.na(data))# Count the number of missing values in the "Salary" column.

str(data)

# Convert categorical features  to numerical.                         
data$WeekStatus=as.numeric(factor(data$WeekStatus))
data$Day_of_week=as.numeric(factor(data$Day_of_week))

#check unique class for each numerical converted categorical features
unique(data$WeekStatus)
unique(data$Day_of_week)

# Create new varaible/feature "Month" based on "Date"
library(lubridate) # Load the lubridate package for date-time manipulation
# Convert the "date" column to a DateTime object
data$date = dmy_hm(data$date)  # Assumes "date" column contains character date-time values

# Extract the month and store it as a numeric variable
Month = month(data$date)
unique(Month)

# Remove the date feature
data$date = NULL

# Combine the original data with the Month
data = cbind(data, Month)
names(data)

#create scatter plots for energy consumption for each month, week status and day
par(mfrow = c(1, 3))
plot(data$Month,data$Usage_kWh,xlab="Month",ylab="Energy Consumption")
plot(data$WeekStatus,data$Usage_kWh,xlab="Week",ylab="Energy Consumption")
plot(data$Day_of_week,data$Usage_kWh,xlab="Day",ylab="Energy Consumption")

#pairs plots
pairs(data)

# Example scatter plot for "Usage_kWh" against "Lagging_Current_Reactive.Power"
plot(s_data$"Lagging_Current_Reactive.Power_kVarh", s_data$Usage_kWh, 
     xlab = "Lagging_Current_Reactive.Power", ylab = "Usage_kWh", 
     main = "Scatter Plot of Usage_kWh vs. Lagging_Current_Reactive.Power")

plot(s_data$"Leading_Current_Reactive_Power_kVarh", s_data$Usage_kWh, 
     xlab = "Leading_Current_Reactive_Power", ylab = "Usage_kWh", 
     main = "Scatter Plot of Usage_kWh vs. Leading_Current_Reactive_Power")

plot(s_data$"CO2.tCO2.", s_data$Usage_kWh, 
     xlab = "CO2.tCO2.", ylab = "Usage_kWh", 
     main = "Scatter Plot of Usage_kWh vs. CO2.tCO2.")

plot(s_data$"Lagging_Current_Power_Factor", s_data$Usage_kWh, 
     xlab = " Lagging_Current_Power_Factor", ylab = "Usage_kWh", 
     main = "Scatter Plot of Usage_kWh vs.  Lagging_Current_Power_Factor")

plot(s_data$NSM, s_data$Usage_kWh, 
     xlab = "NSM", ylab = "Usage_kWh", 
     main = "Scatter Plot of Usage_kWh vs. NSM")

# Calculate the correlation between each feature and "Usage_kWh"
correlation_results <- sapply(data[, -1], function(x) cor(x, s_data$Usage_kWh))

# Display the correlation results
print(correlation_results)

#Display correlation matrix
library(corrplot)
corrplot(cor(data), method="color")

# Create a 70%-30% train-test split
set.seed(1)
test = sample(nrow(data), 0.3 * nrow(data))

# Specify train data
train = (1:nrow(data))[-test] #Select rest of the data i.e 70% other than test data as training data
train_data <- data[train, ]
test_data <- data[test, ]

train_output <- train_data$Usage_kWh
test_output <- test_data$Usage_kWh


##LR MODEL##
# Train a Linear Regression model with all featrures
lm_model <- lm(Usage_kWh ~ ., data = train_data)

# Display a summary of the Linear Regression model
summary(lm_model)

# Make predictions on the test data
predictions <- predict(lm_model, newdata = test_data)

#calculate the MSE:
mse <- (mean((test_output - predictions)^2))

#Calculae R-squared:
r_squared <- 1 - (sum((test_output - predictions)^2) / sum((test_output - mean(test_output))^2))

#print results
cat(" Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", r_squared, "\n")

##RF MODEL##
# Load the randomForest package
library(randomForest)

# Train a Random Forest model with all features
rf_model <- randomForest(Usage_kWh ~ ., data = train_data)

# Print the summary of the Random Forest model 
summary(rf_model)

# Make predictions on the test data
predictions_rf <- predict(rf_model, newdata = test_data)

#calculate the MSE:
mse <- (mean((test_output - predictions_rf)^2))

#Calculae R-squared:
r_squared <- 1 - (sum((test_output - predictions_rf)^2) / sum((test_output - mean(test_output))^2))

#print results
cat(" Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", r_squared, "\n")

##subset selection##
library(leaps)

# Define the response variable
Y <- train_data$Usage_kWh

# Define the predictor variables, excluding the response
X <- train_data[, -1]

# Perform Best Subset Selection
best_subsets <- regsubsets(Y ~ ., data = X, method = "forward")

# Display the summary of the Best Subset Selection
reg.summary1=summary(best_subsets)

#adjr2 (Adusted R-squared)
reg.summary1$adjr2 # Display the Adusted R-squared values for different subsets (8)of predictors.
plot(reg.summary1$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary1$adjr2)
points(8,reg.summary1$adjr2[7],col="red",cex=2,pch=20)

## variable model with adjr2
coef(best_subsets, 8)
summary(best_subsets)

# Extract the names of the predictors for the best subset model with 8 predictors
selected_predictors <- names(coef(best_subsets, 8))
selected_predictors

# Selecting specific columns based on subset selection 
selected_columns <- c("Usage_kWh", "Lagging_Current_Reactive.Power_kVarh", "Leading_Current_Reactive_Power_kVarh",
                      "CO2.tCO2.", "Lagging_Current_Power_Factor", "Leading_Current_Power_Factor",
                      "NSM", "Day_of_week", "Month")

# Subset the train_data with the selected columns
subset_data <- train_data[selected_columns]

# Create the linear model using the selected subset of predictors
subset_model_lr <- lm(Usage_kWh ~ . - Usage_kWh, data = subset_data)

# Print summary of the model
summary(subset_model_lr)

# Apply the subset model to the testing data
Y_pred_subset <- predict(subset_model_lr, newdata = test_data)

# Compare predictions with actual values in test_data (if available)
actual_Y <- test_data$Usage_kWh  # Replace 'Usage_kWh' with your actual response variable in the test data


#calculate the MSE:
mse <- (mean((actual_Y - Y_pred_subset)^2))

#Calculae R-squared:
r_squared <- 1 - (sum((actual_Y - Y_pred_subset)^2) / sum((actual_Y - mean(actual_Y))^2))

#print results
cat(" Mean Squared Error (MSE) for subset_model_lr:", mse, "\n")
cat("R-squared for subset_model_lr:", r_squared, "\n")


#LASSO model ##
#use cross-validation to choose the tuning parameter λ.
set.seed(1)
library(glmnet)

X <- as.matrix(X)
Y <- as.matrix(Y)

cv.out1 = cv.glmnet(X, Y, alpha = 1)
plot(cv.out1)
bestlam_lasso=cv.out1$lambda.min
#print results
cat(" best lambda for lasso model:", bestlam_lasso, "\n")


#find coefficients of best model
best_model_lasso <- glmnet(X, Y, alpha = 1, lambda = bestlam_lasso)
coef(best_model_lasso)

X_test <- test_data[, -1]

# Convert to matrix (if not already)
X_test <- as.matrix(X_test)

# Extract the response variable
Y_test <- test_data$Usage_kWh

#use lasso regression model to predict response value
predictions_lasso=predict(best_model, s = bestlam_lasso, newx = X_test)

# Calculating Mean Squared Error (MSE)
test_mse_lasso <- mean((predictions_lasso - Y_test) ^ 2)
cat(" test mse for lasso model:", test_mse_lasso, "\n")

#Calculae R-squared:
r_squared_lasso <- 1 - (sum((Y_test - predictions_lasso)^2) / sum((Y_test - mean(Y_test))^2))

#print results
cat(" test mse for lasso model:", test_mse_lasso, "\n")
cat(" test r-squared for lasso model:", r_squared_lasso, "\n")


#using bestlam1

lasso.coef1=predict(cv.out1, s = bestlam_lasso, type = "coefficients")[1:9, ]
lasso.coef1

#Zero Coefficients:
lasso.coef1[lasso.coef1==0]

#Non-Zero Coefficients:
lasso.coef1[lasso.coef1!=0] 

#RIDGE##
#use cross-validation to choose the tuning parameter λ.
set.seed(1)
library(glmnet)

cv.out2 = cv.glmnet(X, Y, alpha = 0)
plot(cv.out2)
bestlam_ridge=cv.out2$lambda.min
cat(" best lanbda for ridge model:", bestlam_ridge, "\n")

#using bestlam2
ridge.coef2=predict(cv.out2, s = bestlam_ridge, type = "coefficients")[1:9, ]
ridge.coef2

#Zero Coefficients:
ridge.coef2[ridge.coef2==0]

#Non-Zero Coefficients:
ridge.coef2[ridge.coef2!=0] 

#find coefficients of best model
best_model_ridge <- glmnet(X, Y, alpha = 1, lambda = bestlam_ridge)
coef(best_model_ridge)


#use lasso regression model to predict response value
predictions_ridge=predict(best_model_ridge, s = bestlam_ridge, newx = X_test)

# Calculating Mean Squared Error (MSE)
test_mse_ridge <- mean((predictions_ridge - Y_test) ^ 2)

#Calculae R-squared:
r_squared_ridge <- 1 - (sum((Y_test - predictions_ridge)^2) / sum((Y_test - mean(Y_test))^2))

#print results
cat(" test mse for ridge model:", test_mse_ridge, "\n")
cat(" test r-squared for ridge model:", r_squared_ridge, "\n")

#PCA##
x = model.matrix(Usage_kWh~., data = train_data)
pca = prcomp(x, center = T, scale = F)

# Get the first principal component's loading scores
first_pc_loadings <- pca$rotation[, 1]

# Calculate the contribution of each original feature to the first principal component
feature_contributions <- abs(first_pc_loadings)

# Sort the features by their contributions to the first PC in descending order
sorted_features <- names(sort(feature_contributions, decreasing = TRUE))
sorted_features

# Select the top k features (e.g., top 10)
k <- 7
selected_features_pca <- sorted_features[1:k]
selected_features_pca

# Create a new dataset with only the selected features
selected_data_pcr <- train_data[, c("Usage_kWh", selected_features_pca)]
selected_data_pcr

# Perform PCR with selected features
model_pcr <- lm(Usage_kWh ~ ., data = selected_data_pcr)

# Print the summary of the PCR model
summary(model_pcr)

selected_test_data_pcr <- test_data[, c("Usage_kWh", selected_features_pca)]
selected_test_data_pcr

# Predict Usage_kWh on the test data using the PCR model
predictions_pcr <- predict(model_pcr, newdata = as.data.frame(selected_test_data_pcr))

# Calculate and print the RMSE (Root Mean Square Error)
mse_pcr <- (mean((test_data$Usage_kWh - predictions_pcr)^2))

#Calculae R-squared:
r_squared_pcr <- 1 - (sum((test_data$Usage_kWh - predictions_pcr)^2) / sum((test_data$Usage_kWh - 
                                                                                mean(test_data$Usage_kWh))^2))
#print results
cat(" test mse for PCR model:", mse_pcr, "\n")
cat(" test r-squared for PCR model:", r_squared_pcr, "\n")

##Now we are applying our proposed model: RF using 5-fold CV

# Load the required package
library(randomForest)
library(caret)

# Define the control parameters for 5-fold cross-validation
set.seed(123)
ctrl <- trainControl(method = "cv", number = 5)

# Train the Random Forest model using 5-fold cross-validation on the train_data
rf_model_cv <- train(Usage_kWh ~., data = train_data, method = "rf", trControl = ctrl)

# View the model details and performance from cross-validation
print(rf_model_cv)


# Make predictions on the test_data using the cross-validated model
predictions_cv <- predict(rf_model_cv, newdata = test_data)

# Calculate Mean Squared Error (MSE) on test data using cross-validated model
mse_cv <- mean((test_output - predictions_cv)^2)

# Calculate R-squared on test data using cross-validated model
r_squared_cv <- 1 - sum((test_output - predictions_cv)^2) / sum((test_output - mean(test_output))^2)

# Print the metrics
cat("MSE for RF-Cv model on test data:", mse_cv, "\n")
cat("R-squared for RF-Cv model on test data on test data:", r_squared_cv, "\n")





##Polynomial models###

# Specify the degree of the polynomial
degree <- 2

# Extract numeric predictors (excluding the response variable "Usage_kWh" and non-numeric columns)
numeric_predictors <- train_data[, sapply(train_data, is.numeric) & names(train_data) != "Usage_kWh"]

# Ensure all values in numeric predictors are numeric
numeric_predictors <- sapply(numeric_predictors, as.numeric)

# Check for missing values and replace them with 0
numeric_predictors[is.na(numeric_predictors)] <- 0

# Apply polynomial transformation to numeric predictors
poly_features_train <- as.data.frame(poly(numeric_predictors, degree, raw = TRUE))

# Combine the polynomial features with the original training data
train_data_poly <- cbind(train_data, poly_features_train)

# Train a Polynomial Regression model with the polynomial features
poly_LR_model <- lm(Usage_kWh ~ ., data = train_data_poly)

# Display a summary of the Polynomial Regression model
summary(poly_LR_model)

# Apply the same polynomial transformation to the test data
numeric_predictors_test <- test_data[, sapply(test_data, is.numeric) & names(test_data) != "Usage_kWh"]
numeric_predictors_test <- sapply(numeric_predictors_test, as.numeric)
numeric_predictors_test[is.na(numeric_predictors_test)] <- 0

poly_features_test <- as.data.frame(poly(numeric_predictors_test, degree, raw = TRUE))
test_data_poly <- cbind(test_data, poly_features_test)

# Make predictions on the test data using the polynomial model
predictions_poly <- predict(poly_model, newdata = test_data_poly)

# Evaluate the polynomial model using metrics (e.g., RMSE, R-squared)
mse_poly <- mean((test_output - predictions_poly)^2)
r_squared_poly <- 1 - (sum((test_output - predictions_poly)^2) / sum((test_output - mean(test_output))^2))

cat("Polynomial Regression Model (Using All Features):\n")
cat("Mean Squared Error (MSE):", mse_poly, "\n")
cat("R-squared:", r_squared_poly, "\n")


###Random Forest model with polynomial features#####

# Load required library for Random Forest
library(randomForest)

# Train a Random Forest model
# Assuming "train_data_poly" contains both original and polynomial features
# Define predictors and response variable
predictors_rf <- train_data_poly[, !(names(train_data_poly) %in% c("Usage_kWh"))]
response_rf <- train_data_poly$Usage_kWh

# Train the Random Forest model
rf_model_poly <- randomForest(x = predictors_rf, y = response_rf, ntree = 100)

# Apply the model to the test data
predict_test_rf <- predict(rf_model_poly, newdata = test_data_poly)

# Evaluate the Random Forest model using metrics (e.g., RMSE, R-squared)
mse_rf_poly <- mean((test_output - predict_test_rf)^2)
r_squared_rf_poly <- 1 - (sum((test_output - predict_test_rf)^2) / sum((test_output - mean(test_output))^2))

cat("Polynomial Random Forest Model:\n")
cat("Mean Squared Error (MSE) fpr polynomial Random Forest Model:", mse_rf_poly, "\n")
cat("R-squared for polynomial Random Forest Model:", r_squared_rf_poly, "\n")
