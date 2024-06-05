#install the required packages
#install.packages(c("ggplot2", "dplyr","plotly"))
# Load the required library
library(ggplot2)
library(dplyr)
library(plotly)

# Reading csv file
eco_data <- read.csv("indianEco.csv")
print(eco_data)
getwd()

# Calculate IQR, variance, and standard deviation for Population
IQR_Population <- IQR(eco_data$Population)
var_Population <- var(eco_data$Population)
sd_Population <- sd(eco_data$Population)

# Print the results
print(paste("IQR for Population:", IQR_Population))
print(paste("Variance for Population:", var_Population))
print(paste("Standard Deviation for Population:", sd_Population))

# Calculate IQR, variance, and standard deviation for InflationConsumerPrices
IQR_InflationConsumerPrices <- IQR(eco_data$InflationConsumerPrices)
var_InflationConsumerPrices <- var(eco_data$InflationConsumerPrices)
sd_InflationConsumerPrices <- sd(eco_data$InflationConsumerPrices)

# Print the results
print(paste("IQR for InflationConsumerPrices:", IQR_InflationConsumerPrices))
print(paste("Variance for InflationConsumerPrices:", var_InflationConsumerPrices))
print(paste("Standard Deviation for InflationConsumerPrices:", sd_InflationConsumerPrices))

# Linear regression
input <- eco_data[,c("GDPPerCapita", "GDPGrowth", "Imports", "Exports", "InflationConsumerPrices", "Population", "PopulationGrowth", "LifeExpectancy")]
model <- lm(GDPPerCapita ~ GDPGrowth + Imports + Exports + InflationConsumerPrices + Population + PopulationGrowth + LifeExpectancy, data = input)
print(summary(model))

# Predicting the values
a <- data.frame(GDPGrowth = 10.91, Imports = 25.60, Exports = 26.35, InflationConsumerPrices = 15.23, Population = 1401243013, PopulationGrowth = 0.67, LifeExpectancy = 70.34)
GDPPerCapita <- predict(model, a)
print(paste("Predicted GDP Per Capita:", GDPPerCapita))

# Calculate the mean squared error
mse <- mean((eco_data$GDPPerCapita - GDPPerCapita)^2)
cat("Mean Squared Error:", mse, "\n")

# Covariance and correlation
library(dplyr)
cov_matrix<-cov(select(eco_data,Imports,Exports))
correlation<-cor(eco_data$Imports,eco_data$Exports)
print("Covariance Matrix:")
print(cov_matrix)
cat("Correlation between Imports and Exports is:",correlation)

#summarise(calc statistic n grp col acc to particular val)
summarise<-eco_data%>%summarise(avg_gdp=mean(GDP),avg_imports=mean(Imports),avg_exports=mean(Exports),
                                avg_tot_reserves=mean(TotalReserves))
print(summarise)

# Filter the data
filtered_data <-eco_data %>%filter(GDPGrowth > 7.0)
print(filtered_data)

# Linear regression for Imports and Exports
lm_model <- lm(Exports ~ Imports, data = eco_data)
print(summary(lm_model))

# Plot Imports vs Exports
png(file="importexport.png")
plot(eco_data$Imports, eco_data$Exports, ylab = "Exports", xlab = "Imports", main = "Imports vs Exports")
dev.off()

png(file="linearimportexport.png")
plot(eco_data$Imports, eco_data$Exports, ylab = "Exports", xlab = "Imports", main = "Imports vs Exports")
abline(lm_model, col = "red")
dev.off()

# Bar Graph for GDPGrowth
png(file="GDP Growth.png")
library(ggplot2)
gdp_growth_bar<-ggplot(eco_data, aes(x = Year, y = GDPGrowth)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "GDP Growth Over the Years", x = "Year", y = "GDP Growth")
print(gdp_growth_bar)
dev.off()

# Line Graph for GDPPerCapita over the years
png(file="gdppercapita.png")
gdp_per_capita_line <- ggplot(eco_data, aes(x = Year, y = GDPPerCapita, group = 1)) +
  geom_line(color = "red") +
  labs(title = "GDP Per Capita Over the Years", x = "Year", y = "GDP Per Capita")
print(gdp_per_capita_line)
dev.off()

# Create a line graph for GDPGrowth
png(file="gdpgrowthlinegraph.png")
line_graph <- ggplot(eco_data, aes(x = Year, y = GDPGrowth, color = "GDP Growth")) +
  geom_line() +
  labs(title = "GDP Growth Over the Years", x = "Year", y = "GDP Growth") +
  theme_minimal()
print(line_graph)
dev.off()

# Create histograms
# Example: GDP Growth histogram
png(file="gdpgrowthhistogram.png")
ggplot(eco_data, aes(x = GDPGrowth)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "GDP Growth Histogram")
dev.off()

# Create scatter plots
# Example: GDP vs GDP Per Capita
png(file="scattergdpvsgdpcapita.png")
ggplot(eco_data, aes(x = GDP, y = GDPPerCapita)) +
  geom_point() +
  labs(title = "GDP vs GDP Per Capita")
dev.off()

# Example: GDP Growth vs Inflation
png(file="scattergdpgrowthvsinflation.png")
ggplot(eco_data, aes(x = GDPGrowth, y = InflationConsumerPrices)) +
  geom_point() +
  labs(title = "GDP Growth vs Inflation")
dev.off()

# For interactive plots (use plotly)
library(plotly)
interactive_line_graph <- plot_ly(eco_data, x = ~Year, y = ~GDPGrowth, type = "scatter", mode = "lines", color = I("blue")) %>%
  layout(title = "Interactive GDP Growth Over the Years", xaxis = list(title = "Year"), yaxis = list(title = "GDP Growth"))

print(interactive_line_graph)

# Create a pie chart for TotalReserves
pie_chart <- plot_ly(eco_data, labels = ~Year, values = ~TotalReserves, type = "pie", hole = 0,
                     textinfo = "percent+label", pull = 0.1)

# Customize the layout
layout <- list(title = "Total Reserves Distribution Over the Years",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Display the pie chart
pie_chart <- pie_chart %>% layout(layout)
print(pie_chart)





##### R PROJECT

df <- read.csv("indianEco.csv")

#convert GDP to be a factor
is.factor(eco_data$GDP)
eco_data$GDP <- as.factor(eco_data$GDP)
is.factor(eco_data$GDP)
levels(eco_data$GDP)

# Set the ratio for training data
training_ratio <- 0.8

# Calculate the index to split the data
split_index <- floor(nrow(df) * training_ratio)

# Create the training and testing sets
train_data <- df[1:split_index, ]
test_data <- df[(split_index + 1):nrow(df), ]
print(train_data)
print(test_data)

# Print the dimensions of the training and testing sets
cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")





####RANDOM FOREST

#install.packages("randomForest")
library(randomForest)

# Select features and target variable
features <- df[c('GDPGrowth', 'Imports', 'Exports', 'TotalReserves', 'InflationConsumerPrices', 'Population', 'PopulationGrowth', 'LifeExpectancy')]
target <- df$GDP

# Create Random Forest model
rf_model <- randomForest(GDP ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)
print(predictions)
test_results <- data.frame(Actual_GDP= test_data$GDP, Predicted_GDP = predictions)
print(test_results)

# Evaluate the model
rf_mse <- mean((predictions - test_data$GDP)^2)
print(paste("Mean Squared Error:", rf_mse))

# Calculate RMSE
rf_rmse <- sqrt(mean((test_data$GDP - predictions)^2))

# Print RMSE
print(paste("Root Mean Squared Error (RMSE):", rf_rmse))





#####LINEAR REGRESSION MODEL

# Build the linear regression model
model <- lm(GDP ~ GDPPerCapita + Imports + GDPGrowth + Exports + TotalReserves + InflationConsumerPrices +
              Population + PopulationGrowth + LifeExpectancy, data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)
print(predictions)
test_results <- data.frame(Actual_GDP= test_data$GDP, Predicted_GDP = predictions)
print(test_results)

# Evaluate the model
lm_mse <- mean((test_data$GDP - predictions)^2)
print(paste("Mean Squared Error:", lm_mse))

# Calculate RMSE
lm_rmse <- sqrt(mean((test_data$GDP - predictions)^2))

# Print RMSE
print(paste("Root Mean Squared Error (RMSE):", lm_rmse))

# Install and load necessary libraries if not already installed
#install.packages("ggplot2")
library(ggplot2)

# Plot of Actual vs Predicted GDP
png(file="linear_regression.png")
ggplot(test_results, aes(x = Actual_GDP, y = Predicted_GDP)) + geom_point(color = "blue") + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +labs(title = "Actual vs Predicted GDP",
                                                                                                                                                                            x = "Actual GDP", y = "Predicted GDP") +theme_minimal()
dev.off()

# Plot of GDP vs Year
# Convert Year to numeric
df$Year <- as.numeric(df$Year)
png(file="linear_regression_over_years.png")
ggplot(df,aes(x=Year,y=GDP))+geom_point()+geom_smooth(method="lm",se=FALSE)+labs(title="Linear Regression Model for GDP Over Years",x = "Year",y="GDP")+theme_minimal()
dev.off()





#####POLYNOMIAL REGRESSION MODEL

# Define the features
#features <- c("Year","GDPPerCapita", "GDPGrowth", "Imports", "Exports", "TotalReserves", "InflationConsumerPrices", "Population", "PopulationGrowth", "LifeExpectancy")

# Build a polynomial regression model
poly_degree <- 2
poly_model <- lm(GDP ~ poly(Year, degree) + poly(GDPPerCapita, degree) +
                   poly(GDPGrowth, degree) + poly(Imports, degree) +
                   poly(Exports, degree) + poly(TotalReserves, degree) +
                   poly(InflationConsumerPrices, degree) + poly(Population, degree) +
                   poly(PopulationGrowth, degree) + poly(LifeExpectancy, degree), data = train_data)
#formula <- as.formula(paste("GDP ~ poly(", paste(features, collapse = " + "), ", degree = ", poly_degree, ")", sep = ""))
polynomial_model <- lm(poly_model, data = train_data)

summary(polynomial_model)

# Make predictions on the test set
test_predictions <- predict(polynomial_model, newdata = test_data)
print(test_predictions)
test_results <- data.frame(Actual_GDP = test_data$GDP, Predicted_GDP = test_predictions)
print(test_results)

#calculate the mean squared error
poly_mse <- mean((test_data$GDP - test_predictions)^2)
cat("Mean Squared Error:", poly_mse, "\n")

# Calculate RMSE
poly_rmse <- sqrt(mean((test_data$GDP - test_predictions)^2))

# Print RMSE
cat("Root Mean Squared Error (RMSE):", poly_rmse, "\n")


# Plot of Actual vs Predicted GDP
library(ggplot2)
png(file="polynomial_regression.png")
ggplot(test_results, aes(x = Actual_GDP, y = Predicted_GDP)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ poly(x, poly_degree)) +
  labs(title = "Polynomial Regression",
       x = "Actual GDP",
       y = "Predicted GDP") +
  theme_minimal()
dev.off()

# Plot of GDP vs Year
library(ggplot2)

# Convert Year to numeric
df$Year <- as.numeric(df$Year)
png(file="polynomial_regression_over_years.png")
# Create a ggplot with polynomial regression (quadratic)
ggplot(df,aes(x = Year, y = GDP))+geom_point()+geom_smooth(method="lm",formula=y~poly(x,2),se = FALSE)+labs(title="Polynomial Regression (Quadratic) for GDP Over Years",x="Year",y="GDP")+theme_minimal()
dev.off()





#RIDGE REGRESSION

train_data$InteractionTerm <- train_data$GDPPerCapita * train_data$GDPGrowth
print(train_data)
#install.packages("glmnet")
features <- c("Year","GDPPerCapita", "GDPGrowth", "Imports", "Exports", "TotalReserves", "InflationConsumerPrices", "Population", "PopulationGrowth", "LifeExpectancy")

library(glmnet)
ridge_model <- cv.glmnet(as.matrix(train_data[, features]), train_data$GDP, alpha = 0)

# Print the optimal lambda value from cross-validation
cat("Optimal Lambda:", ridge_model$lambda.min, "\n")

# Build Ridge regression model
ridge_regression_model <- glmnet(as.matrix(train_data[, features]), train_data$GDP, alpha = 0, lambda = ridge_model$lambda.min)

# Make predictions on the test set
test_predictions_ridge <- predict(ridge_regression_model, newx = as.matrix(test_data[, features]))
print(test_predictions_ridge )


#to create two columns for predicted and actual GDP
test_results <- data.frame(Actual_GDP = test_data$GDP, Predicted_GDP = test_predictions_ridge)

# Display the predicted GDP values
print(test_results)

# Mean Squared Error
mse_ridge <- mean((test_data$GDP - test_predictions_ridge)^2)
cat("Mean Squared Error (Ridge):", mse_ridge, "\n")

#RMSE
# Calculate RMSE for Ridge regression
rmse_ridge <- sqrt(mean((test_data$GDP - test_predictions_ridge)^2))

# Print RMSE for Ridge regression
cat("Root Mean Squared Error (Ridge):", rmse_ridge, "\n")

# Plot of Actual vs Predicted GDP
# install.packages("ggplot2")
library(ggplot2)

ridge_test_results <- data.frame(Actual_GDP = test_data$GDP, Predicted_GDP = test_predictions_ridge)
print(ridge_test_results)

# Scatter plot of Actual vs Predicted GDP for Ridge Regression
png(file="ridge_regression.png")
ggplot(ridge_test_results, aes(x = Actual_GDP, y = s0)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted GDP (Ridge Regression)",
       x = "Actual GDP",
       y = "Predicted GDP") +
  theme_minimal()
dev.off()

# Plot of GDP vs Year
# Load necessary libraries
library(ggplot2)
library(glmnet)

# Plotting the Ridge regression model predictions
png(file="ridge_regression_over_years.png")
plot(test_data$Year, test_data$GDP, col = "blue", pch = 20, main = "Ridge Regression Predictions and Actual GDP vs Year",
     xlab = "Year", ylab = "GDP", ylim = c(min(test_data$GDP, test_predictions_ridge), max(test_data$GDP, test_predictions_ridge)))

# Add lines for both actual GDP and Ridge regression predictions
lines(test_data$Year, test_data$GDP, col = "blue", lwd = 2)
lines(test_data$Year, test_predictions_ridge, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Actual GDP", "Predicted GDP (Ridge)"), col = c("blue", "red"), pch = 20)

# Show the plot
dev.off()



#####LASSO REGRESSION MODEL

# install.packages("glmnet")
library(glmnet)
df<-read.csv("indianEco.csv")
X <- df[, c("GDPPerCapita", "GDPGrowth", "Imports", "Exports", "TotalReserves", "InflationConsumerPrices", "Population", "PopulationGrowth", "LifeExpectancy")]
y <- df$GDP

# Standardize features
X_scaled <- scale(X)
## 0 MEAN and UNIT VARIANCE..This ensures that all the features contribute equally to the training process

# Set the ratio for training data
training_ratio <- 0.8

# Calculate the index to split the data
split_index <- floor(nrow(df) * training_ratio)

# Create the training and testing sets
train_data <- df[1:split_index, ]
test_data <- df[(split_index + 1):nrow(df), ]
print(train_data)
print(test_data)

# Standardize the features in training and testing sets
train_data_scaled <- scale(train_data[, c("GDPPerCapita", "GDPGrowth", "Imports", "Exports", "TotalReserves", "InflationConsumerPrices", "Population", "PopulationGrowth", "LifeExpectancy")])
test_data_scaled <- scale(test_data[, c("GDPPerCapita", "GDPGrowth", "Imports", "Exports", "TotalReserves", "InflationConsumerPrices", "Population", "PopulationGrowth", "LifeExpectancy")])
print(train_data_scaled)
print(test_data_scaled)

# Fit Lasso regression model : cross validation
lasso_model <- cv.glmnet(as.matrix(train_data_scaled), train_data$GDP, alpha = 1)

# Make predictions on the test set
predictions <- predict(lasso_model, newx = as.matrix(test_data_scaled))

# Print the predicted GDP values
cat("Predicted GDP:",predictions, "\n")
test_results <- data.frame(Actual_GDP= test_data$GDP, Predicted_GDP = predictions)
print(test_results)

# Evaluate the model
lasso_mse <- mean(( predictions - test_data$GDP)^2)
cat("Mean Squared Error:", lasso_mse, "\n")

# Calculate RMSE for Lasso regression
lasso_rmse <- sqrt(lasso_mse)

# Print RMSE for Lasso regression
cat("Root Mean Squared Error (Lasso):", lasso_rmse, "\n")

# Plot of Actual vs Predicted GDP
#install.packages("ggplot2")
library(ggplot2)

# Create a data frame with actual and predicted GDP values
plot_data <- data.frame(Actual_GDP = test_data$GDP, Predicted_GDP = as.vector(predictions))

# Calculate Mean Squared Error (MSE)
mse <- mean((plot_data$Actual_GDP - plot_data$Predicted_GDP)^2)

# Plot the scatter plot between Actual and Predicted GDP with a line and MSE information
png(file="lasso_regression.png")
ggplot(plot_data, aes(x = Actual_GDP, y = Predicted_GDP)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid", formula = y ~ x) +
  labs(title = "Actual vs Predicted GDP (Lasso Regression)",
       x = "Actual GDP",
       y = "Predicted GDP") +
  theme_minimal()
dev.off()

#Plot of GDP vs Year
#install.packages("ggplot2")
library(ggplot2)

# Assuming your dataframe includes a column for years, adjust the column name accordingly
years <- test_data$Year

# Create a data frame with actual and predicted GDP values along with years
plot_data <- data.frame(Year = years, Actual_GDP = test_data$GDP, Predicted_GDP = as.vector(predictions))

# Plot the scatter plot between GDP and Year with connecting lines
png(file="lasso_regression_over_years.png")
ggplot(plot_data, aes(x = Year, y = Actual_GDP)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_point(aes(y = Predicted_GDP), color = "red", size = 3, alpha = 0.8) +
  geom_line(aes(y = Actual_GDP), color = "blue", size = 1, linetype = "solid" ) +
  geom_line(aes(y = Predicted_GDP), color = "red", size = 1, linetype = "dashed") +
  labs(title = "Actual and Predicted GDP vs Year(Lasso Regression)",
       x = "Year",
       y = "GDP") +
  scale_color_manual(values = c("Actual GDP" = "blue", "Predicted GDP" = "red"),
                     labels = c("Actual GDP", "Predicted GDP")) +
  theme_minimal()
dev.off()





#####POISSON REGRESSION MODEL

# Specify predictors
predictors <- c("Year", "GDPPerCapita", "GDPGrowth", "Imports", "Exports", "TotalReserves", "InflationConsumerPrices", "Population", "PopulationGrowth", "LifeExpectancy")

# Fit a Poisson regression model
poisson_model <- glm(train_data$GDP ~ ., data = train_data[, predictors], family = "poisson")

# Make predictions on the testing set
poisson_predict <- predict(poisson_model, newdata = test_data[, predictors], type = "response")
test_results <- data.frame(Actual_GDP= test_data$GDP, Predicted_GDP = poly_predict)

# Display the predicted GDP values
print(test_results)

# Calculate Mean Squared Error
poisson_mse<- mean((poisson_predict - test_data$GDP)^2)
print(poisson_mse)

# Calculate RMSE for Poisson regression
poisson_rmse <- sqrt(mean((poisson_predict - test_data$GDP)^2))

# Print RMSE for Poisson regression
cat("Root Mean Squared Error (Poisson):", poisson_rmse, "\n")

# Plot of Actual vs Predicted GDP
# install.packages("ggplot2")
library(ggplot2)

poisson_test_results <- data.frame(Actual_GDP = test_data$GDP, Predicted_GDP = poisson_predict)
png(file="poison_regression.png")
ggplot(poisson_test_results, aes(x = Actual_GDP, y = Predicted_GDP)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted GDP (Poisson Regression)",
       x = "Actual GDP",
       y = "Predicted GDP") +
  theme_minimal()
dev.off()

#Plot of GDP vs Year
# Load necessary libraries
library(ggplot2)

# Plot a Poisson prediction graph for GDP vs Year
png(file = "poison_regression_over_years.png")
ggplot(test_results, aes(x = test_data$Year, y = Actual_GDP)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(x = test_data$Year, y = Actual_GDP), color = "blue", size = 1, group = 1) +
  geom_line(aes(x = test_data$Year, y = Predicted_GDP), color = "red", size = 1) +
  labs(title = "Poisson Regression Predictions for GDP Over Years", x = "Year", y = "GDP") +
  theme_minimal()
dev.off()





####ARTIFICIAL NEURAL NETWORK

#install.packages("neuralnet")

# Install and load necessary packages
if (!require(neuralnet)) {
  install.packages("neuralnet")
}
library(neuralnet)

# Assuming `data` is your dataset and is already loaded
# Normalize the numeric columns (excluding Year and Population)
num_cols <- colnames(eco_data )[sapply(eco_data , is.numeric) & !(colnames(eco_data ) %in% c("Year", "Population"))]
data_norm <- eco_data
data_norm[, num_cols] <- scale(data_norm[, num_cols])

# Split data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(data_norm), size = 0.7 * nrow(data_norm))
train_data <- data_norm[train_index, ]
test_data <- data_norm[-train_index, ]

# Create and train the neural network
nn_formula <- GDP ~ GDPPerCapita + GDPGrowth + Imports + Exports + TotalReserves + InflationConsumerPrices + Population + PopulationGrowth + LifeExpectancy
nn_model <- neuralnet(nn_formula,
                      data = train_data,
                      hidden = c(5, 3),  # Number of hidden layers and neurons
                      linear.output = TRUE)  # For regression tasks

# Make predictions on the test set
predictions <- compute(nn_model, test_data[, -which(colnames(test_data) == "GDP")])$net.result

# Calculate the mean squared error (MSE) as a measure of performance
ANN_mse <- mean((test_data$GDP - predictions)^2)
print(paste("Mean Squared Error:", ANN_mse))

# Calculate RMSE
ANN_rmse <- sqrt(ANN_mse)
print(paste("Root Mean Squared Error (RMSE):", ANN_rmse))


#### ACCURACY
eco_data <- read.csv("indianEco.csv")

# Assuming range_GDP is the range of your GDP variable
range_GDP <- max(eco_data$GDP) - min(eco_data$GDP)


# Linear Regression Model
percentage_accuracy_linear <- 100 * (1 - (lm_mse / range_GDP^2))
cat("Percentage Accuracy of Linear Regression Model:", percentage_accuracy_linear, "%\n")

# Polynomial Regression Model
percentage_accuracy_poly <- 100 * (1 - (poly_mse / range_GDP^2))
cat("Percentage Accuracy of Polynomial Regression Model:", percentage_accuracy_poly, "%\n")

# Ridge Regression Model
percentage_accuracy_ridge<- 100 * (1 - (mse_ridge / range_GDP^2))
cat("Percentage Accuracy of Ridge Regression Model:", percentage_accuracy_ridge, "%\n")

# Random Forest Model
percentage_accuracy_rf<- 100 * (1 - (rf_mse / range_GDP^2))
cat("Percentage Accuracy of Random Forest Model:", percentage_accuracy_rf, "%\n")

# Lasso Regression Model
percentage_accuracy_lasso<- 100 * (1 - (lasso_mse / range_GDP^2))
cat("Percentage Accuracy of Lasso Regression Model:", percentage_accuracy_lasso, "%\n")

# Poisson Regression Model
percentage_accuracy_poisson <- 100 * (1 - (poisson_mse / range_GDP^2))
cat("Percentage Accuracy of Poisson Regression Model:", percentage_accuracy_poisson, "%\n")

# AI Nueral Network
percentage_accuracy_ANN<- 100 * (1 - (ANN_mse / range_GDP^2))
cat("Percentage Accuracy of ANN Model:", percentage_accuracy_ANN, "%\n")


#checking which regression model is more accurate for indian economy dataset

if(lm_mse<poly_mse & lm_mse<poisson_mse & lm_mse<lasso_mse & lm_mse<rf_mse & lm_mse<ANN_mse & lm_mse<mse_ridge){
  print("linear regression model is more accurate for the dataset")
}else if(poly_mse<lm_mse & poly_mse<poisson_mse & poly_mse<lasso_mse & poly_mse<rf_mse & poly_mse<ANN_mse &poly_mse<mse_ridge){
  print("polynomial regression model is more accurate for the dataset")
}else if(poisson_mse<lm_mse & poisson_mse<poly_mse & poisson_mse<lasso_mse & poisson_mse<rf_mse & poisson_mse<ANN_mse & poisson_mse<mse_ridge){
  print("poisson regression model is more accurate for the dataset")
}else if(lasso_mse<lm_mse & lasso_mse<poly_mse & lasso_mse<poisson_mse & lasso_mse<rf_mse & lasso_mse<ANN_mse & lasso_mse<mse_ridge){
  print("lasso regression model is more accurate for the dataset")
}else if(rf_mse<lm_mse & rf_mse<poly_mse & rf_mse<poisson_mse & rf_mse<lasso_mse & rf_mse<ANN_mse &rf_mse<mse_ridge){
  print("Random Forest model is more accurate for the dataset")
}else if(ANN_mse<lm_mse & ANN_mse<poly_mse &ANN_mse<poisson_mse & ANN_mse<lasso_mse & ANN_mse<rf_mse & ANN_mse<mse_ridge){
  print("ANN model is more accurate for the dataset")
}else{
  print("Ridge regression model is more accurate for the dataset")
}
