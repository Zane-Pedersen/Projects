
if (!require(forecast)) {
  install.packages("forecast")
  library(forecast)
}

if (!require(zoo)) {
  install.packages("zoo")
  library(zoo)
}


if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

install.packages("dplyr")



# Set the file path
file_path <- "C:/Users/zpdrs/OneDrive/Documents/stu_10_21_05.xlsx"

# Read data
data <- read_excel(file_path)


nValid   <- 4

# Calculate length of 'Sales' data
sales_len <- length(data$Sales)

# Create a time series object from 'Sales'
sales_ts <- ts(data$Sales, start = c(2014, 1), end = c(2014, sales_len), freq = 4)

# Print summary of the time series
summary(sales_ts)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#960659  981343 1000092 1007497 1038191 1069846




# Calculate the number of observations for training
nTrain <- length(sales_ts) - nValid



# Define the sales time series
sales_ts <- ts(data$Sales, start = c(2014, 1), end = c(2014, sales_len), freq = 4)

# Calculate the number of observations for training and create time series windows
nValid <- 8
nTrain <- length(sales_ts) - nValid
train_ts <- window(sales_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(sales_ts, start = c(2014, nTrain + 1), end = c(2014, nTrain + nValid))

# Plot the training series with validation overlap
plot(train_ts, ylab = "Sales", main = "Training Series with Validation Overlap", col = "blue", lwd = 2, type = 'l')
lines(valid_ts, col = "red", lwd = 2)
legend("topleft", legend = c("Training", "Validation"), fill = c("blue", "red"), cex = 0.75)

# Automatically select the best ETS model
auto_ets_model <- ets(train_ts)
auto_ets_pred <- forecast(auto_ets_model, h=nValid, level=c(80,95))
print(accuracy(auto_ets_pred, valid_ts))  # Display accuracy metrics
# Plot the ETS model forecast
plot(auto_ets_pred, main="ETS Model Forecast", ylab="Forecasted Sales", xlab="Time", col = "blue", lwd = 2)
lines(valid_ts, col = "red", lwd = 2)
legend("topright", legend = c("ETS Forecast", "Validation Data"), fill = c("blue", "red"), cex = 0.75)
graphics.off()  

# Extract and store residuals from the ETS model
auto_ets_residuals <- residuals(auto_ets_model)
# Check residuals using built-in diagnostics
checkresiduals(auto_ets_model)
# Display the residuals using ggtsdisplay for a comprehensive graphical analysis
ggtsdisplay(auto_ets_residuals, main="ETS Model Residuals for Labor Costs")

# Forecast the next year using all available sales data
all_ets <- ets(sales_ts, model = "AAA")
all_ets_pred <- forecast(all_ets, h=4, level=c(80,95))
plot(all_ets_pred, main="Next Year Sales Forecast", ylab="Sales", xlab="Time", col = "red", lwd = 2)
legend("topleft", legend = c("ETS Forecast", "Actual Data"), fill = c("red", "blue"), cex = 0.75)

# Extract and print the predicted sales for the next year
all_pred_sales <- all_ets_pred$mean
print(all_pred_sales)

# Manually setting names for the all_pred_sales vector if they are not already set
names(all_pred_sales) <- c("Qtr1 2023", "Qtr2 2023", "Qtr3 2023", "Qtr4 2023")

# Now create the data frame
sales_pred_df <- data.frame(
  Sales = all_pred_sales
)

# Print the new data frame to verify its structure
print(sales_pred_df)

#Qtr1      Qtr2      Qtr3      Qtr4
#2023  980574.6 1009623.7  975333.1 1050262.5
#2024  980778.1 1009786.5  975463.3 1050366.7
############################## LABOR COST #############################################

labor_len <- length(data$Labor)
nValid <- 4
nTrain <- length(sales_ts) - nValid

# Creating the time series object for Labor
lab_ts <- ts(data$Labor, start = c(2014, 1), end = c(2014, labor_len), freq = 4)
summary(lab_ts)

# Calculate the number of training data points
nTrain <- length(lab_ts) - nValid

# Creating training and validation subsets
train_ts <- window(lab_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(lab_ts, start = c(2014, nTrain + 1), end = c(2014, nTrain + nValid))

# Optional: Visualize the training data set
plot_train_ts <- window(lab_ts, start = c(2014, 1), end = c(2014, nTrain + 1))
plot(plot_train_ts, type = 'l', main = "Training Data Set for Labor Costs", xlab = "Time", ylab = "Labor Costs")


#Multiple R-squared: 0.918, a very high value, indicating that about 91.8% of the 
#variability in the dataset is accounted for by the combined effects of trend and seasonality.
#Adjusted R-squared: 0.8994
#F-statistic: 49.28

train_ets <- ets(train_ts, model = "MMM")

ets_pred <- forecast(train_ets, h=nValid, level=c(80,95))
accuracy(ets_pred, valid_ts)
plot(ets_pred, col = "red", lwd = 2)
legend("topright", legend=c("3-Period Moving Avg Forecast", "5-Period Moving Avg Forecast"),
       col=c("red", "blue"), lty=1, lwd=2, cex=0.8)
#           MASE
#Training set 0.5842500
#Test set     0.6553796

# Extract and store residuals from the ETS model
auto_ets_residuals <- residuals(train_ets)

ets_residuals <- residuals(ets_fit)

# Display the residuals using ggtsdisplay
ggtsdisplay(ets_residuals, main="ETS Model Labor Cost Residuals")

graphics.off()  


# finally, predict the next year using all the data
ets_model_labor <- ets(lab_ts, model = "MMM")
ets_model_labor

# Forecast labor costs for the next 8 quarters with 80% and 95% prediction intervals
forecast_labor <- forecast(ets_model_labor, h=4, level=c(80,95))

# Plot the labor cost forecast
plot(forecast_labor, col = "red", lwd = 2, main="Forecast for Labor Costs", ylab="Cost", xlab="Quarter")

# Add a legend to differentiate between training and validation data
legend("topleft", legend = c("Training", "Validation"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))

# Extract and print the mean forecast values for labor costs
predicted_labor_costs <- forecast_labor$mean
predicted_labor_costs

names(predicted_labor_costs) <- c("Qtr1 2023", "Qtr2 2023", "Qtr3 2023", "Qtr4 2023")

# Create a data frame from these values
labor_cost_df <- data.frame(
  Labor_cost = predicted_labor_costs
)

# Print the new data frame to verify its structure
print(labor_cost_df)


 # next year's labor costs
#Qtr1     Qtr2     Qtr3     Qtr4
#2023 230271.0 273629.3 237211.5 311465.5
#2024 229958.5 273258.0 236889.6 311042.8

############################## Materials ####################################
# Define the material cost time series
mat_ts <- ts(data$Material, start = c(2014, 1), end = c(2014, sales_len), freq = 4)

# Prepare training and validation datasets
nTrain <- length(mat_ts) - nValid  # Ensure nTrain is defined similarly
train_ts <- window(mat_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(mat_ts, start = c(2014, nTrain + 1), end = c(2014, nTrain + nValid))

# Fit an ETS model on the training dataset
material_ets_model <- ets(train_ts)

# Forecast and evaluate
material_ets_forecast <- forecast(material_ets_model, h=nValid, level=c(80,95))
print(accuracy(material_ets_forecast, valid_ts))
plot(material_ets_forecast, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Validation"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))

graphics.off()
# Residual analysis
auto_ets_residuals <- residuals(train_ets)

# If you want to check and display residuals, continue with the following
checkresiduals(train_ts)  # Optional: Perform diagnostic checks
ggtsdisplay(auto_ets_residuals, main="ETS Model Material Cost Residuals")

# Fit the ETS model to the entire dataset for future forecasting
full_ets_model <- ets(mat_ts)
future_ets_forecast <- forecast(full_ets_model, h=4, level=c(80,95))

# Print forecasted values
predicted_material_costs <- future_ets_forecast$mean
print(predicted_material_costs)

names(predicted_material_costs) <- c("Qtr1 2023", "Qtr2 2023", "Qtr3 2023", "Qtr4 2023")

# Create a data frame from these values
material_cost_df <- data.frame(
  Material_cost = predicted_material_costs
)

# Print the new data frame to verify its structure
print(material_cost_df)


#Qtr1     Qtr2     Qtr3     Qtr4
#2023 327607.0 370701.3 328035.3 404473.7
# Display the forecast plot for the next periods
plot(future_ets_forecast, col = "red", lwd = 2)
legend("topleft", legend = c("Current Data", "Future Forecast"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))


###############################Fixed ###########################################

# Define the time series for fixed costs
fixed_ts <- ts(data$Fixed, start = c(2014, 1), end = c(2014, sales_len), freq = 4)

# Split the time series into training and validation sets for fixed costs
fixed_train_ts <- window(fixed_ts, start = c(2014, 1), end = c(2014, nTrain))
fixed_valid_ts <- window(fixed_ts, start = c(2014, nTrain + 1), end = c(2014, nTrain + nValid))

fixed_ets_model <- ets(fixed_train_ts)

# Print accuracy of the ETS model for fixed costs against the validation set
fixed_ets_pred <- forecast(fixed_ets_model, h=4, level=c(80,95))
print(accuracy(fixed_ets_pred, fixed_valid_ts))
#                 MASE
#  Training set       0.6312263
#  Test set           0.6749482

# Extract residuals from the fitted ETS model
fixed_ets_residuals <- residuals(fixed_ets_model)

# Plot ACF and PACF for residuals to check for any autocorrelation
par(mfrow=c(2, 1))  # Organize plots in 2 rows, 1 column to display both plots vertically
Acf(fixed_ets_residuals, main="ACF of Fixed Costs ETS Model Residuals")
Pacf(fixed_ets_residuals, main="PACF of Fixed Costs ETS Model Residuals")

# Generate period labels for the forecasts (2023 and 2024)
forecast_labels <- paste("Q", rep(1:4, 2), rep(2023:2024, each=4))

predicted_fixed_costs <- fixed_ets_pred$mean

print(predicted_fixed_costs)

fixed_ets_pred <- forecast(fixed_ets_model, h=8, level=c(80,95))
print(accuracy(fixed_ets_pred, fixed_valid_ts))

# Assuming fixed_ets_pred$mean contains eight values intended for two years (2023 and 2024)
predicted_fixed_costs <- fixed_ets_pred$mean

# Assign names to these forecast values for clarity
names(predicted_fixed_costs) <- c("Q1 2023", "Q2 2023", "Q3 2023", "Q4 2023", "Q1 2024", "Q2 2024", "Q3 2024", "Q4 2024")

# Create a data frame but only include the quarters for 2023
fixed_cost_df <- data.frame(
  FixedCost = predicted_fixed_costs[1:4]        # Select only the fixed cost values for 2023
)

# Print the new data frame to verify its structure
print(fixed_cost_df)



# Plot the ETS model forecast for fixed costs
plot(fixed_ets_pred, col = "red", lwd = 2)
legend_txt <- c("Forecast", "Validation")
legend_col <- c("red", "blue")
legend("topleft", legend = legend_txt, fill = legend_col, cex = 0.75, col = legend_col)

#Qtr1      Qtr2      Qtr3      Qtr4
#2023  98744.36 101119.13  98764.12 100992.07

########################### Profit ############################

profit <- data$Sales - data$Fixed - data$Labor - data$Material
summary(profit)
profit_ts <- ts(profit, start = c(2014, 1), 
                end = c(2014, sales_len), freq = 4)
nValid   <- 4

(nTrain  <- length(sales_ts) - nValid)
train_ts <- window(sales_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(sales_ts, start = c(2014, nTrain + 1),
                   end = c(2014, nTrain + nValid))


train_ets <- ets(train_ts, model = "ZZZ")
ets_pred <- forecast(train_ets, h=nValid, level=c(80,95))
accuracy(ets_pred, valid_ts)
plot(ets_pred, col = "red", lwd = 2)
#MASE       
#Training set 0.6931864        
#Test set     0.5902402   

# finally, predict the next year using all the data
all_ets <- ets(sales_ts, model = "MMM")
all_ets
ets_pred <- forecast(all_ets, h=4, level=c(80,95))
plot(ets_pred, col = "red", lwd = 2)

(all_pred_profit <- ets_pred$mean) # next year's profits
print(all_pred_profit)

# Assign names to the predicted profit values
names(all_pred_profit) <- c("Qtr1 2023", "Qtr2 2023", "Qtr3 2023", "Qtr4 2023")

# Create a data frame from these values
profit_forecast_df <- data.frame(
  Profit = all_pred_profit
)

# Print the new data frame to verify its structure
print(profit_forecast_df)

#VALUATION FORECAST
#Qtr1     Qtr2     Qtr3     Qtr4
#2023  978317.7 1007673.4  974314.7 1048684.5
#NET PROFIT FORECAST
#Qtr1     Qtr2     Qtr3     Qtr4
#2023 323952.3 264174.0 311322.2 233331.2




print(profit_forecast_df)
print(fixed_cost_df)
print(material_cost_df)
print(sales_pred_df)
print(labor_cost_df)

# Calculate profit for each quarter of 2023
profit_2023 <- sales_pred_df$Sales - fixed_cost_df$FixedCost - material_cost_df$Material_cost - labor_cost_df$Labor_cost
print(profit_2023)

# Assign quarter names to the profit values
names(profit_2023) <- paste("Qtr", 1:4, "2023", sep=" ")

# Create a data frame for the profit forecast in 2023
profit_forecast_2023 <- data.frame(
  Profit = profit_2023
)

# Print the profit forecast for 2023
print(profit_forecast_2023)

#> print(profit_forecast_2023)
#Profit
#Qtr 1 2023 323980.4
#Qtr 2 2023 264068.3
#Qtr 3 2023 311186.8
#Qtr 4 2023 233312.9


install.packages("knitr")
library(knitr)

# Combine the outputs into a data frame
forecast_table <- data.frame(
  "Quarter" = c("2023 Q1", "2023 Q2", "2023 Q3", "2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"),
  "Fixed Costs" = predicted_fixed_costs,
  "Material Costs" = predicted_material_costs,
  "Labor Costs" = predicted_labor_costs,
  "Sales" = all_pred_sales
)


# Print the table using kable for a nicer display
kable(forecast_table, format = "markdown")

#|Quarter | Fixed.Costs| Material.Costs| Labor.Costs|     Sales|
#  |:-------|-----------:|--------------:|-----------:|---------:|
#  |2023 Q1 |    98744.36|       327607.0|    230271.0|  980574.6|
#  |2023 Q2 |   101119.13|       370701.3|    273629.3| 1009623.7|
#  |2023 Q3 |    98764.12|       328035.3|    237211.5|  975333.1|
#  |2023 Q4 |   100992.07|       404473.7|    311465.5| 1050262.5|
#  |2024 Q1 |    98744.36|       327607.0|    229958.5|  980778.1|
#  |2024 Q2 |   101119.13|       370701.3|    273258.0| 1009786.5|
#  |2024 Q3 |    98764.12|       328035.3|    236889.6|  975463.3|
#  |2024 Q4 |   100992.07|       404473.7|    311042.8| 1050366.7| 

########################## SECOUND DATA SET ###########################

library(forecast)

# Set the file path
file_path <- "C:/Users/zpdrs/OneDrive/Documents/stu_10_20_09.xlsx"

# Read data
data <- read_excel(file_path)


sales_len <- length(data$Sales)

sales_ts <- ts(data$Sales, start = c(2014, 1), end = c(2014, sales_len), freq = 4)

summary(sales_ts)
# Min.     1st Qu.  Median Mean    3rd Qu.  Max. 
#2000692 2018240 2056289 2080105 2103368 2237386

nValid <- 4

nTrain <- length(sales_ts) - nValid
train_ts <- window(sales_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(sales_ts, start = c(2014, nTrain + 1), end = c(2014, nTrain + nValid))
plot_train_ts <- window(sales_ts, start = c(2014, 1), end = c(2014, nTrain + 1))

auto_ets_model <- ets(train_ts)

# Forecast for the validation period with 80% and 95% confidence intervals
auto_ets_pred <- forecast(auto_ets_model, h=nValid, level=c(80,95))

# Print accuracy metrics
accuracy_metrics <- accuracy(auto_ets_pred, valid_ts)
print(accuracy_metrics)
#     MASE        
#Training set 0.6281567
#Test set     1.093978

plot(auto_ets_pred, main="ETS Model Forecast for Sales", ylab="Sales", xlab="Time", col = "red", lwd = 2)
lines(valid_ts, col = "blue", lwd = 2, type = "l")
legend("bottomright", legend=c("ETS Forecast", "Validation Data"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

checkresiduals(auto_ets_model)

# Extract residuals from the ETS model
auto_ets_residuals <- residuals(auto_ets_model)
ggtsdisplay(auto_ets_residuals, main="ETS Model Residuals for Sales")

# Forecasting for the next year using all the Sales data
all_ets <- ets(sales_ts)
all_ets_pred <- forecast(all_ets, h=4, level=c(80,95))

# Plot the forecast for the next year
plot(all_ets_pred, col = "red", lwd = 2, main="Sales Forecast for Next Year", ylab="Sales", xlab="Quarter")
legend("topleft", legend = c("ETS Forecast", "Actual Data"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))

# Extract and print the forecasted sales for the next year
all_pred_sales <- all_ets_pred$mean
print(all_pred_sales)
#        Qtr1    Qtr2    Qtr3    Qtr4
#2023 2052183 2034593 2041181 2212963

#################################### Labor Costs ###############################

lab_ts <- ts(data$Labor, start = c(2014, 1), end = c(2014, sales_len), freq = 4)
train_ts <- window(lab_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(lab_ts, start = c(2014, nTrain + 1),
                   end = c(2014, nTrain + nValid))

train_lm_trend_season <- tslm(train_ts ~ trend + season)
summary(train_lm_trend_season)

train_ets <- ets(train_ts, model = "ZZZ")
train_ets
ets_pred <- forecast(train_ets, h=8, level=c(80,95))
accuracy_metrics <- accuracy(ets_pred, valid_ts)
print(accuracy_metrics)

# Plot the forecast with confidence intervals
plot(ets_pred, col = "red", lwd = 2, main="Exponential Smoothing Forecast for Labor Costs")
lines(valid_ts, col = "blue", lwd = 2, type = "l")
legend("topright", legend=c("ETS Forecast", "Validation Data"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)
#          MASE 
#Training set 0.7290901
#Test set     0.8969924
# Analyze the residuals of the ETS model
# Extract and store residuals from the ETS model
auto_ets_residuals <- residuals(train_ets)
checkresiduals(train_ets)
ggtsdisplay(auto_ets_residuals, main="ETS Model Residuals for Labor Costs")

# finally, predict the next year using all the data
all_ets <- ets(lab_ts, model = "ZZZ")
all_ets_pred <- forecast(all_ets, h=4, level=c(80,95))
# Forecast for the next year (2023)
ets_pred_2023 <- forecast(all_ets, h=4)

# Plot the forecast for 2023
plot(ets_pred_2023, col = "red", lwd = 2, main="Forecast for Labor Costs for 2023", ylab="Cost", xlab="Quarter")
legend("topleft", legend = c("ETS Forecast", "Actual Data"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))

# Extract and print the predicted labor costs for 2023
predicted_labor_costs_2023 <- ets_pred_2023$mean
print(predicted_labor_costs_2023)
#Qtr1     Qtr2     Qtr3     Qtr4
#2023 538880.7 522601.1 558363.6 692198.3

########################  Materials ####################################

mat_ts <- ts(data$Material, start = c(2014, 1), 
             end = c(2014, sales_len), freq = 4)
train_ts <- window(mat_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(mat_ts, start = c(2014, nTrain + 1),
                   end = c(2014, nTrain + nValid))

auto_ets_model <- ets(train_ts)
auto_ets_pred <- forecast(auto_ets_model, h=nValid, level=c(80,95))
print(accuracy(auto_ets_pred, valid_ts)) 
plot(auto_ets_pred, col = "red", lwd = 2)
         #MASE      
#Training set 0.5936308   
#Test set     0.6304068 
auto_ets_mat_residuals <- residuals(auto_ets_model)
checkresiduals(auto_ets_model) 
ggtsdisplay(auto_ets_mat_residuals, main="ETS Model Residuals for Material Costs")

# Forecast for the next year using all the Material cost data
all_mat_ets <- ets(mat_ts, model = "ZZZ")
all_mat_ets_pred <- forecast(all_mat_ets, h=4, level=c(80,95))
print(all_mat_ets_pred)

auto_ets_mat_residuals <- residuals(auto_ets_model)
checkresiduals(auto_ets_model) 
ggtsdisplay(auto_ets_mat_residuals, main="ETS Model Residuals for Material Costs")

# Plot the forecast for the next year (2023)
plot(all_mat_ets_pred, col = "red", lwd = 2, main="Forecast for Material Costs for 2023", ylab="Cost", xlab="Quarter")
legend("topleft", legend = c("ETS Forecast", "Actual Data"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))


plot(auto_ets_pred, main="Forecasted Values For Material Cost", ylab="Cost", xlab="Time", col = "red", lwd = 2)
lines(valid_ts, col = "blue", lwd = 2, type = "l")
legend("bottomright", legend=c("ETS Forecast", "Validation Data"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

predicted_material_costs_2023 <- all_mat_ets_pred$mean
print(predicted_material_costs_2023)
        #Qtr1     Qtr2     Qtr3     Qtr4
#2023 747958.7 741987.9 746956.9 905658.0

#################################Fixed############################# 
fix_ts <- ts(data$Fixed, start = c(2014, 1), 
             end = c(2014, sales_len), freq = 4)
train_ts <- window(fix_ts, start = c(2014, 1), end = c(2014, nTrain)); auto_ets_model <- ets(train_ts)
auto_ets_pred <- forecast(auto_ets_model, h=nValid, level=c(80,95))
print(accuracy(auto_ets_pred, valid_ts))
plot(auto_ets_pred, main="Automatic ETS Model Forecast", ylab="Forecasted Values", xlab="Time", col = "red", lwd = 2)


fix_ts <- ts(data$Fixed, start = c(2014, 1), end = c(2014, sales_len), freq = 4)

# Create training and validation sets
train_ts <- window(fix_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_ts <- window(fix_ts, start = c(2014, nTrain + 1), end = c(2014, nTrain + nValid))

# Fit an ETS model on the training data
auto_ets_model <- ets(train_ts)

# Forecast for the validation period with 80% and 95% confidence intervals
auto_ets_pred <- forecast(auto_ets_model, h=nValid, level=c(80,95))

# Print accuracy metrics
accuracy_metrics <- accuracy(auto_ets_pred, valid_ts)
print(accuracy_metrics)
#       MASE
#Training set 0.6281567
#Test set     0.8348567

fix_ets_residuals <- residuals(auto_ets_pred)
ggtsdisplay(auto_ets_residuals, main="ETS Model Residuals for Fixed Costs") 

# Plot the forecast against actual validation data for Fixed costs
plot(auto_ets_pred, main="Automatic ETS Model Forecast for Fixed Costs", ylab="Fixed Costs", xlab="Time", col = "red", lwd = 2)
lines(valid_ts, col = "blue", lwd = 2, type = "l")
legend("bottomright", legend=c("ETS Forecast", "Validation Data"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

# Forecasting for the next period using all available Fixed cost data
all_fix_ets <- ets(fix_ts)
all_fix_ets_pred <- forecast(all_fix_ets, h=4, level=c(80,95)) 

# Plot the forecast for the next period (adjust the main title as needed)
plot(all_fix_ets_pred, col = "red", lwd = 2, main="Fixed Cost Forecast for Next Period", ylab="Cost", xlab="Quarter")
legend("topleft", legend = c("ETS Forecast", "Actual Data"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))

# Extract and print the forecasted Fixed costs for the next period
all_pred_fixed <- all_fix_ets_pred$mean
print(all_pred_fixed)

# next year's fixed costs
#        Qtr1      Qtr2      Qtr3      Qtr4
#2023  98891.96 100999.96  98740.47 101088.61



################################## PROFITS #########################################

# Calculate the Profit data as a time series
profit <- data$Sales - data$Fixed - data$Labor - data$Material
summary(profit)
profit_ts <- ts(profit, start = c(2014, 1), freq = 4)
sales_len <- length(profit_ts)

nValid <- 4 
nTrain <- sales_len - nValid

train_ts <- window(profit_ts, start = c(2014, 1), end = c(2014, nTrain))
valid_profit_ts <- window(profit_ts, start = c(2014, nTrain + 1), end = c(2014, sales_len))

auto_ets_profit_model <- ets(train_ts)

auto_ets_profit_forecast <- forecast(auto_ets_profit_model, h=nValid)

profit_accuracy_metrics <- accuracy(auto_ets_profit_forecast, valid_profit_ts)

print(profit_accuracy_metrics)

          #MASE 
#Training set 0.5759653
#Test set     0.6717830

# Plot the forecast against actual validation data for Profit
plot(auto_ets_profit_forecast, main="ETS Model Forecast for Profit", ylab="Profit", xlab="Time", col = "red", lwd = 2)
lines(valid_profit_ts, col = "blue", lwd = 2, type = "l")
legend("bottomright", legend=c("ETS Forecast", "Validation Data"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

# Extract residuals 
auto_ets_profit_residuals <- residuals(auto_ets_profit_model)
checkresiduals(auto_ets_profit_model)

# Forecasting for the next year using all the available profit data
all_ets_profit <- ets(profit_ts)
all_ets_profit_pred <- forecast(all_ets_profit, h=4, level=c(80,95))

# Plot the forecast for the next year
plot(all_ets_profit_pred, col = "red", lwd = 2, main="Profit Forecast for Next Year", ylab="Profit", xlab="Quarter")
legend("topleft", legend = c("ETS Forecast", "Actual Data"), fill = c("red", "blue"), cex = 0.75, col = c("red", "blue"))

all_pred_profit <- all_ets_profit_pred$mean
print(all_pred_profit)
#        Qtr1     Qtr2     Qtr3     Qtr4
#2023 657880.5 662964.7 630572.3 507480.6

(all_pred_profit <- ets_pred$mean) # next year's profits
(net_preds <- all_pred_sales - predicted_labor_costs_2023 - 
    predicted_material_costs_2023 - all_pred_fixed)


#Qtr1     Qtr2     Qtr3     Qtr4
#2023 666451.9 669004.0 637119.9 514017.7
      


