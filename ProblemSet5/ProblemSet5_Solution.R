# Omer Furkan Coban
# Forecasting Methods - Problem Set 5
# International Tourists to Australia: Total visitor nights.
# WiSe 23/24

# Library Imports ----
install.packages("fpp2")
library(fpp2) # To load "austourists" dataset.
library(forecast)

head(austourists)
summary(austourists)

# 1. Create  a  R  script  file  library  required  packages  and  use  R  data  set  calledaustourists. ----

data("austourists")
head(austourists)
summary(austourists)

# 2. Create a time series object beginning 2005. Hint: use window command. ----

ts_data <- window(austourists, start=c(2005,1))

# 3. Graph and explain with respect to the components of a time series. ----

plot(ts_data)

decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)

# 4. Which model would be appropriate. ----

# ETS or Holt-Winters

# 5. Use both additive and multiplicative Holt’s Winter approach. ----

hw_additive <- hw(ts_data, seasonal="additive")
hw_multiplicative <- hw(ts_data, seasonal="multiplicative")

# 6. Compare the two models using RMSE, which model is the best fit. ----

rmse_additive <- sqrt(mean(hw_additive$residuals^2))
rmse_multiplicative <- sqrt(mean(hw_multiplicative$residuals^2))

# 7. Compute by R (using the formula) forecast for 2016-2017 ----

forecast_additive <- forecast(hw_additive, h=8)  # 'h' is the number of periods to forecast
forecast_multiplicative <- forecast(hw_multiplicative, h=8)

print(forecast_additive)
print(forecast_multiplicative)
print(rmse_additive)
print(rmse_multiplicative)

