# Omer Furkan Coban
# Forecasting Methods - Problem Set 4
# Gas Station Dataset
# WiSe 23/24

# Library Imports ----
library(ggplot2)
library(forecast)

# 1. Create a R script file and import the Station data. ----

gas_station <- read.csv("Datasets/station5108.csv")

# 2. Create  a  time  series  object  for  the  following  variables  crude  oil  prices(cp),
# average diesel prices(pd) and Mean temperature(tg). Note: use full sample! ----

# Converting date to an appropriate format.
gas_station$mydate <- as.Date(gas_station$mydate, format = "%d%b%Y")

ts_cp <- ts(gas_station$cp, start = c(min(gas_station$mydate)), frequency = 365)
ts_p <- ts(gas_station$p, start = c(min(gas_station$mydate)), frequency = 365)
ts_pd <- ts(gas_station$pd, start = c(min(gas_station$mydate)), frequency = 365)
ts_tg <- ts(gas_station$tg, start = c(min(gas_station$mydate)), frequency = 365)

# 3. Graph the above variables and explain with respect to the components of a time series. ----

gas_station <- gas_station[gas_station$pd != -9999.00, ] # Removed some outliers for more meaningful graph

ggplot(gas_station, aes(x = mydate)) +
  geom_line(aes(y = cp, color = "Crude Oil Price")) +
  geom_line(aes(y = p, color = "Average E5 Price")) +
  geom_line(aes(y = pd, color = "Average Diesel Price")) +
  geom_line(aes(y = tg, color = "Mean Temperature"))


# 4. Apply a simple exponential smoothing to the variables that satisfy the properties of SES. ----

# Assuming crude oil prices (cp) are suitable for SES
ses_cp <- ses(ts_cp, h = 20) # Example for crude oil prices

# 5. What is the optimal alpha level? ----

optimal_alpha <- ses_cp$model$par[["alpha"]]
optimal_alpha

# 6. Create 20 ahead forecast. ----

forecast_cp <- forecast(ses_cp, h = 20)
plot(forecast_cp)

# 7. Write the equation for each series ----
# SES
Y[t+1] = α * Y[t] + (1 - α) * Y[t-1]