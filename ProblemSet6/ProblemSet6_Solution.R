# Questions 1 ----

# Given Data
alpha <- 0.83
observations <- c(510.31, 488.89, 509.87, 456.72, 473.82, 525.95, 549.83, 542.34)  # Given observations (yt) for 2006 to 2013
levels <- c(510.31)  # Starting level ℓ0

# Calculate level and forecast for each year
for (t in 1:length(observations)) {
    # Update level: ℓt = αyt + (1 - α)ℓt−1
    new_level <- alpha * observations[t] + (1 - alpha) * tail(levels, 1)
    levels <- c(levels, new_level)
}

# Calculate forecasts using levels: Forecast for t+1 is the level at t
forecasts <- levels[-1]  # Exclude the initial level to align forecast periods with t+1

# Print the forecasts
forecasts


# Question 2 ----

# Given data and parameters
alpha <- 0.7
beta <- 0.8
observations <- c(160217.99, 143538.70, 148158.37, 139589.44, 147395.12, 161243.67)  # yt for t=1 to 6
initial_level <- 148112.60  # l0
initial_trend <- 436.57  # b0

# Initialize vectors for levels and trends
levels <- c(initial_level)  # Starting with the initial level
trends <- c(initial_trend)  # Starting with the initial trend
forecasts <- vector("numeric", length(observations) + 1)  # +1 for the initial t=0
forecasts[1] <- NA  # No forecast for t=0

# Calculate level, trend, and forecast for each year
for (t in 1:length(observations)) {  # t goes from 1 to the number of observations
  # Update level
  new_level <- alpha * observations[t] + (1 - alpha) * (levels[t] + trends[t])
  levels <- c(levels, new_level)

  # Update trend
  new_trend <- beta * (new_level - levels[t]) + (1 - beta) * trends[t]
  trends <- c(trends, new_trend)

  # Forecast for one step ahead (h=1)
  new_forecast <- new_level + new_trend  # As h=1
  forecasts[t + 1] <- new_forecast
}

# Output the forecasts
forecasts


# Question 3 ----

# Parameters
alpha <- 0.306
beta <- 0.0003
gamma <- 0.426
seasons <- 4  # Since the data is quarterly

# Initial Values Starting from 2004 Q4
l_t_minus_1 <- 32.26
b_t_minus_1 <- 0.70
s_t_minus_4 <- 1.31  # Same seasonality component one year ago (Q4)

# Historical Observations from 2004 Q1 to 2006 Q4
observations <- c(9.70, -9.31, -1.69, 32.26, 42.21, 24.65, 32.67, 37.26, 73.26, 47.70, 61.10, 66.06)

# Forecast Years and Quarters
forecast_years <- c(2007, 2007, 2007, 2007, 2008, 2008, 2008, 2008)
forecast_quarters <- c("Q1", "Q2", "Q3", "Q4", "Q1", "Q2", "Q3", "Q4")

# Initialize vectors to store calculated components and forecasts
level <- c(l_t_minus_1)
trend <- c(b_t_minus_1)
seasonality <- rep(s_t_minus_4, seasons)  # Repeats the initial seasonality component for each quarter
forecast <- c()

# Update level, trend, and seasonality components
for (obs in observations) {
    l_t <- alpha * (obs - seasonality[length(seasonality) - seasons + 1]) + (1 - alpha) * (tail(level, 1) + tail(trend, 1))
    b_t <- beta * (l_t - tail(level, 1)) + (1 - beta) * tail(trend, 1)
    s_t <- gamma * (obs - l_t) + (1 - gamma) * seasonality[length(seasonality) - seasons + 1]

    level <- c(level, l_t)
    trend <- c(trend, b_t)
    seasonality <- c(seasonality, s_t)
}

# Calculate forecasts for 2007 and 2008
for (i in 1:length(forecast_years)) {
    forecast <- c(forecast, tail(level, 1) + i * tail(trend, 1) + seasonality[length(seasonality) - seasons + i %% seasons])
}

# Match forecasts with years and quarters
forecast_results <- data.frame(Year = forecast_years, Quarter = forecast_quarters, Forecast = forecast)
print(forecast_results)



