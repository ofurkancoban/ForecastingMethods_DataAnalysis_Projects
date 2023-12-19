# Omer Furkan Coban
# Forecasting Methods - Problem Set 2
# Germany Import - Export
# WiSe 23/24

# Library Imports ----
# No need extra libraries.

# 1. Create a R script file and import the expo−impo−germany data. ----
# Reading the "expo-impo-germany" dataset into a variable 'ger_exim' using 'read.csv' function.
ger_exim <- read.csv("Datasets/expo-impo-germany.csv")

# 2. Create time series for all four variables and plot them. ----

# To do that I need to seperate time variable combined year-month format into distinct year and month components.
# To seperate the 'time' variable I use the sub() expression.
# # sub("m.*", "", ger_eximtime) is used to remove everything after and including 'm', leaving us with just the year.
# # sub(".*m", "", ger_exim$time) is used to remove everything before and including 'm', leaving us with the month.

# Extracting year and month from 'time'
ger_exim$year <- as.integer(sub("m.*","",ger_exim$time))

ger_exim$month <- as.integer(sub(".*m","",ger_exim$time))


# To create Time Series, I use ts() function to convert relevant columns into time series objects.
# # ts() function requires start time and frequency. I used min() function to choose earliest month and year as start time.
# # The frequency parameter is set to 12 to indicate monthly data.

exports_ts <- ts(ger_exim$exports, start=c(min(ger_exim$year),min(ger_exim$month)), frequency = 12)
imports_ts <- ts(ger_exim$imports, start=c(min(ger_exim$year),min(ger_exim$month)), frequency = 12)
p_exports_ts <- ts(ger_exim$p_exports, start=c(min(ger_exim$year),min(ger_exim$month)), frequency = 12)
p_imports_ts <- ts(ger_exim$p_imports, start=c(min(ger_exim$year),min(ger_exim$month)), frequency = 12)

# Plot Time Series
plot(exports_ts, main="Exports Time Series")
plot(imports_ts, main="Imports Time Series")
plot(p_exports_ts, main="Export Prices Time Series")
plot(p_imports_ts, main="Import Prices Time Series")

# 3. Explain the plots with respect to the components of a time series. ----

# 1. Trend Component
# The trend component reflects the long-term progression of the series.
# In the context of exports and imports, this would show whether there is a general increase or decrease over time.
# For price indices, the trend would indicate inflationary or deflationary patterns.

# Exports and Imports: An upward or downward sloping trend would indicate an increase or decrease in trade volume over time.
# Export and Import Prices: A rising trend might indicate inflationary pressures, while a declining trend could suggest
# deflation or decreasing cost pressures in international trade.

# 2. Cyclical Component
# The cyclical component represents fluctuations tied to business or economic cycles.
# These are not regular seasonal effects but can span several years, often related to broader economic conditions.

# Exports and Imports: Fluctuations in these series might correlate with economic expansions and recessions.
# For instance, a prolonged dip followed by a recovery could represent a business cycle.
# Export and Import Prices: Cyclical trends in prices might be tied to global commodity price changes or shifts in global economic conditions.

# 3. Seasonal Component
# Seasonal components are regular fluctuations that occur at specific intervals (e.g., monthly, quarterly).

# Exports and Imports: You might observe seasonal patterns, such as an increase in exports or imports during certain months of the year,
# perhaps due to holiday seasons or agricultural cycles.
# Export and Import Prices: Seasonal variations might be due to factors like seasonal demand changes,
# harvesting seasons, or holiday periods impacting trade.

# 4. Residual Component
# Residuals are what remain after the trend, cyclical, and seasonal components have been accounted for. These are irregular and not explained by the model used.

# Exports and Imports: Residuals in these series could be due to unexpected events like political changes, sudden demand shifts, or supply chain disruptions.
# Export and Import Prices: Residual fluctuations might arise from unforeseen market events, policy changes, or other non-systematic factors.

# 4.  Generate real export and import prices hint:[(actualvalue/price)×100] ----
ger_exim$real_export_prices <- (ger_exim$exports/ger_exim$p_exports) * 100
ger_exim$real_import_prices <- (ger_exim$imports/ger_exim$p_imports) * 100

# 5.  Create time series for the new variables and plot them. ----

real_export_prices_ts <- ts(ger_exim$real_export_prices, start = c(min(ger_exim$year), min(ger_exim$month)),frequency = 12)
real_import_prices_ts <- ts(ger_exim$real_import_prices, start = c(min(ger_exim$year), min(ger_exim$month)),frequency = 12)

plot(real_export_prices_ts, main="Real Export Prices Time Series")
plot(real_import_prices_ts, main="Real Import Prices Time Series")


# 6.  Explain the plots with respect to the components of a time series. ----

# 1. Trend Component
# The trend in the time series of real export and import prices reflects long-term movements after accounting for price changes.

# Real Export Prices: If the trend is upward, it suggests that the value of exports is increasing over time relative
# to export prices, indicating potentially growing export volumes or higher value exports. A downward trend could indicate the opposite.
# Real Import Prices: Similarly, an upward trend would suggest increasing import volumes or values relative to import prices,
# while a downward trend might indicate a reduction.

# 2. Cyclical Component
# Cyclical fluctuations reflect broader economic cycles and are not as regular as seasonal patterns.

# Real Export Prices: Fluctuations here could be tied to global economic conditions, demand for exported goods, or changes in international trade policies.
# Real Import Prices: Variations might correlate with domestic economic conditions, changes in global market prices, or shifts in import policies.

# 3. Seasonal Component
# Seasonal components are regular, periodic fluctuations within a year.

# Real Export Prices: You might observe seasonal patterns due to factors like annual trade agreements, holiday seasons affecting demand,
# or agricultural export cycles.
# Real Import Prices: Seasonal variations could be due to changes in domestic demand during certain times of the year,
# such as increased imports during holiday seasons or specific weather-related import needs.

# 4. Residual Component
# Residuals are the irregular, unexplained variations after accounting for trend, cyclical, and seasonal components.

# Real Export Prices: These could include unexpected geopolitical events, sudden changes in foreign markets,
# or other unforeseen factors impacting export values.
# Real Import Prices: Residuals here might arise from sudden supply chain disruptions, unexpected changes in domestic demand, or other random events.

# 7. Fit a Linear trend for the new generated variables. ----
linear_model_export <- lm(real_export_prices_ts ~ time(real_export_prices_ts))
linear_model_import <- lm(real_import_prices_ts ~ time(real_import_prices_ts))

# 8. Predict the variables and generate the residuals. ----
predictions_linear_export <- predict(linear_model_export)
residuals_linear_export <- real_export_prices_ts - predictions_linear_export

predictions_linear_import <- predict(linear_model_import)
residuals_linear_import <- real_import_prices_ts - predictions_linear_import

# 9. Fit a polynomial trend curve from order 2 to 5, predict and generate residuals. ----

# Create a time variable for the time series
time_export <- time(real_export_prices_ts)
time_import <- time(real_import_prices_ts)

print(length(time_export))
print(length(real_export_prices_ts))

# If the lengths are different, adjust time_export to match real_export_prices_ts
# For example, if real_export_prices_ts is shorter:
time_export <- time_export[seq_along(real_export_prices_ts)]
time_import <- time_import[seq_along(real_import_prices_ts)]



# Polynomial models for exports
poly_model_export_2 <- lm(real_export_prices_ts ~ poly(time_export, 2))
predictions_poly_export_2 <- predict(poly_model_export_2)
residuals_poly_export_2 <- real_export_prices_ts - predictions_poly_export_2

poly_model_export_3 <- lm(real_export_prices_ts ~ poly(time_export, 3))
predictions_poly_export_3 <- predict(poly_model_export_3)
residuals_poly_export_3 <- real_export_prices_ts - predictions_poly_export_3

poly_model_export_4 <- lm(real_export_prices_ts ~ poly(time_export, 4))
predictions_poly_export_4 <- predict(poly_model_export_4)
residuals_poly_export_4 <- real_export_prices_ts - predictions_poly_export_4

poly_model_export_5 <- lm(real_export_prices_ts ~ poly(time_export, 5))
predictions_poly_export_5 <- predict(poly_model_export_5)
residuals_poly_export_5 <- real_export_prices_ts - predictions_poly_export_5

# Polynomial models for imports
poly_model_import_2 <- lm(real_import_prices_ts ~ poly(time_import, 2))
predictions_poly_import_2 <- predict(poly_model_import_2)
residuals_poly_import_2 <- real_import_prices_ts - predictions_poly_import_2

poly_model_import_3 <- lm(real_import_prices_ts ~ poly(time_import, 3))
predictions_poly_import_3 <- predict(poly_model_import_3)
residuals_poly_import_3 <- real_import_prices_ts - predictions_poly_import_3

poly_model_import_4 <- lm(real_import_prices_ts ~ poly(time_import, 4))
predictions_poly_import_4 <- predict(poly_model_import_4)
residuals_poly_import_4 <- real_import_prices_ts - predictions_poly_import_4

poly_model_import_5 <- lm(real_import_prices_ts ~ poly(time_import, 5))
predictions_poly_import_5 <- predict(poly_model_import_5)
residuals_poly_import_5 <- real_import_prices_ts - predictions_poly_import_5

# 10. Plot the predicted values for all the polynomials. ----


# For Real Export Prices
plot(time_export, real_export_prices_ts, type = "l", col = "black", main = "Real Export Prices with Polynomial Predictions", ylab = "Real Export Prices", xlab = "Time")
lines(time_export, predictions_poly_export_2, col = "red")
lines(time_export, predictions_poly_export_3, col = "blue")
lines(time_export, predictions_poly_export_4, col = "green")
lines(time_export, predictions_poly_export_5, col = "purple")
legend("topright", legend = c("Actual", "Poly 2", "Poly 3", "Poly 4", "Poly 5"), col = c("black", "red", "blue", "green", "purple"), lty = 1, cex = 0.8)

# For Real Import Prices
plot(time_import, real_import_prices_ts, type = "l", col = "black", main = "Real Import Prices with Polynomial Predictions", ylab = "Real Import Prices", xlab = "Time")
lines(time_import, predictions_poly_import_2, col = "red")
lines(time_import, predictions_poly_import_3, col = "blue")
lines(time_import, predictions_poly_import_4, col = "green")
lines(time_import, predictions_poly_import_5, col = "purple")
legend("topright", legend = c("Actual", "Poly 2", "Poly 3", "Poly 4", "Poly 5"), col = c("black", "red", "blue", "green", "purple"), lty = 1, cex = 0.8)


# Step 11: Standardize Residuals and Create Histogram ----

# Standardize Residuals and Create Histogram for Exports
# Polynomial Order 2 - Exports
std_residuals_poly_export_2 <- (residuals_poly_export_2 - mean(residuals_poly_export_2)) / sd(residuals_poly_export_2)
hist(std_residuals_poly_export_2, main="Histogram of Standardized Residuals (Order 2) - Exports")

# Polynomial Order 3 - Exports
std_residuals_poly_export_3 <- (residuals_poly_export_3 - mean(residuals_poly_export_3)) / sd(residuals_poly_export_3)
hist(std_residuals_poly_export_3, main="Histogram of Standardized Residuals (Order 3) - Exports")

# Polynomial Order 4 - Exports
std_residuals_poly_export_4 <- (residuals_poly_export_4 - mean(residuals_poly_export_4)) / sd(residuals_poly_export_4)
hist(std_residuals_poly_export_4, main="Histogram of Standardized Residuals (Order 4) - Exports")

# Polynomial Order 5 - Exports
std_residuals_poly_export_5 <- (residuals_poly_export_5 - mean(residuals_poly_export_5)) / sd(residuals_poly_export_5)
hist(std_residuals_poly_export_5, main="Histogram of Standardized Residuals (Order 5) - Exports")

# Standardize Residuals and Create Histogram for Imports
# Polynomial Order 2 - Imports
std_residuals_poly_import_2 <- (residuals_poly_import_2 - mean(residuals_poly_import_2)) / sd(residuals_poly_import_2)
hist(std_residuals_poly_import_2, main="Histogram of Standardized Residuals (Order 2) - Imports")

# Polynomial Order 3 - Imports
std_residuals_poly_import_3 <- (residuals_poly_import_3 - mean(residuals_poly_import_3)) / sd(residuals_poly_import_3)
hist(std_residuals_poly_import_3, main="Histogram of Standardized Residuals (Order 3) - Imports")

# Polynomial Order 4 - Imports
std_residuals_poly_import_4 <- (residuals_poly_import_4 - mean(residuals_poly_import_4)) / sd(residuals_poly_import_4)
hist(std_residuals_poly_import_4, main="Histogram of Standardized Residuals (Order 4) - Imports")

# Polynomial Order 5 - Imports
std_residuals_poly_import_5 <- (residuals_poly_import_5 - mean(residuals_poly_import_5)) / sd(residuals_poly_import_5)
hist(std_residuals_poly_import_5, main="Histogram of Standardized Residuals (Order 5) - Imports")

