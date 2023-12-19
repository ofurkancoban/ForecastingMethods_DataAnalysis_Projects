# Omer Furkan Coban
# Forecasting Methods - Problem Set 2
# Stock Prices
# WiSe 23/24

# Library Imports ----
library(TTR) # For moving averages
library(ggplot2)

# 1. Create a R script file and import the stock prices data. ----
stock_data <- read.csv("Datasets/BASF.csv")
View(stock_data)
# stock_data <- read.csv("Datasets/DAX.csv")
# stock_data <- read.csv("Datasets/VW.csv")

# 2. Create 15 trailing moving averages for share basf. ----
# Using 'adjclose' for calculating moving averages
trailing_ma <- SMA(stock_data$Adj.Close, n = 15)

# 3. Create 15 centered moving averages for share basf. ----
centered_ma <- filter(stock_data$Adj.Close, rep(1/15, 15), sides = 2)

# 4. Create 15 forward moving averages for share basf. ----
forward_ma <- c(rep(NA, 14), trailing_ma)[1:length(trailing_ma)]

# 5. Graph all three moving averages against the original series and explain. ----
combined_stock_data <- data.frame(Date = stock_data$Date, AdjClose = stock_data$Adj.Close, Trailing_MA = trailing_ma, Centered_MA = centered_ma, Forward_MA = forward_ma)

# Display the first few rows of the combined data
head(combined_stock_data,100)

# Plotting the adjusted close prices and the moving averages
ggplot(combined_stock_data, aes(x = Date)) +
    geom_line(aes(y = AdjClose, colour = "Adjusted Close Price", group = 1)) +
    geom_line(aes(y = Trailing_MA, colour = "Trailing MA", group = 1)) +
    geom_line(aes(y = Centered_MA, colour = "Centered MA", group = 1)) +
    geom_line(aes(y = Forward_MA, colour = "Forward MA", group = 1)) +
    labs(title = "Adjusted Close Prices and Moving Averages", x = "Date", y = "Price") +
    theme_minimal() +
    scale_colour_manual("",
                        breaks = c("Adjusted Close Price", "Trailing MA", "Centered MA", "Forward MA"),
                        values = c("black", "blue", "red", "green"))

# Adjusted Close Price (Black Line): This line represents the actual price movements of the BASF stock,
# accounting for dividends and stock splits. It provides the raw fluctuations in price over time, showing the real-time
# market valuation of the stock.

# Trailing Moving Average (Red Line): This line smooths out the price data to show a lagging trend based on the
# past 15 periods (days, weeks, etc., depending on the data granularity). The trailing MA lags the actual price line
# because it's based on past data. It helps in identifying past trends and smoothing out short-term volatility.

# Centered Moving Average (Blue Line): The centered MA provides a balanced view of the stock's trend by averaging
# prices before and after a specific time point. It's plotted centrally along the time series and gives a clearer
# picture of the stock's overall trend without the lagging effect seen in the trailing MA.

# Forward Moving Average (Green Line): This line, which is less common in real-time analysis since it requires
# future data, projects a smoothing effect into the future of the stock price. It anticipates the trend by averaging
# future values, giving a hypothetical view of where the trend may go if current patterns continue.

# Interpretation:

# Volatility and Trend: The distance between the moving averages and the actual price line indicates the level of
# volatility; a greater distance suggests more volatility. When the lines converge, it implies a consolidating price
# trend.

# Crossover Points: Moments where the actual price line crosses over a moving average can be significant. For instance
# if the price crosses above a moving average, it could indicate a potential upward trend and vice versa.

# Trend Direction and Strength: When all moving averages and the price line move in the same direction, it confirms
# the strength and direction of the trend. For example, if all lines are trending upward, the overall market sentiment
# is bullish.

# Comparative Analysis: The relationship between the different types of moving averages can also provide insight.
# If the trailing MA is below the centered MA, which is below the forward MA, it suggests a strong upward trend.
# Conversely, the opposite configuration can signal a downward trend.

# 6.Create and graph a 10 forward moving averages for the random walk data. ----

# Generate random walk data
set.seed(123) # Setting seed for reproducibility
random_walk <- cumsum(rnorm(1000)) # Generating 1000 random steps

# Convert random walk data to a data frame
random_walk_data <- data.frame(Step = 1:1000, Value = random_walk)

# Calculate 10-period forward moving average
n <- 10 # Window size for moving average
forward_ma_rw <- filter(random_walk_data$Value, rep(1/n, n), sides = 1)

# Combine random walk data with the moving average
combined_rw_data <- data.frame(Step = random_walk_data$Step, RandomWalk = random_walk_data$Value, Forward_MA = forward_ma_rw)

# Plotting the random walk data and the moving average
ggplot(combined_rw_data, aes(x = Step)) +
    geom_line(aes(y = RandomWalk, colour = "Random Walk")) +
    geom_line(aes(y = Forward_MA, colour = "Forward Moving Average")) +
    labs(title = "Random Walk Data and 10-period Forward Moving Average", x = "Step", y = "Value") +
    theme_minimal() +
    scale_colour_manual("",
                        breaks = c("Random Walk", "Forward Moving Average"),
                        values = c("blue", "red"))
