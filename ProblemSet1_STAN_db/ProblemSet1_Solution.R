# Omer Furkan Coban
# Forecasting Methods - Problem Set 1
# STAN Database
# WiSe 23/24

# Library Imports ----

# Uncomment the following lines if you need to install the packages
# install.packages("dplyr")
# install.packages("stringr")
library(dplyr)

# 1. Create a R script file and import the STAN data base.----
# Reading the STAN database into a variable 'stan' using 'read.csv' function.
stan <- read.csv("Datasets/STAN_ALL.csv")

# 2. Drop  all  variables  with  exception  of  COUNTRY  YEAR  VALU  WAGE  INDUSTRY ISICREV3.----
# Using 'select()' to keep only the columns of interest (COUNTRY, YEAR, VALU, WAGE, INDUSTRY, ISICREV3).
stan <- stan %>%
  select(all_of(c("COUNTRY","YEAR","VALU","WAGE","INDUSTRY","ISICREV3")))

# 3. Create a new variable which is the growth rate of value added in current prices.----
# Using 'mutate()' to create a new variable 'GrowthRateValueAdded'.
# Calculating growth rate using 'lag()' to get the previous year's 'VALU' value.
stan <- stan %>%
  arrange(COUNTRY, ISICREV3, YEAR) %>%
  group_by(COUNTRY, ISICREV3) %>%
  mutate(GrowthRateValueAdded = (VALU - dplyr::lag(VALU, n = 1, default = NA)) /
         dplyr::lag(VALU, n = 1, default = NA)) %>%
  ungroup()

# 4. Create a new variable which is the growth rate of the wages.----
# Again, using 'mutate()' to add a new variable 'GrowthRateWages' to calculate the growth rate.
stan <- stan %>%
  mutate(GrowthRateWages = ((WAGE - lag(WAGE)) / lag(WAGE)) * 100)

# Drop all observations where value added and wages have both a missing value.
# Using 'filter()' to remove rows where 'VALU' and 'WAGE' columns have NA values.
stan <- stan %>%
  filter(!(is.na(VALU) & is.na(WAGE)))

# 6. Create a scatter plot between the growth rates of value added and the wages for the Motor Vehicles, Trailers and Semi-Trailers sector in France.----
FranceMotorVehicles <- stan %>%
  filter(COUNTRY == "FRA", ISICREV3 == "34")

plot(FranceMotorVehicles$GrowthRateValueAdded, FranceMotorVehicles$GrowthRateWages,
     main = "Scatter Plot: Growth Rates of Value Added vs Wages",
     xlab = "Growth Rate of Value Added (%)",
     ylab = "Growth Rate of Wages (%)")

# 7. Create a line plot of the growth rate of wages over time for the Motor Vehicles, Trailers and Semi-Trailers sector in France.----
plot(FranceMotorVehicles$YEAR, FranceMotorVehicles$GrowthRateWages,
     type = 'l',
     main = "Line Plot: Growth Rate of Wages Over Time",
     xlab = "Year",
     ylab = "Growth Rate of Wages (%)")

# 8. Get the means of the growth rates of value added and wages for all observations and for the Motor Vehicles, Trailers and Semi-Trailers sector in France.----
MeanGrowthRates <- stan %>%
  summarise(MeanGrowthRateValueAdded = mean(GrowthRateValueAdded, na.rm = TRUE),
            MeanGrowthRateWages = mean(GrowthRateWages, na.rm = TRUE))

FranceMotorVehiclesMeanGrowthRates <- FranceMotorVehicles %>%
  summarise(MeanGrowthRateValueAdded= mean(GrowthRateValueAdded, na.rm = TRUE),
            MeanGrowthRateWages = mean(GrowthRateWages, na.rm = TRUE))

# Printing the means----
print(MeanGrowthRates)
print(FranceMotorVehiclesMeanGrowthRates)
