#Install Packages
install.packages("rugarch")
install.packages("FinTS")

# Load required libraries
library(tidyquant)
library(dplyr)
library(lubridate)
library(tseries)
library(forecast)
library(rugarch)

# Download data from Yahoo Finance
data <- tq_get('ARVINDFASN.NS', from = "2019-01-01", to = "2024-07-25")

# Ensure adjusted column is an xts object and calculate returns
data_xts <- xts(data$adjusted, order.by = data$date)
returns <- dailyReturn(data_xts, type = "log")
data <- data %>% mutate(Returns = as.numeric(returns)) %>% na.omit()

library(FinTS)

# Check for ARCH/GARCH effects
arch_test <- ArchTest(data$Returns, lags = 1)
print(arch_test$p.value)

# Fit a GARCH(1,1) model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0))
)
model <- ugarchfit(spec = spec, data = data$Returns)

# Forecast three-month (90 days) volatility
forecasts <- ugarchforecast(model, n.ahead = 90)
volatility_forecasts <- sigma(forecasts)

# Create a data frame for plotting
data_plot <- data.frame(Date = data$date, Returns = data$Returns)
forecast_dates <- seq.Date(from = as.Date(tail(data$date, 1)), by = "days", length.out = 90)
forecast_data <- data.frame(Date = forecast_dates, Volatility = as.numeric(volatility_forecasts))

# Plot returns and forecasted volatility
plot(data_plot$Date, data_plot$Returns, type = "l", main = "Arvind Fashions Stock Returns and Forecasted Volatility", xlab = "Date", ylab = "Returns/Volatility", col = "black", ylim = c(-0.1, 0.1))
lines(forecast_data$Date, forecast_data$Volatility, col = "red")
legend("topright", legend = c("Returns", "Forecasted Volatility"), col = c("black", "red"), lty = 1, cex = 0.8)

#(b) VAR, VECM
setwd("C:\\Users\\Ferah Shan\\Downloads")
getwd()

# Load necessary libraries
library(readxl)
library(dplyr)
library(janitor)
library(urca)
library(vars)

df = read_excel('pinksheet.xlsx', sheet="Monthly Prices", skip = 6)

# Rename the first column to "Date"
colnames(df)[1] <- 'Date'

# Convert the Date column to Date format
df$Date <- as.Date(paste0(df$Date, "01"), format = "%YM%m%d")
str(df)

# Get the column numbers for each column
column_numbers <- setNames(seq_along(df), colnames(df))

commodity = df[,c(1,3,25,70,72,61,31)]

commodity = clean_names(commodity)

str(commodity)

# Use dplyr::select to avoid any conflicts and exclude the Date column
commodity_data <- dplyr::select(commodity, -date)

vecm.model <- ca.jo(commodity_data, ecdet = 'const', type  = 'eigen', K = 2, spec = 'transitory', dumvar = NULL)

summary(vecm.model)

# Estimating the VECM
vecm <- cajorls(vecm.model, r = 1)  # r is the number of cointegration vectors
summary(vecm)

# Extracting the coefficients from the VECM model
vecm_coefs <- cajorls(vecm.model, r = 1)$rlm$coefficients

# Creating a VECM model for prediction
vecm_pred <- vec2var(vecm.model, r = 1)


# Forecasting using the VECM

# Forecasting 10 steps ahead
forecast <- predict(vecm_pred, n.ahead = 12)

# Plotting the forecast
par(mar = c(4, 4, 2, 2))  # Adjust margins: c(bottom, left, top, right)
plot(forecast)
