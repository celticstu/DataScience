# Read in Value_of_Merchandise_Trade.csv into q4 data frmae
q4 <- read.csv("Value_of_Merchandise_Trade.csv", header = FALSE)

# Add new cloumn names to the q4 data frame 
colnames(q4) <- c("Country", "Import_Export", "Jan10", "Feb10", "Mar10", "Apr10", "May10", "Jun10", "Jul10", "Aug10", "Sep10", "Oct10", "Nov10", "Dec10",
    "Jan11", "Feb11", "Mar11", "Apr11", "May11", "Jun11", "Jul11", "Aug11", "Sep11", "Oct11", "Nov11", "Dec11",
    "Jan12", "Feb12", "Mar12", "Apr12", "May12", "Jun12", "Jul12", "Aug12", "Sep12", "Oct12", "Nov12", "Dec12",
    "Jan13", "Feb13", "Mar13", "Apr13", "May13", "Jun13", "Jul13", "Aug13", "Sep13", "Oct13", "Nov13", "Dec13",
    "Jan14", "Feb14", "Mar14", "Apr14", "May14", "Jun14", "Jul14", "Aug14", "Sep14", "Oct14", "Nov14", "Dec14",
    "Jan15", "Feb15", "Mar15", "Apr15", "May15", "Jun15", "Jul15", "Aug15", "Sep15", "Oct15", "Nov15", "Dec15",
    "Jan16", "Feb16", "Mar16", "Apr16", "May16", "Jun16", "Jul16", "Aug16", "Sep16", "Oct16", "Nov16", "Dec16",
    "Jan17", "Feb17", "Mar17", "Apr17", "May17", "Jun17", "Jul17", "Aug17", "Sep17", "Oct17", "Nov17", "Dec17",
    "Jan18", "Feb18")

# Check structure of q4
str(q4)

head(q4)
library(forecast)
library(tseries)

# Create seperate data frames (imports and exports from the UK)
imports <- subset(q4, Country == "Great Britain" & Import_Export == "Value of Imports (Euro Thousand)")
exports <- subset(q4, Country == "Great Britain" & Import_Export == "Value of Exports (Euro Thousand)")
head(imports)
head(exports)

# remove first two columns
imports <- imports[, - c(1,2)]
exports <- exports[, - c(1, 2)]
#t_s <- ts(imports, frequency = 12)
dim(imports)
dim(exports)

# rotate the data frames so the dates are all in one column
imports <- as.data.frame(t(imports))
imports <- as.data.frame(imports)
head(imports)
exports <- as.data.frame(t(exports))
exports <- as.data.frame(exports)
head(exports)

library(data.table)
# by adding row names, it allowed the Date values become a column
setDT(imports, keep.rownames = TRUE)[]
setDT(exports, keep.rownames = TRUE)[]

# Add column name to the imports list
names(imports) <- c("Date", "Value")
head(imports)
names(exports) <- c("Date", "Value")
head(exports)
#imports$Date <- as.Date(imports$Date, "%b/%Y")
# Format the data values to year-month-day 
imports$Date <- seq(as.Date("2010/1/1"), by = "month", length.out = 98)
head(imports)
str(imports)
imports <- as.data.frame(imports)


# Exports
exports$Date <- seq(as.Date("2010/1/1"), by = "month", length.out = 98)
head(exports)
str(exports)
exports <- as.data.frame(exports)

# Density Plot
# imports
plot(density(imports$Value), main = "Density Plot: Value of Imports from UK", ylab = "Frequency", sub = paste("Skewness:"))
# exports
plot(density(exports$Value), main = "Density Plot: Value of Exports to the UK", ylab = "Frequency", sub = paste("Skewness:"))

# Sum the values for each year and reformat the output to just year
#imports_total <- as.data.frame(rowsum(imports$Value, format(imports$Date, '%Y')))
#head(imports_total)

#exports_total <- as.data.frame(rowsum(exports$Value, format(imports$Date, '%Y')))
#head(exports_total)

# by adding row names, it allowed the Date values become a column
#setDT(imports_total, keep.rownames = TRUE)[]
#setDT(exports_total, keep.rownames = TRUE)[]
# Add column name to the imports list
#names(imports_total) <- c("Date", "Value")
#names(exports_total) <- c("Date", "Value")
#head(imports_total)
#head(exports_total)
#imports$Value <- as.double(imports$Value)
typeof(imports$Value)

opar <- par()
par(mfrow = c(1, 2))

# Using the Augmented Dickey-Fuller Test check for stationarity 
adf.test(imports$Value)
adf.test(exports$Value)

kpss.test(imports$Value)
kpss.test(exports$Value)

Acf(imports)
Pacf(imports)

Acf(exports)
Pacf(exports)


# Check for trends
plot(ndiffs(imports))
# None of these would work as I was getting the error message -
#   time series has no or less than 2 periods

#decompose_result <- decompose(imports, type = "mult")
#decompose_result <- decompose(total_year, type = "mult")
#decompose_result <- decompose(total_year, type = "additive")
#seasonal_trend_error <- stl(total_year, s.window = "periodic")

# As a replacement for the above funtions, I found the HoltWinters
# function and set the beta and gama arguments to false that 
# allowed me to carry out the forcasts 
imports_Forecasts = HoltWinters(imports$Value, beta = FALSE, gamma = FALSE)
plot(imports_Forecasts)
exports_Forecasts = HoltWinters(exports, beta = FALSE, gamma = FALSE)
plot(exports_Forecasts)




plot(ts(imports$Value, start = c(2010), end = c(2017), frequency = 1))
plot(ts(exports_total$Value, start = c(2010), end = c(2017), frequency = 1))

acf(ts(imports$Value, start = c(2010), end = c(2017), frequency = 1))
acf(ts(exports$Value, start = c(2010), end = c(2017), frequency = 1))
# Applying seasonal differance.  This was done although the the data was statioary but 
# the fitted model wouldn't run due to this erros message
# auto.arima can only handle univariate time series

#Imports
imp_seasdiff <- diff(imports$Value, lag = frequency(imports$Date), differences = 1)
plot(imp_seasdiff, type = "l", main = "Yearly Differenced")

#Exports
exp_seasdiff <- diff(exports$Value, lag = frequency(exports$Date), differences = 1)
plot(exp_seasdiff, type = "l", main = "Yearly Differenced")

ndiffs(imports$Value)

trained_model <- lm(imports)
plot(resid(trained_model), type = "l")


# Making the stationary again
stationaryImports <- diff(imp_seasdiff, differences = 1)
plot(stationaryImports, type = "l", main = "Differenced and Stationary")

stationaryExports <- diff(exp_seasdiff, differences = 1)
plot(stationaryExports, type = "l", main = "Differenced and Stationary")


Acf(stationaryExports)
#Pacf(stationaryExports)

fit <- auto.arima(stationaryImports)
fit
accuracy(fit)
Box.test(fit$residuals, type = "Ljung-Box")
forecast(fit, 3)
plot(forecast(fit, 3))

# Find the best fit
fit <- auto.arima(stationaryExports)
fit
accuracy(fit)
Box.test(fit$residuals, type = "Ljung-Box")
forecast(fit, 3)
plot(forecast(fit, 3))
qqnorm(fit$residuals)