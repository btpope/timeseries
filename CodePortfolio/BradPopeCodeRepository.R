### Time Series code
# Time Series examples from Introduction to Time Series Analysis by David S. Matteson

## Creating Time Series
# data_vector is comprised of 10 6 11 8 10 3 6 9
time_series = ts(data_vector)

# Convert data_vector to a ts object with start = 2004 and frequency = 4
time_series <- ts(data_vector, start=2004, frequency=4) 

## Plotting and Summarize Time Series 

# Plot AirPassengers
library(astsa)
plot(AirPassengers)

# boxplot of airpassengers for each cycle
boxplot(AirPassengers ~ cycle(AirPassengers))

#Reduce seasonal effect by aggregating data to the annual level. 
plot(aggregate(AirPassengers), ylab = "Aggregated annually")


# View the start and end dates of AirPassengers
start(AirPassengers); end(AirPassengers)

# Use time(), deltat(), frequency(), and cycle() with AirPassengers 
time(AirPassengers); deltat(AirPassengers); frequency(AirPassengers); cycle(AirPassengers)
summary(AirPassengers)

## Imputing Values

# Compute the mean of AirPassengers
mean(AirPassengers, na.rm=TRUE)

# Impute mean values to NA in AirPassengers
AirPassengers[85:96] <- mean(AirPassengers, na.rm = TRUE)



## diff()  function calculates the differences between all consecutive values of a vector

# Generate the first difference of z
dz <- diff(z)
  
# View the length of z and dz, respectively
length(z)
length(dz)

# Generate a diff of x with lag = 4. Save this to dx
dx <- diff(x, lag=4)
  
# Plot dx
ts.plot(dx)
  
# View the length of x and dx, respectively 
length(x)
length(dx)

## white noise with arima.sim

# Simulate a WN model 
white_noise <- arima.sim(model = list(order=c(0,0,0)), n = 100)
ts.plot(white_noise)

# Simulate a WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order=c(0,0,0)), n = 100, mean = 100, sd = 10)
ts.plot(white_noise_2)

## Simulate a Random Walk
# Generate a RW model using arima.sim
random_walk <- arima.sim (model = list(order=c(0,1,0)), n = 100)
ts.plot(random_walk)

# Calculate the first difference series
random_walk_diff <- diff(random_walk) 
ts.plot(random_walk_diff)
  
## Random Walk with drift
rw_drift <- arima.sim(model = list(order = c(0,1,0)), n = 100, mean = 1)
ts.plot(rw_drift)

# Or use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)


# Calculate the first difference series
rw_drift_diff <- diff(rw_drift)
ts.plot(rw_drift_diff)

# Plot White Noise, Random Walk
plot.ts(cbind(white_noise, random_walk))


## Log Diff

# Convert prices to log returns
logreturns <- diff(log(eu_stocks))
plot(logreturns)


## Covariance and Correlation
# Use cov() to form the variance-covariance matrix
cov(DAX_logreturns, FTSE_logreturns)
cov(logreturns)

# Use cor() to compute a correlation matrix
cor(DAX_logreturns, FTSE_logreturns)
cor(logreturns)

## acf
# Use acf() to view the autocorrelations 
acf(x, lag.max = 10, plot = FALSE)

# View the ACF of x
acf(x)


## Autoregressive (AR) model 
# Widely used time series model that shares the very familiar interpretation of a simple linear regression, but here each observation is regressed on the previous observation. The AR model also includes the white noise (WN) and random walk (RW) models examined in earlier chapters as special cases.

# Simulate an AR model with 0.5 slope
x <- arima.sim(model = list(ar=.5), n = 100)

# Simulate an AR model with 0.9 slope
y <- arima.sim(model = list(ar=.9), n=100)

# Simulate an AR model with -0.75 slope
z <- arima.sim(model = list(ar=-0.75), n=100)

# Plot your simulated data
plot.ts(cbind(x, y, z))

# Fit the AR model to a dataset x
AR = arima(x, order = c(1,0,0))
print(AR)
#Results
#Coefficients:
#         ar1  intercept
#      0.8575    -0.0948
# s.e.  0.0491     0.6703
# sigma^2 estimated as 1.022:  log likelihood = -143.66,  aic = 293.32

# Plot the series and fitted values plotted together on the same plot - fitted with points
ts.plot(AirPassengers)
AR_fitted <- AirPassengers - residuals(AR)
points(AR_fitted, type = "l", col = 2, lty = 2)


## Forecasting with ARIMA model

# Fit an AR model to Nile
AR_fit <-arima(Nile, order  = c(1,0,0))
print(AR_fit)

# Use predict() to make a 1-step forecast
predict_AR <- predict(AR_fit)

# Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]

# Use predict to make 1-step through 10-step forecasts
predict(AR_fit, n.ahead = 10)

# Run to plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)


## Examples:

# Add a legend to a ts.plot
plot(eu_stocks, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998",legend("topleft", colnames(eu_stocks), lty = 1, col = 1:4, bty = "n"))

# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef

# Use abline(0, ...) to add time trend to the figure
abline(0,int_wn)

# Scatterplot matrix of eu_stocks
pairs(eu_stocks)

# as.ts with two plotted
inflation = as.ts(Mishkin[,1])
ts.plot(inflation) ; acf(inflation)


### xts
## From Manipulating Time Series Data in R with xts & zoo by Jeffrey Ryan

# load the xts library
library(xts)

## Creating xts

#Example 1:
# Create the object data using 5 random numbers
data = rnorm(5)

# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")
 
# Use xts() to create smith
smith <- xts(x = data, order.by = dates)
 
# Create bday (1899-05-08) using a POSIXct date class object
bday <- as.POSIXct("1899-05-08")
 
# Create hayek and add a new attribute called born
hayek <- xts(x = data, order.by = dates, born = bday)
#Example 1 end

# Example 2 Create a new xts file that automatically populates the dates for the rows
a <- xts(x = 1:2, as.Date("2012-01-01") + 0:1)

#Example 3 
 # Create 5 dates first 
dates <- as.Date("2016-01-01") + 0:4

# Create ts_a by creating 5 rows and referencing the dates.
ts_a <- xts(x = 1:5, order.by = dates)


## Converting to xts

# Create dat by reading tmp_file
 dat = read.csv(tmp_file)
 
# Convert dat into xts
 xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))

# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")

# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)


## Removing xts formatting

# Convert sunspots to xts using as.xts().
sunspots_xts=as.xts(sunspots)
head(sunspots)

# Get the temporary file name
tmp <- tempfile()
head(tmp)

# Write the xts object using zoo to tmp
write.zoo(sunspots_xts, sep = ",", file = tmp)
head(tmp)

# Read the tmp file. FUN = as.yearmon converts strings such as Jan 1749 into a proper time class

sun <- read.zoo(tmp, sep = ",", FUN = as.yearmon)
head(sun)

# Convert sun into xts. Save this as sun_xts
sun_xts = as.xts(sun)
head(sun_xts)



## Decomposing Time Series

#Create a chocolate time series
Choc <- ts(week5cbe[, 1], start = 1958, freq = 12)

Chocdecom2 <- decompose(Choc, "additive")
plot(Chocdecom2)



## Extracting data from xts
# Extract the core data of hayek
hayek_core=coredata(hayek)
 
# View the class of hayek_core
class(hayek_core)

# Extract the index of hayek
hayek_index=index(hayek)

# View the class of hayek_index
class(hayek_index)


#### Zoo Library

library(zoo)
wZ <- zoo(inData)


#The two columns (Symbol and Series) are characters and need to be removed:
Z <- read.zoo(inData[3:10],format = "%d-%b-%y",index.column = "Date")


## Aggregate by quarter and year
# Get the mean of Z$DeliveryVolume to Z$TotalVolume per quarter, by using Aggregate function.
aggregate(Z$DeliveryVolume/Z$TotalVolume,as.yearqtr,mean)

#- Get the mean of Z$DeliveryVolume to Z$TotalVolume per month, by using Aggregate function.
aggregate(Z$DeliveryVolume/Z$TotalVolume,as.yearmon,mean)

##Extract rows
#- Extract only the rows from 2015-Feb-01 to 2015-Feb-15 from Zoo object Z
window(Z, start = as.Date("2015-02-01"),end = as.Date("2015-02-15"))

## Convert to XTS change the format and count
# convert zoo data to xts
library(xts)
z_xts <- as.xts(Z)

# change the format to 01/02/19 from 2019-01-02
indexFormat(z_xts) <- "%m-%d-%Y"

# estimate the periodicity of data
periodicity(z_xts)

# count months
nmonths(z_xts)

# count years
nyears(z_xts)
