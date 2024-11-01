################################################################################

# install necessary packages

# install.packages("fpp")
# install.packages("fma")

# load packages
library(fpp)
library(fma)
library(readr)

################################################################################


################################################################################
# Load data
################################################################################

### DATA SOURCE
# INE
# https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_indicadores&indOcorrCod=0000863&contexto=bd&selTab=tab2
# change indicators to: Embarked passengers (No.) in airports by Geographic localization, Type of traffic and Nature of traffic; Monthly

lisbon_airport_disembark <- read_delim("ine_lisbon_disembarked_passengers.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  col_types = cols(value = col_number())
)

lisbon_airport_disembark

tail(lisbon_airport_disembark)

plot(lisbon_airport_disembark$value)


lis_airport_disembark_ts <- ts(
  lisbon_airport_disembark$value, 
  start = 2004,       # either provide the start or the end
  frequency = 12      # if we don't provide start and end, we need a frequency
)    

# plot the time series

plot.ts(lis_airport_disembark_ts)

# plot tsdisplay

tsdisplay (   # Plots a time series along with its acf and either its pacf, lagged scatterplot or spectrum
  lis_airport_disembark_ts, 
  plot.type="scatter"    # we can choose pacf, lagged scatterplot or spectrum
)


################################################################################
# Filter until 2020
################################################################################

lis_disembark_filt <-  window(               # cuts the time series to a portion. use this to exclude long cycles
  lis_airport_disembark_ts,
  start = 2004,
  end = 2020    
)

plot.ts(lis_disembark_filt)

# plot tsdisplay

tsdisplay (   # Plots a time series along with its acf and either its pacf, lagged scatterplot or spectrum
  lis_disembark_filt, 
  plot.type="scatter"    # we can choose pacf, lagged scatterplot or spectrum
)


################################################################################
# Decompose the time series into it's components
################################################################################


# decompose the time series into it's components
lis_airport_disembark_decomp <- decompose(
  lis_disembark_filt,
  type = "multiplicative"
)


plot(lis_airport_disembark_decomp)


monthplot(lis_disembark_filt)

par(mfrow = c(1,2))
acf(lis_disembark_filt)
pacf(lis_disembark_filt)
par(mfrow = c(1,1))


################################################################################
# Simple Forecasting Models
################################################################################


# create a new window around the second time series part
plot.ts(
  lis_disembark_filt,
  xlim = c(2004, 2025)          # extend the x-axis to plot the forecast
)

lines(meanf(lis_disembark_filt, h=24)$mean, col = 2, lwd = 3)                # mean
lines(naive(lis_disembark_filt, h=24)$mean, col = 3, lwd = 3)                # naive
lines(snaive(lis_disembark_filt, h=24)$mean, col = 4, lwd = 3)               # seasonal naive
lines(rwf(lis_disembark_filt, drift = T, h=24)$mean, col = 5, lwd = 3)       # drift


################################################################################
# Handling Non-constant Variance
################################################################################

plot.ts(lis_disembark_filt)

lambda <- BoxCox.lambda(lis_disembark_filt)
lambda
# not close to 0 so we will have to apply the transformation to remove variance

lis_disembark_filt_transf <- BoxCox(lis_disembark_filt, lambda = lambda)
plot.ts(lis_disembark_filt_transf)

monthplot(lis_disembark_filt_transf)

par(mfrow = c(1,2))
acf(lis_disembark_filt_transf)
pacf(lis_disembark_filt_transf)
par(mfrow = c(1,1))

################################################################################
# Handling Trend
################################################################################

lis_disembark_filt_transf_diff1 <- diff(lis_disembark_filt_transf, 1)
plot.ts(lis_disembark_filt_transf_diff1)

# inspect non-seasonal components
par(mfrow = c(1,2))
acf(lis_disembark_filt_transf_diff1)
pacf(lis_disembark_filt_transf_diff1)
par(mfrow = c(1,1))



################################################################################
# Handling Seasonality
################################################################################

pt_co2_ts_filt_transf_diff1_diff12 <- diff(lis_disembark_filt_transf_diff1, 12)
plot.ts(pt_co2_ts_filt_transf_diff1_diff12)

# inspect non-seasonal components
par(mfrow = c(1,2))
acf(pt_co2_ts_filt_transf_diff1_diff12)
pacf(pt_co2_ts_filt_transf_diff1_diff12)
par(mfrow = c(1,1))

# ACF with single spike in lag 1
# PACF exponentially decays
# AM(1) seems to be a potential model



# increase window to inspect seasonal components
par(mfrow = c(1,2))
acf(pt_co2_ts_filt_transf_diff1_diff12, 48)
pacf(pt_co2_ts_filt_transf_diff1_diff12, 48)
par(mfrow = c(1,1))

# ACF with spike in 1, 2
# PACF exponentially decays to 0
# ARIMA(1,1,1) seems to be a potential model



################################################################################
# ARIMA Model
################################################################################



fit<-arima(lis_disembark_filt_transf,order = c(0,1,1),
             seasonal=list(order=c(0,1,1),
                           period=12))

fit



tsdiag(fit) # check the 3 plot of the residuals
# ljung-box - accumulated autocorrelation should be all above 0.05


par(mfrow = c(1,1))
plot(as.vector(fitted(fit)), as.vector(residuals(fit)))

fit_auto <- auto.arima(pt_co2_ts_filt_transf)
fit_auto

tsdiag(fit_auto) # check the 3 plot of the residuals
# ljung-box - accumulated autocorrelation should be all above 0.05



plot(as.vector(fitted(fit_auto)), as.vector(residuals(fit_auto)))

# Plot the forecast

plot(InvBoxCox(forecast(fit, h=48)$mean, lambda=lambda))

InvBoxCox(forecast(fit, h=48), lambda = lambda)




lines(forecast(fit, h=24), col = 2, lwd = 3)

forecast(fit, h=72)

abline(h=1400, col = 2, lwd = 3)                # mean

Box.test(residuals(fit), lag=8,
         fitdf=2, type="Ljung")


