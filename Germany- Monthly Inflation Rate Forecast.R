
#####################################################################################################################################################################################



#                                               Germany: Monthly Inflation Rate Forecast


#####################################################################################################################################################################################

#German Inflation Rate 
https://www.statbureau.org/en/germany/inflation-tables

#Data
"-0.31	0.41	0.51	-0.20	0.61	0.20	0.61	-0.30	-0.10	-0.20	-0.51	0.41
-0.51	0.61	-0.20	0.10	-0.10	0.30	0.00	0.20	-0.30	0.00	-0.10	0.81
-0.60	0.40	0.50	0.10	-0.10	0.00	0.20	0.10	-0.10	0.10	0.10	0.60
-0.20	0.60	0.59	0.00	0.00	0.10	0.20	0.10	0.20	0.00	0.20	0.19
-0.10	0.68	0.58	-0.19	0.00	-0.19	0.39	0.38	0.10	0.00	0.10	0.29
-0.48	0.57	0.48	-0.47	0.38	0.09	0.47	0.00	0.00	-0.19	0.19	0.38
-0.56	0.47	0.28	-0.19	-0.09	0.28	0.28	0.00	0.00	-0.28	0.00	0.00
-1.03	0.85	0.47	0.00	0.09	-0.09	0.19	0.00	-0.19	0.00	0.09	-0.09
-0.84	0.38	0.75	-0.37	0.28	0.09	0.28	0.00	0.09	0.19	0.09	0.74
-0.64	0.65	0.18	0.00	-0.18	0.18	0.37	0.09	0.09	0.00	"


mydata = scan()

plot.ts(mydata)

GermanInflation = ts(mydata, start = 2008, frequency = 12)

plot(GermanInflation)

## About Dataset ##
# No Trend
# Seasonal
# Contains Negative Values so you cannot use Multiplicative ES Models
# Stable Amplitude

# Seasonal Decomposition
decompose(GermanInflation)

plot(decompose(GermanInflation))

# Using the stl method
plot(stl(GermanInflation, s.window = 7))

# stl forecasting
plot(stlf(GermanInflation, method = "ets")) #We get an (A,N,N) which means (Additative Seasonality, No Trend, No Remainder/Error)

# comparison with a standard ets forecast
plot(forecast(ets(GermanInflation), h = 24)) #We get an (A,N,A) which means (Additative Seasonaility, No Trend, Additative Error)


# using autoplot
library(ggplot2)
autoplot(stlf(GermanInflation, method = "ets"))

## Seasonal Arima (package forecast)
auto.arima(GermanInflation, stepwise = T, 
           approximation = F, trace = T)
## Arima has several Parts ##
# Non-Seasonal (p,d,q)
# Seasonal (P,D,Q)
# Frequency [m] This is the number of periods per cycle in this case as a 12 month season
# The Best Model for this case is ARIMA(1,0,2)(0,1,1)[12] it has the lowest IC


# Getting an object
GermanInflationarima = auto.arima(GermanInflation, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

# Forecast
forec = forecast(GermanInflationarima)
plot(forec)

## Exponential Smoothing with ets
# Auto gemerated 
ets(GermanInflation)
# Forecast plot
GermanInflationets = ets(GermanInflation)

plot(forecast(GermanInflationets, h = 60))

# Comparison with seasonal Holt Winters model
plot(hw(GermanInflation, h = 60))

## Cross Validation of 2 models
GermanInflationets = ets(GermanInflation)
GermanInflationarima = auto.arima(GermanInflation, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

forecastets = function(x, h) {
  forecast(ets(x), h = h)
}

forecastarima = function(x, h) {
  forecast(auto.arima(x), stepwise = T, approximation = F, h=h)
}

etserror = tsCV(GermanInflation, forecastets, h=1)
arimaerror = tsCV(GermanInflation, forecastarima, h=1)

mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)
# Cannot use IC to evaluate the 2 models because the IC is computed differently so we will use Mean Squared Error (MSE)
# ETS has a MSE=0.091 and ARIMA has a MSE=0.078 
# The lowest MSE is the better fit model which is ARIMA in this case
