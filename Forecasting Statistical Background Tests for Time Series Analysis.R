lynx

time(lynx)

length(lynx)

tail(lynx) # last 6 observations

mean(lynx); median(lynx)

plot(lynx)

sort(lynx)

sort(lynx)[c(57,58)]

quantile(lynx)

quantile(lynx, prob = seq(0, 1, length = 11), type = 5)

### SIMPLE FORECAST METHODS  ####################################################################

set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(forecast)
meanm <- meanf(myts, h=20)
naivem <- naive(myts, h=20)
driftm <- rwf(myts, h=20, drift = T)

plot(meanm, plot.conf = F, main = "")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("topleft",lty=1,col=c(4,123,22),
       legend=c("Mean method","Naive method","Drift Method"))

#### MODEL COMPARISON AND ACCURACY TESTS  ####################################################################

set.seed(95)
myts <- ts(rnorm(200), start = (1818))
mytstrain <- window(myts, start = 1818, end = 1988)
plot(mytstrain)

library(forecast)
meanm <- meanf(mytstrain, h=30)
naivem <- naive(mytstrain, h=30)
driftm <- rwf(mytstrain, h=30, drift = T)

mytstest <- window(myts, start = 1988)

accuracy(meanm, mytstest)
accuracy(naivem, mytstest)
accuracy(driftm, mytstest)

###### RESIDUALS  ####################################################################
# They show the quality of the model and how much of the randomness cannot be explained by the model. Residuals should be ideally normally distributed 
set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(forecast)
meanm <- meanf(myts, h=20)
naivem <- naive(myts, h=20)
driftm <- rwf(myts, h=20, drift = T)

var(meanm$residuals)
mean(meanm$residuals)

mean(naivem$residuals)

naivwithoutNA <- naivem$residuals
naivwithoutNA <- naivwithoutNA[2:200]
var(naivwithoutNA)
mean(naivwithoutNA)

driftwithoutNA <- driftm$residuals
driftwithoutNA <- driftwithoutNA[2:200]
var(driftwithoutNA)
mean(driftwithoutNA)

hist(driftm$residuals)

acf(driftwithoutNA)




### STATIONARITY ####################################################################
# If a time series is stationary it means there are consistent statistical properties i.e. (variance, mean, autocorrelation)

x <- rnorm(1000) # no unit-root, stationary

library(tseries)

adf.test(x) # augmented Dickey Fuller Test and Stationary because P-Value <0.05


plot(nottem) # Let s see the nottem dataset

plot(decompose(nottem))

adf.test(nottem) # Stationary because P-Value <0.05


y <- diffinv(x) # non-stationary

plot(y)

adf.test(y) # Non-Stationary because P-Value >0.05



### AUTOCORRELATION TESTS  ####################################################################
# This means there is correlation on a time scale. The events in one period impact the next. For the Lynx data set, capturing too many Lynx in one period means there are less to catch in the following period

# Durbin Watson test for autocorrelation
# Shows autocorrelation for only the first order. Has weaknesses with trends and seasonality. 

length(lynx); head(lynx); head(lynx[-1]); head(lynx[-114]) # check the required traits for the test while subtracting the first and last observations

library(lmtest)

dwtest(lynx[-114] ~ lynx[-1]) # Here we get the last observation and subtract the first observation to get the 1 lag time difference
#There is autocorrlation present


x = rnorm(700) # Lets take a look at random numbers

dwtest(x[-700] ~ x[-1]) #Minus the last and first observations
# There is no autocorrelation within this dataset

length(nottem) # and the nottem dataset

dwtest(nottem[-240] ~ nottem[-1])


### Autocorrelation Test(ACF) and Partial Autocorrelation Function(PACF)

acf(lynx, lag.max = 20); pacf(lynx, lag.max =20, plot = F)

# lag.max for numbers of lags to be calculated

# plot = F to suppress plotting

acf(rnorm(500), lag.max = 20)
#The first lag is not important but for one partially through it is above the line which is find since there are 20 points and 1 of 20 fits our 95% confidence requirement

## Exercise messy data

set.seed(54)
myts <- ts(c(rnorm(50, 34, 10), 
             rnorm(67, 7, 1), 
             runif(23, 3, 14)))

# myts <- log(myts)

plot(myts)
# We can see here that the first issuea are that mean and variance are not constant 

library(forecast)
meanm <- meanf(myts, h=10)
naivem <- naive(myts, h=10)
driftm <- rwf(myts, h=10, drift = T)

plot(meanm, main = "", bty = "l")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("bottomleft",lty=1,col=c(4,123,22), bty = "n", cex = 0.75,
       legend=c("Mean method","Naive method","Drift Method"))

length(myts)
mytstrain <- window(myts, start = 1, end = 112 )
mytstest <- window(myts, start = 113)

meanma <- meanf(mytstrain, h=28)
naivema <- naive(mytstrain, h=28)
driftma <- rwf(mytstrain, h=28, drift = T)

accuracy(meanma, mytstest)
accuracy(naivema, mytstest)
accuracy(driftma, mytstest)

plot(naivem$residuals) # It is not homoskedastic mean isn't near 0, clear variation in variance

mean(naivem$residuals[2:140])

hist(naivem$residuals) # normal distribution, but there is to much weight on center region

shapiro.test(naivem$residuals) # test for normal distribution, null of normal distr can be rejected

acf(naivem$residuals[2:140]) # autocorrelation test, autocorrelation present (4 times crossing)


### Converting with Log Transofrmation
mytslog <- log(myts)

plot(mytslog)
# It is now rescaled and the variance is less steep

library(forecast)
meanm <- meanf(mytslog, h=10)
naivem <- naive(mytslog, h=10)
driftm <- rwf(mytslog, h=10, drift = T)

plot(meanm, main = "", bty = "l")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("bottomleft",lty=1,col=c(4,123,22), bty = "n", cex = 0.75,
       legend=c("Mean method","Naive method","Drift Method"))

length(mytslog)
mytstrain <- window(mytslog, start = 1, end = 112 )
mytstest <- window(mytslog, start = 113)

meanma <- meanf(mytstrain, h=28)
naivema <- naive(mytstrain, h=28)
driftma <- rwf(mytstrain, h=28, drift = T)

accuracy(meanma, mytstest)
accuracy(naivema, mytstest)
accuracy(driftma, mytstest)

plot(naivem$residuals) # It is not homoskedastic mean isn't near 0, clear variation in variance

mean(naivem$residuals[2:140])

hist(naivem$residuals) # normal distribution, but there is to much weight on center region

shapiro.test(naivem$residuals) # test for normal distribution, null of normal distr can be rejected

acf(naivem$residuals[2:140]) # autocorrelation test, autocorrelation present (4 times crossing)
