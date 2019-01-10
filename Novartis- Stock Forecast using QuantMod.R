
#####################################################################################################################################################################################



#                                               Novartis: Stock Forecast using QuantMod


#####################################################################################################################################################################################




# Fetching data from yahoo with quantmod
library(quantmod)

Novartis = getSymbols("NVS", auto.assign=F, 
                      from = "2018-01-01", to = "2019-01-01")
# use argument return.class to modify output class to ts or mts

## using a column like a standard ts
plot(as.ts(Novartis$NVS.Open))

# functions to explore unprocessed xts from quantmod
chartSeries(Novartis, type = "line")

# acf and pacf to get an idea about autocorrelation
library(forecast)
ggtsdisplay(Novartis$NVS.Open)
# The ACF plot shows that we need to do work due to Autocorrelation
# The PACF plot shows that there is very little correlation beyond 1 lag


# Arima model
NovartisArima = auto.arima(Novartis$NVS.Open, 
                           stepwise = T, 
                           approximation = F, 
                           trace = T); NovartisArima
#The Best ARIMA model here is (0,1,0)


# Alternative arima with autoregressive part 
NovartisArima2 = Arima(Novartis$NVS.Open, order = c(1,1,1))
NovartisArima2

# Forecast arima
plot(forecast(NovartisArima, h = 20))
plot(forecast(NovartisArima2, h = 20))

# Ets model
NovartisEts = ets(Novartis$NVS.Open)

# Forecast ets
plot(forecast(NovartisEts, h = 20))

## Getting a regular time series

# Conversion to dataframe
Novartis = as.data.frame(Novartis)

### Adding the rownames as date
Novartis$Date = rownames(Novartis)
Novartis$Date = as.Date(Novartis$Date)
head(Novartis)

# Creating the date column, 'by' can be either nr days or integer
# 'from' and 'to' with as.Date to make sure this is a date format
MyDates = seq.Date(from = as.Date("2018-01-01"), 
                   to = as.Date("2019-01-01"), 
                   by = 1)

# Converting to a df (required for the merge)
MyDates = data.frame(Date = MyDates)

# Padding with 'MyDates'
MyData = merge(Novartis, MyDates, by = "Date", all.y = T)


## CLEANING DATES ###################################################################################
# Need to view 2018 Jan on Calendar 

# Removing initial days to start on monday
MyData = MyData[8:366,]

# Removing sundays, watch the from as the first one to remove
MyData = MyData[-(seq(from = 7, to = nrow(MyData), by = 7)),]
# Removing saturdays
MyData = MyData[-(seq(from = 6, to = nrow(MyData), by = 6)),]

# Using last observatoin carried forward imputation
MyData = na.locf(MyData)

## Which days are the ones best to buy or sell?

# Putting the closeprice into a weekly time series
HighestPrice = ts(as.numeric(MyData$NVS.High), 
                  frequency = 5)

# Various plots
seasonplot(HighestPrice, season.labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
monthplot(HighestPrice)
monthplot(HighestPrice, base = median, col.base = "red") 
plot(stl(HighestPrice, s.window = "periodic"))

# Comparison with the low prices
par(mfrow = c(1,2))
LowestPrice = ts(as.numeric(MyData$NVS.Low), 
                 frequency = 5)
monthplot(LowestPrice, base = median, col.base = "red")
monthplot(HighestPrice, base = median, col.base = "red")
par(mfrow = c(1,1))
# Here we can see, that based off of 2018 data, the best days to purchase are Thursday and Friday and the best day overall to sell is Tuesday.

