
#####################################################################################################################################################################################



#                                               Oregon Campsite Restaurant: Neural Nets and Interactive Graphs


#####################################################################################################################################################################################





## Import Data Camping_Revenue as revenue

# check the quotes while importing to get 2 columns
class(revenue$V2)

# chopping off the useless quotes at 2 positions
library(tidyr)
revenue <- separate(revenue, col = V2, 
                    sep = c(2, -3), 
                    into = c("rest", "data", "rest2"))

# all the relevant data is in column "data"
head(revenue)

# class is still a character (with some missing data)
class(revenue$data)

# conversion to time series
MyTimeSeries <- ts(as.numeric(revenue$data),
           start = 1997, frequency = 12)

# data is still not clean (outliers and NAs)
summary(MyTimeSeries)
# 4 NA's
# Min of 3 and Max of 3334333 which isn't logical


# all in one cleaning tool
library(forecast)
MyTimeSeries <- tsclean(MyTimeSeries)

# check the data
summary(MyTimeSeries)

plot(MyTimeSeries)

# set up a Neural Network model
mynnetar <- nnetar(MyTimeSeries)

# forecasting 3 years with the model
nnetforecast <- forecast(mynnetar, h = 36,
                         PI = T)
library(ggplot2)
autoplot(nnetforecast)

## interactive dygraph

# data we need for the graph
data <- nnetforecast$x
lower <- nnetforecast$lower[,2]
upper <- nnetforecast$upper[,2]
pforecast <- nnetforecast$mean

mydata <- cbind(data, lower, upper,
                pforecast)

library(dygraphs)

dygraph(mydata, main = "Oregon Campsite Restaurant") %>% 
  dyRangeSelector() %>% 
  dySeries(name = "data", label = "Revenue Data") %>%
  dySeries(c("lower","pforecast","upper"), label = "Revenue Forecast") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Monthly Revenue USD") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey") %>%
  dyAnnotation("2010-8-1", text = "CF", tooltip = "Camp Festival", attachAtBottom = T)