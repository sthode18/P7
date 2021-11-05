library(readr)
library(tseries)
library(urca)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(pracma)
library(RCurl)

#Define the data
#Order the data by dates such that they are in ascending order
#Remove NA rows for dates
#Keep only date and close price
#Close -> numeric and date -> date
BNB <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_BNBUSDT_1h.csv")
BNB <- read.csv(text=BNB)
BNB <- as.data.frame(BNB)
BNB <- t(BNB)
BNB <- as.data.frame(BNB)
BNB <- rev(BNB)
BNB <- t(BNB)
BNB <- as.data.frame(BNB)
#BNB <- BNB[, -1]
#BNB <- BNB[, -c(2:5)]
#BNB <- BNB[, -c(3:5)]
BNB$date <- lubridate::ymd_hms(BNB$date, tz = "UCT")
BNB$close <- as.numeric(BNB$close)
BNB <- na.omit(BNB)

BTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_BTCUSDT_1h.csv")
BTC <- read.csv(text=BTC)
BTC <- as.data.frame(BTC)
BTC <- t(BTC)
BTC <- as.data.frame(BTC)
BTC <- rev(BTC)
BTC <- t(BTC)
BTC <- as.data.frame(BTC)
#  BTC <- BTC[, -1]
# BTC <- BTC[, -c(2:5)]
#BTC <- BTC[, -c(3:5)]
BTC$date <- lubridate::ymd_hms(BTC$date, tz = "UCT")
BTC$close <- as.numeric(BTC$close)
BTC <- na.omit(BTC)

ETH <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_ETHUSDT_1h.csv")
ETH <- red.csv(text=ETH)
ETH <- as.data.frame(ETH)
ETH <- t(ETH)
ETH <- as.data.frame(ETH)
ETH <- rev(ETH)
ETH <- t(ETH)
ETH <- as.data.frame(ETH)
#ETH <- ETH[, -1]
#ETH <- ETH[, -c(2:5)]
#ETH <- ETH[, -c(3:5)]
ETH$date <- lubridate::ymd_hms(ETH$date, tz = "UCT")
ETH$close <- as.numeric(ETH$close)
ETH <- na.omit(ETH)

LTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_LTCUSDT_1h.csv")
LTC <- read.csv(text=LTC)
LTC <- as.data.frame(LTC)
LTC <- t(LTC)
LTC <- as.data.frame(LTC)
LTC <- rev(LTC)
LTC <- t(LTC)
LTC <- as.data.frame(LTC)
#LTC <- LTC[, -1]
#LTC <- LTC[, -c(2:5)]
#LTC <- LTC[, -c(3:5)]
LTC$date <- lubridate::ymd_hms(LTC$date, tz = "UCT")
LTC$close <- as.numeric(LTC$close)
LTC <- na.omit(LTC)

#Plot of time series
ggplot(BNB, aes(x=date, y=close)) + geom_line() + ggtitle("Hourly closing price of Binance Coin")
ggplot(BTC, aes(x=date, y=close)) + geom_line() + ggtitle("Hourly closing price of Bitcoin")
ggplot(ETH, aes(x=date, y=close)) + geom_line() + ggtitle("Hourly closing price of Etherium")
ggplot(LTC, aes(x=date, y=close)) + geom_line() + ggtitle("Hourly closing price of Litecoin")

#Checking the order of integration and p
auto.arima(BNB$close, max.q=0)
auto.arima(BTC$close, max.q=0)
auto.arima(ETH$close, max.q=0)
auto.arima(LTC$close, max.q=0)

#Adf test to find order of lag
adf.test(BNB$close)
adf.test(BTC$close)
adf.test(ETH$close)
adf.test(LTC$close)

#Create a trend variable
trend <- seq_along(BNB)

#Johansen cointegration method
johansen <- ca.jo(data.frame(BNB$close, BTC$close, ETH$close, LTC$close), 
                  type="trace", K=22, ecdet="trend", spec="longrun")
summary(johansen)

#Construct VECM to determine strategy
...

#Check the cointegration relationships with one trend.
Coint_BNB <- (1*BNB$close + 0.03600043*BTC$close - 0.18615000*ETH$close - 7.59178597*LTC$close - 0.02967386*trend)
newdf_BNB <- data.frame(Coint_BNB, BNB$date)
ggplot(newdf_BNB, aes(x=BNB.date, y=Coint_BNB)) + geom_line() + ggtitle("Detrended Binance Coin")
adf.test(Coint_BNB)

Coint_BTC <- (1*BNB$close + 0.002841566*BTC$close - 0.013028599*ETH$close - 0.983434463*LTC$close - 0.043218684*trend)
newdf_BTC <- data.frame(Coint_BTC, BTC$date)
ggplot(newdf_BTC, aes(x=BTC.date, y=Coint_BTC)) + geom_line() + ggtitle("Detrended Bitcoin")
adf.test(Coint_BTC)

Coint_ETH <- (1*BNB$close - 0.008120015*BTC$close - 0.363316606*ETH$close + 1.034854278*LTC$close + 0.116415100*trend)
newdf_ETH <- data.frame(Coint_ETH, ETH$date)
ggplot(newdf_ETH, aes(x=ETH.date, y=Coint_ETH)) + geom_line() + ggtitle("Detrended Etherium")
adf.test(Coint_ETH)

Coint_LTC <- (1*BNB$close - 0.004606477*BTC$close + 0.121899135*ETH$close - 3.507164035*LTC$close - 0.050954104*trend)
newdf_LTC <- data.frame(Coint_LTC, LTC$date)
ggplot(newdf_LTC, aes(x=LTC.date, y=Coint_LTC)) + geom_line() + ggtitle("Detrended Litecoin")
adf.test(Coint_LTC)

#Plotting detrended coint relatioships (tror ikke det er rigtigt)
detrended_coint_ETH <- detrend(Coint_ETH, bp=c(2000, nrow(Coint_ETH)))
newdf_ETH <- data.frame(detrended_coint_ETH, ETH$date)
ggplot(newdf_ETH, aes(x=ETH.date, y=detrended_coint_ETH)) + geom_line() + ggtitle("Detrended Etherium")

detrended_coint_LTC <- detrend(Coint_LTC, bp=c(4000, nrow(Coint_LTC)))
newdf_LTC <- data.frame(detrended_coint_LTC, LTC$date)
ggplot(newdf_LTC, aes(x=LTC.date, y=detrended_coint_LTC)) + geom_line() + ggtitle("Detrended Litecoin")

#Detrending before johansen is applied.






library(RCurl)
x <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
y <- read.csv(text = x)
