library(readr)
library(tseries)
library(urca)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(pracma)


#Define the data
#Order the data by dates such that they are in ascending order
#Remove NA rows for dates
#Keep only date and close price
#Close -> numeric and date -> date
BNB <- readr::read_delim("Binance_BNBUSDT_1h.csv", delim = ",")
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
  
BTC <- readr::read_delim("Binance_BTCUSDT_1h.csv", delim = ",")
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
      
ETH <- readr::read_delim("Binance_ETHUSDT_1h.csv", delim = ",")
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

LTC <- readr::read_delim("Binance_LTCUSDT_1h.csv", delim = ",")
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
johansen <- ca.jo(data.frame((BNB$close), (BTC$close), (ETH$close), (LTC$close)), 
                  type="trace", K=9, ecdet="none", spec="longrun")
summary(johansen)

#Construct VECM to determine strategy
...

#Check the cointegration relationships.
#Binance Coin
Coint_BNB <- (BNB$close)-(1*(BNB$close) + 0.04732263*(BTC$close) - 0.31884579*(ETH$close) - 9.64491974*(LTC$close))
newdf_BNB <- data.frame(Coint_BNB, BNB$date)
ggplot(newdf_BNB, aes(x=BNB.date, y=Coint_BNB)) + geom_line() + ggtitle("Binance Coin-ish")

detrended_coint_BNB <- detrend(Coint_BNB)
newdf_BNB <- data.frame(detrended_coint_BNB, BNB$date)
ggplot(newdf_BNB, aes(x=BNB.date, y=detrended_coint_BNB)) + geom_line() + ggtitle("Detrended Binance Coin-ish")

summary(ur.df(detrended_coint_BNB))

#Bitcoin
Coint_BTC <- (BTC$close) - (1*(BNB$close) + 0.00160012*(BTC$close) - 0.12836658*(ETH$close) - 0.32779104*(LTC$close))
newdf_BTC <- data.frame(Coint_BTC, BTC$date)
ggplot(newdf_BTC, aes(x=BTC.date, y=Coint_BTC)) + geom_line() + ggtitle("Bitcoin-ish")

detrended_coint_BTC <- detrend(Coint_BTC)
newdf_BTC <- data.frame(detrended_coint_BTC, BTC$date)
ggplot(newdf_BTC, aes(x=BTC.date, y=detrended_coint_BTC)) + geom_line() + ggtitle("Detrended Bitcoin-ish")

summary(ur.df(detrended_coint_BTC))

#Etherium
Coint_ETH <- (ETH$close) - (1*(BNB$close) - 0.00486006*(BTC$close) - 0.04659237*(ETH$close) - 1.81662123*(LTC$close))
newdf_ETH <- data.frame(Coint_ETH, ETH$date)
ggplot(newdf_ETH, aes(x=ETH.date, y=Coint_ETH)) + geom_line() + ggtitle("Detrended Etherium-ish")

detrended_coint_ETH <- detrend(Coint_ETH)
newdf_ETH <- data.frame(detrended_coint_ETH, ETH$date)
ggplot(newdf_ETH, aes(x=ETH.date, y=detrended_coint_ETH)) + geom_line() + ggtitle("Detrended Etherium")

summary(ur.df(detrended_coint_ETH))

#Litecoin
Coint_LTC <- (LTC$close) - (1*(BNB$close) - 0.002872576*(BTC$close) - 0.900412684*(ETH$close) + 7.850815307*(LTC$close))
newdf_LTC <- data.frame(Coint_LTC, LTC$date)
ggplot(newdf_LTC, aes(x=LTC.date, y=Coint_LTC)) + geom_line() + ggtitle("Litecoin-ish")

detrended_coint_LTC <- detrend(Coint_LTC, bp=c(4000, nrow(Coint_LTC)))
newdf_LTC <- data.frame(detrended_coint_LTC, LTC$date)
ggplot(newdf_LTC, aes(x=LTC.date, y=detrended_coint_LTC)) + geom_line() + ggtitle("Detrended Litecoin-ish")

summary(ur.df(detrended_coint_LTC))

#Detrending before johansen is applied.
