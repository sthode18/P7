#Load packages
pacman::p_load(readr, tseries, urca, ggplot2, dplyr, lubridate, forecast,
               pracma, vars, stats, car)

#Define the data
#Order the data by dates such that they are in ascending order
#Remove NA rows for dates
#Close -> numeric and date -> date
BNB <- readr::read_delim("Binance_BNBUSDT_1h.csv", delim = ",")
BNB <- as.data.frame(BNB)
BNB <- t(BNB)
BNB <- as.data.frame(BNB)
BNB <- rev(BNB)
BNB <- t(BNB)
BNB <- as.data.frame(BNB)
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
LTC$date <- lubridate::ymd_hms(LTC$date, tz = "UCT")
LTC$close <- as.numeric(LTC$close)
LTC <- na.omit(LTC)

#Plot of time series
ggplot(BNB, aes(x=date, y=close)) + geom_line() + xlab("Date") + ylab("USD")
ggplot(BTC, aes(x=date, y=close)) + geom_line() + xlab("Date") + ylab("USD")
ggplot(ETH, aes(x=date, y=close)) + geom_line() + xlab("Date") + ylab("USD")
ggplot(LTC, aes(x=date, y=close)) + geom_line() + xlab("Date") + ylab("USD")

#Checking the order of integration of the individual series
arBNB <- auto.arima(BNB$close, max.q=0)
arBTC <- auto.arima(BTC$close, max.q=0)
arETH <- auto.arima(ETH$close, max.q=0)
arLTC <- auto.arima(LTC$close, max.q=0)

#Detrending the series to check for seasonality and trend
dc_BNB <- decompose(ts(BNB$close, start=c(1,1), frequency = 52.1429))
plot(dc_BNB)
dc_BTC <- decompose(ts(BTC$close, start=c(1,1), frequency = 52.1429))
plot(dc_BTC)

#Q-Q plots
qqnorm(BNB$close, pch = 1, frame = FALSE)
qqline(BNB$close, col = "steelblue", lwd = 2)

#Adf test to find order of lag
summary(ur.df(BNB$close))
summary(ur.df(BTC$close))
summary(ur.df(ETH$close))
summary(ur.df(LTC$close))

#Check for trend maybe idk (R^2_{adj}). Maybe after Johansen?
...

#Check for seasonality maybe idk (ACF)
...

#Create a trend variable
trend <- seq_along(BNB)

#Select value of p
VARselect(data.frame(BNB$close, BTC$close, ETH$close, LTC$close))

#Johansen cointegration method
johansen <- ca.jo(data.frame((BNB$close), (BTC$close), (ETH$close), (LTC$close)), 
                  type="trace", K=2, ecdet="trend", spec="longrun")
summary(johansen)

#Construct VECM to determine strategy??
...

#Check the cointegration relationships.
#Binance Coin
Coint_BNB <- (BNB$close)-(- 0.02899906*(BTC$close) + 0.30624763*(ETH$close) + 5.33532556*(LTC$close) - 0.11097319*trend)

newdf_BNB <- data.frame(Coint_BNB, BNB$date)
ggplot(newdf_BNB, aes(x=BNB.date, y=Coint_BNB)) + geom_line() + ggtitle("Binance Coin-ish")

detrended_coint_BNB <- detrend(Coint_BNB)
newdf_BNB <- data.frame(detrended_coint_BNB, BNB$date)
ggplot(newdf_BNB, aes(x=BNB.date, y=detrended_coint_BNB)) + geom_line() + ggtitle("Detrended Binance Coin-ish")

summary(ur.df(detrended_coint_BNB))

#Bitcoin
Coint_BTC <- (BTC$close) - (74.8828644792*(BNB$close) - 74.8828644792*0.01904117*(ETH$close) - 74.8828644792*3.03633913*(LTC$close) - 74.8828644792*0.05657007*trend)

newdf_BTC <- data.frame(Coint_BTC, BTC$date)
ggplot(newdf_BTC, aes(x=BTC.date, y=Coint_BTC)) + geom_line() + ggtitle("Bitcoin-ish")

detrended_coint_BTC <- detrend(Coint_BTC)
newdf_BTC <- data.frame(detrended_coint_BTC, BTC$date)
ggplot(newdf_BTC, aes(x=BTC.date, y=detrended_coint_BTC)) + geom_line() + ggtitle("Detrended Bitcoin-ish")

summary(ur.df(detrended_coint_BTC))

#Etherium
Coint_ETH <- (ETH$close) - (-3.7150307085*(BNB$close) + 3.7150307085*0.005361963*(BTC$close) - 3.7150307085*0.516022721*(LTC$close) - 3.7150307085*0.071618578*trend)
newdf_ETH <- data.frame(Coint_ETH, ETH$date)
ggplot(newdf_ETH, aes(x=ETH.date, y=Coint_ETH))+ geom_line() + ggtitle("Detrended Etherium-ish")

detrended_coint_ETH <- detrend(Coint_ETH)
newdf_ETH <- data.frame(detrended_coint_ETH, ETH$date)
ggplot(newdf_ETH, aes(x=ETH.date, y=detrended_coint_ETH)) + geom_line() + ggtitle("Detrended Etherium")

summary(ur.df(detrended_coint_ETH))

#Litecoin
Coint_LTC <- (LTC$close) - (-0.28037780999*(BNB$close) + 0.00330521*0.28037780999*(BTC$close) - 0.12893916*0.28037780999*(ETH$close) + 0.05881550*0.28037780999*trend)
newdf_LTC <- data.frame(Coint_LTC, LTC$date)
ggplot(newdf_LTC, aes(x=LTC.date, y=Coint_LTC)) + geom_line() + ggtitle("Litecoin-ish")

detrended_coint_LTC <- detrend(Coint_LTC)
newdf_LTC <- data.frame(detrended_coint_LTC, LTC$date)
ggplot(newdf_LTC, aes(x=LTC.date, y=detrended_coint_LTC)) + geom_line() + ggtitle("Detrended Litecoin-ish")

summary(ur.df(detrended_coint_LTC))
