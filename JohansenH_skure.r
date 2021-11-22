#Load packages
pacman::p_load(readr, tseries, urca, ggplot2, dplyr, lubridate, forecast,
               pracma, vars, stats, tsDyn)

#Define the data
#Order the data by dates such that they are in ascending order
fix <- function(x){
  as.data.frame(t(rev(as.data.frame(t(as.data.frame(readr::read_delim(x, delim = ",")))))))
}

#Fix the time-series
#Close -> numeric and date -> date
#Remove NA rows for dates
BNB <- fix("Binance_BNBUSDT_1h.csv")
  BNB$date <- lubridate::ymd_hms(BNB$date, tz = "UCT")
  BNB$close <- as.numeric(BNB$close)
    BNB <- na.omit(BNB)

BTC <- fix("Binance_BTCUSDT_1h.csv")
  BTC$date <- lubridate::ymd_hms(BTC$date, tz = "UCT")
  BTC$close <- as.numeric(BTC$close)
    BTC <- na.omit(BTC)

ETH <- fix("Binance_ETHUSDT_1h.csv")
  ETH$date <- lubridate::ymd_hms(ETH$date, tz = "UCT")
  ETH$close <- as.numeric(ETH$close)
    ETH <- na.omit(ETH)

LTC <- fix("Binance_LTCUSDT_1h.csv")
  LTC$date <- lubridate::ymd_hms(LTC$date, tz = "UCT")
  LTC$close <- as.numeric(LTC$close)
    LTC <- na.omit(LTC)

#Plot of time series
tsplot <- function(z){
    ggplot(z, aes(x=date, y=close)) + geom_line() + xlab("Date") + ylab("USD")
}

tsplot(BNB)
tsplot(BTC)
tsplot(ETH)
tsplot(LTC)

#Decomposing the series to check for seasonality and trend
dcompose <- function(x){
plot(decompose(ts(x$close, start=c(1,1), frequency = 52.1429)))
}

dcompose(BNB)
dcompose(BTC)
dcompose(ETH)
dcompose(LTC)

#Checking the order of integration of the individual series
auto.arima(BNB$close, max.q=0)
auto.arima(BTC$close, max.q=0)
auto.arima(ETH$close, max.q=0)
auto.arima(LTC$close, max.q=0)

#Q-Q plots
fuck <- auto.arima(BNB[1:9000, ]$close, max.q=0)
qqnorm(fuck, pch = 1, frame = FALSE)
qqline(fuck, col = "steelblue", lwd = 2)

#Adf test to find order of lag
summary(ur.df(BNB[1:9000, ]$close))
summary(ur.df(BTC[1:9000, ]$close))
summary(ur.df(ETH[1:9000, ]$close))
summary(ur.df(LTC[1:9000, ]$close))

#Check for trend maybe idk (R^2_{adj}). Maybe after Johansen?
...

#Check for seasonality maybe idk (ACF)
...

#Create a trend variable
trend <- seq_along(BNB$close)

#Select value of p
all_coins <- data.frame(BNB$close, BTC$close, ETH$close, LTC$close)
VARselect(all_coins)

#Johansen cointegration method
johansen <- ca.jo(all_coins[1:8800,], type="eigen", K=2, 
                  ecdet="trend", spec="longrun")
summary(johansen)

plot_residuals <- function(z){
  fuck <- johansen@R0[,z]
  if (z == 1){
    newdf_BNB_n <- data.frame(fuck, BNB[1:8798,]$date)
    print(ggplot(newdf_BNB_n, aes(x=BNB.1.8798....date, y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Binance residuals"))
  }
  if (z == 2){
    newdf_BNB_n <- data.frame(fuck, BTC[1:8798,]$date)
    print(ggplot(newdf_BNB_n, aes(x=BTC.1.8798....date, y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Bitcoin residuals"))
  }
  if (z == 3){
    newdf_BNB_n <- data.frame(fuck, ETH[1:8798,]$date)
    print(ggplot(newdf_BNB_n, aes(x=ETH.1.8798....date, y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Etherium residuals"))
  }
  if (z == 4){
    newdf_BNB_n <- data.frame(fuck, LTC[1:8998,]$date)
    print(ggplot(newdf_BNB_n, aes(x=LTC.1.8798....date, y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Litecoin residuals"))
  }
}

plot_residuals(1)

plot_qq <- function(z){
qqnorm(johansen@R0[,z], pch = 1, frame = FALSE) 
qqline(johansen@R0[,z], col = "steelblue", lwd = 2)
}

plot_qq(1)

plot(density(fuck, width = 100))
#Try some shit, så vi ikke behøver ctrl c + ctrl v hvert tal
#Aka linear combinations wrt. cointegration vector
finalplots <- function(z, ecdet){
  if (ecdet == "none"){
    wtf <- BNB$close - johansen@V[2, z]*BTC$close + johansen@V[3, z]*ETH$close + johansen@V[4, z]*LTC$close
  }
  if (ecdet == "trend"){
    wtf <- BNB$close - johansen@V[2, z]*BTC$close + johansen@V[3, z]*ETH$close + johansen@V[4, z]*LTC$close + johansen@V[5, z]*trend
  }
  newdf_BNB <- data.frame(wtf, BNB$date)[9001:11000, ]
  ggplot(newdf_BNB_n, aes(x=BNB.date, y=wtf)) + geom_line() + ggtitle("Binance differenced-ish")
}

finalplots_dt <- function(z, ecdet){
  if (ecdet == "none"){
    wtf <- BNB$close - johansen@V[2, z]*BTC$close + johansen@V[3, z]*ETH$close + johansen@V[4, z]*LTC$close
  }
  if (ecdet == "trend"){
    wtf <- BNB$close - johansen@V[2, z]*BTC$close + johansen@V[3, z]*ETH$close + johansen@V[4, z]*LTC$close + johansen@V[5, z]*trend
  }
  wtf_dt <- detrend(wtf)
  newdf_BNB_n <- data.frame(wtf_dt, BNB$date)[9001:11000, ]
  ggplot(newdf_BNB_n, aes(x=BNB.date, y=wtf_dt)) + geom_line() + ggtitle("Detrended Binance differenced-ish")
}

finalplots(1, ecdet = "trend")

finalplots_dt(1, ecdet = "trend")
#Construct VECM to determine strategy??
library(tsDyn)
est_tsdyn <- VECM(all_coins, 2, r = 1, include = "none", estim = "ML", exogen = trend)
summary(est_tsdyn)

#Impulse response analysis
var <- vec2var(johansen, r = 1)
#Irf for BNB
ir <- irf(var, n.ahead = 20, impulse = "BNB.close", response = "BTC.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "BNB.close", response = "ETH.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "BNB.close", response = "LTC.close", ortho = FALSE, runs = 500)
#Irf for BTC
ir <- irf(var, n.ahead = 20, impulse = "BTC.close", response = "BNB.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "BTC.close", response = "ETH.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "BTC.close", response = "LTC.close", ortho = FALSE, runs = 50)
#Irf for ETH
ir <- irf(var, n.ahead = 20, impulse = "ETH.close", response = "BNB.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "ETH.close", response = "BTC.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "ETH.close", response = "LTC.close", ortho = FALSE, runs = 50)
#Irf for LTC
ir <- irf(var, n.ahead = 20, impulse = "LTC.close", response = "BNB.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "LTC.close", response = "BTC.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "LTC.close", response = "ETH.close", ortho = FALSE, runs = 50)

plot(ir)


#First linear combination without trend
Coint_BNB_n <- BNB$close - johansen@V[2, 1]*BTC$close + johansen@V[3, 1]*ETH$close + johansen@V[4, 1]*LTC$close
newdf_BNB_n <- data.frame(Coint_BNB_n, BNB$date)[9001:11000, ]
ggplot(newdf_BNB_n, aes(x=BNB.date, y=Coint_BNB_n)) + geom_line() + ggtitle("Binance Coin-ish")

detrended_coint_BNB_n <- detrend(Coint_BNB_n)
newdf_BNB_n <- data.frame(detrended_coint_BNB_n, BNB$date)[9001:11000, ]
ggplot(newdf_BNB_n, aes(x=BNB.date, y=detrended_coint_BNB_n)) + geom_line() + ggtitle("Detrended Binance Coin-ish")

summary(ur.df(detrended_coint_BNB_n))

#First linear combination with trend
Coint_BNB <- (BNB$close)-(- 0.1943329*(BTC$close) - 0.8997849*(ETH$close) + 46.8879068*(LTC$close) + 0.2883075*trend)
newdf_BNB <- data.frame(Coint_BNB, BNB$date)[9001:11000, ]
ggplot(newdf_BNB, aes(x=BNB.date, y=Coint_BNB)) + geom_line() + ggtitle("Binance Coin-ish")

detrended_coint_BNB <- detrend(Coint_BNB)
newdf_BNB <- data.frame(detrended_coint_BNB, BNB$date)[9001:11000, ]
ggplot(newdf_BNB, aes(x=BNB.date, y=detrended_coint_BNB)) + geom_line() + ggtitle("Detrended Binance Coin-ish")

summary(ur.df(detrended_coint_BNB))

#Second linear combination without trend
Coint_BTC_n <- (BNB$close) - (+ 0.006208809*(BTC$close) - 0.155704826*(ETH$close) - 1.096283069*(LTC$close))
newdf_BTC_n <- data.frame(Coint_BTC_n, BTC$date)[9001:11000, ]
ggplot(newdf_BTC_n, aes(x=BTC.date, y=Coint_BTC_n)) + geom_line() + ggtitle("Bitcoin-ish")

detrended_coint_BTC_n <- detrend(Coint_BTC_n)
newdf_BTC_n <- data.frame(detrended_coint_BTC_n, BTC$date)[9001:11000, ]
ggplot(newdf_BTC_n, aes(x=BTC.date, y=detrended_coint_BTC_n)) + geom_line() + ggtitle("Detrended Bitcoin-ish")

summary(ur.df(detrended_coint_BTC_n))

#Third linear combination without trend
Coint_ETH_n <- (BNB$close) - (- 0.003601332*(BTC$close) - 0.038975259*(ETH$close) - 1.976053994*(LTC$close))
newdf_ETH_n <- data.frame(Coint_ETH_n, ETH$date)[9001:11000, ]
ggplot(newdf_ETH_n, aes(x=ETH.date, y=Coint_ETH_n)) + geom_line() + ggtitle("Etherium-ish")

detrended_coint_ETH_n <- detrend(Coint_ETH_n)
newdf_ETH_n <- data.frame(detrended_coint_ETH_n, ETH$date)[9001:11000, ]
ggplot(newdf_ETH_n, aes(x=ETH.date, y=detrended_coint_ETH_n)) + geom_line() + ggtitle("Detrended Etherium")

summary(ur.df(detrended_coint_ETH_n))

#Third linear combination without trend
Coint_LTC_n <- (BNB$close) - (- 0.00824494*(BTC$close) - 0.77375033*(ETH$close) + 6.83330601*(LTC$close))
newdf_LTC_n <- data.frame(Coint_LTC_n, LTC$date)[9001:11000, ]
ggplot(newdf_LTC_n, aes(x=LTC.date, y=Coint_LTC_n)) + geom_line() + ggtitle("Litecoin-ish")

detrended_coint_LTC_n <- detrend(Coint_LTC_n)
newdf_LTC_n <- data.frame(detrended_coint_LTC_n, LTC$date)[9001:11000, ]
ggplot(newdf_LTC_n, aes(x=LTC.date, y=detrended_coint_LTC_n)) + geom_line() + ggtitle("Detrended Litecoin-ish")

summary(ur.df(detrended_coint_LTC))

