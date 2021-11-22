library(readr)
library(tseries)
library(urca)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(pracma)
library(RCurl)
library(tidyverse)
library(vars) 
library(funtimes)

#A function that will fix our data.
fix <- function(x){
  x <- as.data.frame(t(rev(as.data.frame(t(as.data.frame(read.csv(text=x,skip=1)))))))
  x$date <- lubridate::ymd_hms(x$date, tz = "UCT")
  x$close <- as.numeric(x$close)
  x <- na.omit(x)
  return(x)
}

#Loading the data
NEO <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_NEOUSDT_1h.csv")
NEO <- fix(NEO)

BNB <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BNBUSDT_1h.csv")
BNB <- fix(BNB)

BTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BTCUSDT_1h.csv")
BTC <- fix(BTC)

LTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_LTCUSDT_1h.csv")
LTC <- fix(LTC)

ETH <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_ETHUSDT_1h.csv")
ETH <- fix(ETH)

ADA <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_ADAUSDT_1h.csv")
ADA <- fix(ADA)

BAT <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BATUSDT_1h.csv")
BAT <- fix(BAT)

BTT <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BTTUSDT_1h.csv")
BTT <- fix(BTT)

CELR <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_CELRUSDT_1h.csv")
CELR <- fix(CELR)

DASH <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_DASHUSDT_1h.csv")
DASH <- fix(DASH)

EOS <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_EOSUSDT_1h.csv")
EOS <- fix(EOS)

ETC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_ETCUSDT_1h.csv")
ETC <- fix(ETC)

LINK <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_LINKUSDT_1h.csv")
LINK <- fix(LINK)

#Performing no trend test to see if there is a trend or not. (These functions take a long time)
LTC.trend_test <- notrend_test(LTC$close)
BNB.trend_test <- notrend_test(BNB$close)
BTC.trend_test <- notrend_test(BTC$close)
ETH.trend_test <- notrend_test(ETH$close)

#Finding the amount of lags in each model.
VARselect(LTC$close)$selection[3]
VARselect(BNB$close)$selection[3]
VARselect(BTC$close)$selection[3]
VARselect(ETH$close)$selection[3]

#Performing the DF test
summary(ur.df(LTC$close,type="trend"))
summary(ur.df(BNB$close,type="trend"))
summary(ur.df(BTC$close,type="trend"))
summary(ur.df(ETH$close,type="trend"))
#They are all stationary

#We now wish to difference the time series and check for stationarity again.
#First we diff
LTC.diff <- diff(LTC$close,differences=1)
BNB.diff <- diff(BNB$close,differences=1)
BTC.diff <- diff(BTC$close,differences=1)
ETH.diff <- diff(ETH$close,differences=1)
#Then finding the time trend - if there is any. (These functions take a long time)
LTC.trend_test.2 <- notrend_test(LTC.diff)
BNB.trend_test.2 <- notrend_test(BNB.diff)
BTC.trend_test.2 <- notrend_test(BTC.diff)
ETH.trend_test.2 <- notrend_test(ETH.diff)
#Finding the lag
VARselect(LTC.diff)$selection[3]
VARselect(BNB.diff)$selection[3]
VARselect(BTC.diff)$selection[3]
VARselect(ETH.diff)$selection[3]
#Doing the DF test
summary(ur.df(LTC.diff,type="drift"))
summary(ur.df(BNB.diff,type="drift"))
summary(ur.df(BTC.diff,type="drift"))
summary(ur.df(ETH.diff,type="drift"))
#Since there is no trend, we have to test for drift
mean(LTC$close)
mean(BNB$close)
mean(BTC$close)
mean(ETH$close)

#Test and train the data
train.percent <- 0.7

BNB.train<-BNB[1:(nrow(BNB)*train.percent),]
BNB.test<-BNB[(nrow(BNB)*train.percent):nrow(BNB),]

BTC.train<-BTC[1:(nrow(BTC)*train.percent),]
BTC.test<-BTC[(nrow(BTC)*train.percent):nrow(BTC),]

ETH.train<-ETH[1:(nrow(ETH)*train.percent),]
ETH.test<-ETH[(nrow(ETH)*train.percent):nrow(ETH),]

ADA.train<-ADA[1:(nrow(ADA)*train.percent),]
ADA.test<-ADA[(nrow(ADA)*train.percent):nrow(ADA),]

BAT.train<-BAT[1:(nrow(BAT)*train.percent),]
BAT.test<-BAT[(nrow(BAT)*train.percent):nrow(BAT),]

BTT.train<-BTT[1:(nrow(BTT)*train.percent),]
BTT.test<-BTT[(nrow(BTT)*train.percent):nrow(BTT),]

CELR.train<-CELR[1:(nrow(CELR)*train.percent),]
CELR.test<-CELR[(nrow(CELR)*train.percent):nrow(CELR),]

DASH.train<-DASH[1:(nrow(DASH)*train.percent),]
DASH.test<-DASH[(nrow(DASH)*train.percent):nrow(DASH),]

EOS.train<-EOS[1:(nrow(EOS)*train.percent),]
EOS.test<-EOS[(nrow(EOS)*train.percent):nrow(EOS),]

NEO.train<-NEO[1:(nrow(NEO)*train.percent),]
NEO.test<-NEO[(nrow(NEO)*train.percent):nrow(NEO),]

ETC.train<-ETC[1:(nrow(ETC)*train.percent),]
ETC.test<-ETC[(nrow(ETC)*train.percent):nrow(ETC),]

LINK.train<-LINK[1:(nrow(LINK)*train.percent),]
LINK.test<-LINK[(nrow(LINK)*train.percent):nrow(LINK),]


#Gather the training data
list.of <- list(BNB.train,BTC.train,ETH.train,NEO.train,ADA.train,BAT.train,BTT.train,CELR.train,DASH.train,EOS.train,ETC.train,LINK.train)
list.of <- list(BNB,BTC,ETH,LTC,NEO,ADA,BAT,BTT,CELR,DASH,EOS,ETC,LINK)

list.of <- list(BNB,BTC,ETH,EOS,ETC,LINK)
list.of <- list(BNB.train,BTC.train,ETH.train,BTT.train,CELR.train,DASH.train,EOS.train,ETC.train,LINK.train)
plot(LTC.train$close)
#This function takes the training data and uses to find Johansen and then the linear combination.
make.comb <- function(list.of,which){
  save.info <- list.of[[1]]$close
  for(i in 2:length(list.of)){
    save.info <- cbind(save.info,list.of[[i]]$close)
  }
  lag.select <- VARselect(save.info, type = "trend",lag.max=20)$selection[3]
  if(lag.select==1){
    lag.select <- 2
  }
  print(VARselect(save.info, type = "trend",lag.max=20)$selection[3])
  johansen <- ca.jo(save.info, type="trace", K=lag.select, ecdet="trend", spec="longrun")
  Coint <- numeric(length(list.of[[1]]$close))
  for(j in 1:length(list.of)){
    print(j)
    Coint <- Coint+ johansen@V[j,which]*list.of[[j]]$close
  }
  Coint <- Coint + johansen@V[length(list.of)+1,which]*seq_along(list.of[[1]]$close)
  together <- data.frame(Coint, list.of[[1]]$date)
  p <- ggplot(together, aes(x=together[,2], y=Coint)) + geom_line() + ggtitle("")
  print(p)
  DF <- adf.test(Coint)
  everything <- list(Coint,johansen,p,DF)
  return(everything)
}

try <- make.comb(list.of,1)
summary(try[[2]])
qqline(try[[1]])
summary(ur.df(try[[1]],type="drift"))

