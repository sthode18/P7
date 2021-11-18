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

#Load the data
fix <- function(x){
  x <- as.data.frame(t(rev(as.data.frame(t(as.data.frame(read.csv(text=x)))))))
  x$date <- lubridate::ymd_hms(x$date, tz = "UCT")
  x$close <- as.numeric(x$close)
  x <- na.omit(x)
  return(x)
}

BNB <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_BNBUSDT_1h.csv")
BNB <- fix(BNB)

BTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_BTCUSDT_1h.csv")
BTC <- fix(BTC)

ETH <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_ETHUSDT_1h.csv")
ETH <- fix(ETH)

LTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_LTCUSDT_1h.csv")
LTC <- fix(LTC)

#Test and train the data
BNB.dt <- sort(sample(nrow(BNB), nrow(BNB)*.7))
BNB.train<-BNB[BNB.dt,]
BNB.test<-BNB[-BNB.dt,]

BTC.dt <- sort(sample(nrow(BTC), nrow(BTC)*.7))
BTC.train<-BTC[BTC.dt,]
BTC.test<-BTC[-BTC.dt,]

ETH.dt <- sort(sample(nrow(ETH), nrow(ETH)*.7))
ETH.train<-ETH[ETH.dt,]
ETH.test<-ETH[-ETH.dt,]

LTC.dt <- sort(sample(nrow(LTC), nrow(LTC)*.7))
LTC.train<-LTC[LTC.dt,]
LTC.test<-LTC[-LTC.dt,]


#Gather the training data
list.of <- list(BNB.train,BTC.train,ETH.train,LTC.train)

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
  print(VARselect(save.info, type = "trend",lag.max=20))
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
  everything <- list(Coint,together,p,DF)
  return(everything)
}

try <- make.comb(list.of,1)
