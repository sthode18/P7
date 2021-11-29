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
library(gridExtra)
library(ggpubr)


#A function that will fix our data.
fix <- function(x){
  x <- as.data.frame(t(rev(as.data.frame(t(as.data.frame(read.csv(text=x,skip=1)))))))
  x$date <- lubridate::ymd_hms(x$date, tz = "UCT")
  x$close <- as.numeric(x$close)
  x <- na.omit(x)
  return(x)
}
fix2 <- function(x){
  x <- as.data.frame(t(rev(as.data.frame(t(as.data.frame(read.csv(text=x)))))))
  x$date <- lubridate::ymd_hms(x$date, tz = "UCT")
  x$close <- as.numeric(x$close)
  x <- na.omit(x)
  return(x)
}

#Loading the data. You have two different to choose from. The load.some is recommended.
load.all <- function(){
  NEO <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_NEOUSDT_1h.csv")
  NEO <<- fix(NEO)
  BNB <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BNBUSDT_1h.csv")
  BNB <<- fix(BNB)
  BTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BTCUSDT_1h.csv")
  BTC <<- fix(BTC)
  LTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_LTCUSDT_1h.csv")
  LTC <<- fix(LTC)
  ETH <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_ETHUSDT_1h.csv")
  ETH <<- fix(ETH)
  ADA <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_ADAUSDT_1h.csv")
  ADA <<- fix(ADA)
  BAT <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BATUSDT_1h.csv")
  BAT <<- fix(BAT)
  BTT <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_BTTUSDT_1h.csv")
  BTT <<- fix(BTT)
  CELR <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_CELRUSDT_1h.csv")
  CELR <<- fix(CELR)
  DASH <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_DASHUSDT_1h.csv")
  DASH <<- fix(DASH)
  EOS <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_EOSUSDT_1h.csv")
  EOS <<- fix(EOS)
  ETC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_ETCUSDT_1h.csv")
  ETC <<- fix(ETC)
  LINK <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Data/Binance_LINKUSDT_1h.csv")
  LINK <<- fix(LINK)  
}
load.some <- function(){
  BNB <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_BNBUSDT_1h.csv")
  BNB <<- fix2(BNB)
  BTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_BTCUSDT_1h.csv")
  BTC <<- fix2(BTC)
  LTC <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_LTCUSDT_1h.csv")
  LTC <<- fix2(LTC)
  ETH <- getURL("https://raw.githubusercontent.com/sthode18/P7/main/Binance_ETHUSDT_1h.csv")
  ETH <<- fix2(ETH)
}
#load.all()
load.some()


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
#They are all non-stationary

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
summary(ur.df(LTC.diff,type="none"))
summary(ur.df(BNB.diff,type="none"))
summary(ur.df(BTC.diff,type="none"))
summary(ur.df(ETH.diff,type="none"))
#Since there is no trend, we have to test for drift
mean(LTC.diff)
mean(BNB.diff)
mean(BTC.diff)
mean(ETH.diff)

#Graphs for differenced time series:
index.numbers <- c(1:length(LTC.diff))
LTC.diff.df <- data.frame(LTC$date[-1],LTC.diff)
BNB.diff.df <- data.frame(BNB$date[-1],BNB.diff)
BTC.diff.df <- data.frame(BTC$date[-1],BTC.diff)
ETH.diff.df <- data.frame(ETH$date[-1],ETH.diff)

#3.75*5.2
LTC.plot <- ggplot(LTC.diff.df,aes(x=index.numbers,y=LTC.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Litecoin")+ylim(-70,70)
BNB.plot <- ggplot(BNB.diff.df,aes(x=index.numbers,y=BNB.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Binance Coin")+ylim(-70, 70)
BTC.plot <- ggplot(BTC.diff.df,aes(x=index.numbers,y=BTC.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Bitcoin")+ylim(-4000, 4000)
ETH.plot <- ggplot(ETH.diff.df,aes(x=index.numbers,y=ETH.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Etherium")+ylim(-450, 450)

ggarrange(LTC.plot, BNB.plot, BTC.plot,ETH.plot,
          ncol = 2, nrow = 2)

#Gather the training data
#list.of <- list(BNB.train,BTC.train,ETH.train,NEO.train,ADA.train,BAT.train,BTT.train,CELR.train,DASH.train,EOS.train,ETC.train,LINK.train)
#list.of <- list(BNB,BTC,ETH,LTC,NEO,ADA,BAT,BTT,CELR,DASH,EOS,ETC,LINK)

list.of <- list(BNB.train,BTC.train,ETH.train,LTC.train)
list.of <- list(BNB,BTC,ETH,LTC)

#list.of <- list(BNB,BTC,ETH,EOS,ETC,LINK)
#list.of <- list(BNB.train,BTC.train,ETH.train,BTT.train,CELR.train,DASH.train,EOS.train,ETC.train,LINK.train)
#This function takes the training data and uses to find Johansen and then the linear combination.
#list.of = list.of,which = 1, type="trend"

make.comb <- function(list.of = list.of,which = 1,trend=T,res=F){
  save.info <- list.of[[1]]$close
  for(i in 2:length(list.of)){
    save.info <- cbind(save.info,list.of[[i]]$close)
  }
  lag.select <- VARselect(save.info, type = "trend",lag.max=20)$selection[3]
  if(lag.select==1){
    lag.select <- 2
    print("lag changed")
  }
  print(VARselect(save.info, type = "trend",lag.max=20)$selection[3])
  johansen <- ca.jo(save.info, type="eigen", K=lag.select, ecdet="trend", spec="longrun")
  Coint <- numeric(length(list.of[[1]]$close)-lag.select)
  length.fun <- length(list.of)
  vec.ts <- johansen@ZK
  if(trend==T){
    length.fun <- length.fun+1
  }
  if(res==T){
    vec.ts <- johansen@RK
  }
  for(j in 1:length.fun){
    Coint <- Coint + johansen@ZK[,j]*johansen@V[j,which]
  }
  together <- data.frame(Coint, list.of[[1]]$date[-c(1:lag.select)])
  p <- ggplot(together, aes(x=together[,2], y=together[,1])) + geom_line() + labs(y="Value", x = "Date",title="Cointegration something for..")
  print(p)
  DF <- summary(ur.df(Coint))
  everything <- list(Coint,johansen,p,DF,together)
  return(everything)
}
try <- make.comb(list.of,2,trend=T,res=F)
try[[4]]



#Prøver at følge side 133 i pfaff
H1 <- try[[2]]
beta <- H1@V 
H1@V 
beta[,2] <- beta[,2]/beta[4,2] 
beta[,3] <- beta[,3]/beta[4,3]
alpha <- H1@PI%*%solve(t(beta)) 
beta1 <- cbind(beta [ ,1:2] , H1@V[ ,3:4])
nrow(beta1)
beta1 <- beta1[-nrow(beta1),]
beta1
ci.1 <- ts((H1@x%*%beta1))
ci.2 <- ts(H1@RK[,-ncol(H1@RK)]%*%beta1)



plot(ci.1)
plot(ci.2)
#3.75*5.2

