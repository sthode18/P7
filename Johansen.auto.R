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
library(plm)
library(latex2exp)

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
qqplotz <- function(x,johansen,title.gg){
  df_stdrs <- data.frame("stdrs" = johansen@R0[,x])
  ggplot(df_stdrs, aes(sample = stdrs)) + stat_qq() + stat_qq_line() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle(title.gg)
}
#load.all()
load.some()

#Performing no trend test to see if there is a trend or not. (These functions take a long time)
#LTC.trend_test <- notrend_test(LTC$close)
#BNB.trend_test <- notrend_test(BNB$close)
#BTC.trend_test <- notrend_test(BTC$close)
#ETH.trend_test <- notrend_test(ETH$close)
LTC.trend_test
BNB.trend_test
BTC.trend_test
ETH.trend_test

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
model.LTC.diff <- lm(LTC.diff ~ c(1:length(LTC.diff)))
summary(model.LTC.diff)
#Graphs for differenced time series:
index.numbers <- c(1:length(LTC.diff))
LTC.diff.df <- data.frame(LTC$date[-1],LTC.diff)
BNB.diff.df <- data.frame(BNB$date[-1],BNB.diff)
BTC.diff.df <- data.frame(BTC$date[-1],BTC.diff)
ETH.diff.df <- data.frame(ETH$date[-1],ETH.diff)

LTC.plot <- ggplot(LTC.diff.df,aes(x=index.numbers,y=LTC.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Litecoin")+ylim(-70,70)
BNB.plot <- ggplot(BNB.diff.df,aes(x=index.numbers,y=BNB.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Binance Coin")+ylim(-70, 70)
BTC.plot <- ggplot(BTC.diff.df,aes(x=index.numbers,y=BTC.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Bitcoin")+ylim(-4000, 4000)
ETH.plot <- ggplot(ETH.diff.df,aes(x=index.numbers,y=ETH.diff)) + geom_line()+
  labs(y="Difference", x = "Index",title="Differenced Etherium")+ylim(-450, 450)

ggarrange(BNB.plot, BTC.plot,ETH.plot, LTC.plot,
          ncol = 2, nrow = 2) #3.75*5.2

#This function does a Johansens test on some data and finds the linear combination.
#Input:
#1: list of the ts that will be looked at. Does not have to be on ts form.
#2: Which linear combination that is inspected
#3: If res=F we make the linear combination of yt. If res=T we do it of the residuals R.
#4: What type of johansens test we perform. Can choose: "trace or "eigen"
#5: What type of johansens test we perform. Can choose: "trend", "const" or "none".
#Output:
#The output is a list of length 4. 
#1: The linear combination as a ts.
#2: Output of Johansens test
#3: Plot of linear combination
#4: DF test on the linear comb. 
make.comb <- function(list.of = list.of,lin.comb.numb = 1,res=F,type.jo="trace",ecdet="trend",title.gg="Linear combination",title.gg.size=11){
  save.info <- list.of[[1]]$close
  for(i in 2:length(list.of)){
    save.info <- cbind(save.info,list.of[[i]]$close)
  }
  lag.select <- VARselect(save.info, type = ecdet,lag.max=20)$selection[3]
  if(lag.select==1){
    lag.select <- 2
  }
  print(VARselect(save.info, type = ecdet,lag.max=20)$selection[3])
  johansen <- ca.jo(save.info, type= type.jo, K=lag.select, ecdet=ecdet, spec="longrun")
  Coint <- numeric(length(list.of[[1]]$close)-lag.select)
  length.fun <- length(list.of)
  vec.ts <- johansen@ZK
  if(ecdet=="trend"){
    length.fun <- length.fun+1
  }
  if(res==T){
    vec.ts <- johansen@RK
  }
  for(j in 1:length.fun){
    Coint <- Coint + vec.ts[,j]*johansen@V[j,lin.comb.numb]
  }
  together <- data.frame(Coint, list.of[[1]]$date[-c(1:lag.select)])
  p <- ggplot(together, aes(x=together[,2], y=together[,1])) + geom_line() + labs(y="USD", x = "Date",title=title.gg)+ theme(plot.title = element_text(size=title.gg.size))
  print(p)
  DF <- summary(ur.df(Coint))
  everything <- list(Coint,johansen,p,DF)
  return(everything)
}
example <- make.comb(all.crypto,lin.comb.numb=1,res=F,type.jo="eigen",ecdet="trend",title.gg="Linear combination")
example[[2]]@ZK

#Using the function on all 4 crypto currencies. Doing it for 1st and 2nd linear combination. with eigen
all.crypto <- list(BNB,BTC,ETH,LTC)
coint.all.eigen.1 <- make.comb(all.crypto,lin.comb.numb=1,res=F,type.jo="eigen",ecdet="trend",title.gg="First Linear Combination")
coint.all.eigen.2 <- make.comb(all.crypto,lin.comb.numb=2,res=F,type.jo="eigen",ecdet="trend",title.gg="Second Linear Combination")
coint.all.eigen.3 <- make.comb(all.crypto,lin.comb.numb=3,res=F,type.jo="eigen",ecdet="trend",title.gg="Third Linear Combination")
coint.all.eigen.4 <- make.comb(all.crypto,lin.comb.numb=4,res=F,type.jo="eigen",ecdet="trend",title.gg="Fourth Linear Combination")


#Creating plot for all 4
all.lin.comb.plot <- ggarrange(coint.all.eigen.1[[3]],coint.all.eigen.2[[3]],coint.all.eigen.3[[3]],
                               coint.all.eigen.4[[3]],nrow=2,ncol=2);all.lin.comb.plot




#5x7.5.
#Testing for trend(Careful about the notrend. takes a long time) with eigen
#coint.all.eigen.1.trendtest <- notrend_test(coint.all.eigen.1[[1]])
#coint.all.eigen.2.trendtest <- notrend_test(coint.all.eigen.2[[1]])
#coint.all.eigen.3.trendtest <- notrend_test(coint.all.eigen.3[[1]])
#coint.all.eigen.4.trendtest <- notrend_test(coint.all.eigen.4[[1]])

coint.all.eigen.1.trendtest
coint.all.eigen.2.trendtest
coint.all.eigen.3.trendtest
coint.all.eigen.4.trendtest

#Testing for drift

#Performing ADF test with no trend nor drift. with eigen
summary(ur.df(coint.all.eigen.1[[1]],type="none"))
summary(ur.df(coint.all.eigen.2[[1]],type="none"))
summary(ur.df(coint.all.eigen.1[[1]],type="trend"))
summary(ur.df(coint.all.eigen.2[[1]],type="trend"))

#Doing it for the residuals with eigen
coint.all.eigen.res.1 <- make.comb(all.crypto,lin.comb.numb=1,res=T,type.jo="eigen",ecdet="trend",title.gg="First Linear Combination")
coint.all.eigen.res.2 <- make.comb(all.crypto,lin.comb.numb=2,res=T,type.jo="eigen",ecdet="trend",title.gg="Second Linear Combination")

#testing the res for trend
coint.all.eigen.res.1.trendtest <- notrend_test(coint.all.eigen.res.1[[1]])
coint.all.eigen.res.2.trendtest <- notrend_test(coint.all.eigen.res.2[[1]])

coint.all.eigen.res.1.trendtest
coint.all.eigen.res.2.trendtest

#Performing the DF tests
summary(ur.df(coint.all.eigen.res.1[[1]],type="none"))
summary(ur.df(coint.all.eigen.res.2[[1]],type="none"))

#Creating qq-plots. with eigen

#Making the lists.
y1 <- list(BNB,BTC)
y2 <- list(BNB,ETH)
y3 <- list(BNB,LTC)
y4 <- list(BTC,ETH)
y5 <- list(BTC,LTC)
y6 <- list(ETH,LTC)

#Testing for coint with eigen
y1.coint.eigen <- make.comb(y1,1,res=F,type.jo="eigen",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1,1}^T\\mathbf{y}_{1,p}$'),title.gg.size=22)
summary(y1.coint.eigen[[2]])
y2.coint.eigen <- make.comb(y2,1,res=F,type.jo="eigen",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1,1}^T\\mathbf{y}_{2,p}$'))
summary(y2.coint.eigen[[2]])
y3.coint.eigen <- make.comb(y3,1,res=F,type.jo="eigen",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1,1}^T\\mathbf{y}_{3,p}$'))
summary(y3.coint.eigen[[2]])
y4.coint.eigen <- make.comb(y4,1,res=F,type.jo="eigen",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1,1}^T\\mathbf{y}_{4,p}$'),title.gg.size=22)
summary(y4.coint.eigen[[2]])
y5.coint.eigen <- make.comb(y5,1,res=F,type.jo="eigen",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1,1}^T\\mathbf{y}_{5,p}$'),title.gg.size=22)
summary(y5.coint.eigen[[2]])
y6.coint.eigen <- make.comb(y6,1,res=F,type.jo="eigen",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1,1}^T\\mathbf{y}_{6,p}$'))
summary(y6.coint.eigen[[2]])


#Plot the three important ones in the same plot:
p1 <- y1.coint.eigen[[3]]
p2 <- y4.coint.eigen[[3]]
p3 <- y5.coint.eigen[[3]]

ggarrange(p1,p2,p3,ncol=3,nrow=1)

#Testing for trend with eigen
#y1.coint.eigen.trend <- notrend_test(y1.coint.eigen[[1]])
#y2.coint.eigen.trend <- notrend_test(y2.coint.eigen[[1]])
#y3.coint.eigen.trend <- notrend_test(y3.coint.eigen[[1]])
#y4.coint.eigen.trend <- notrend_test(y4.coint.eigen[[1]])
#y5.coint.eigen.trend <- notrend_test(y5.coint.eigen[[1]])
#y6.coint.eigen.trend <- notrend_test(y6.coint.eigen[[1]])

#Testing for stationarity with eigen
y1.coint.eigen.df <- summary(ur.df(y1.coint.eigen[[1]])); y1.coint.eigen.df 
y2.coint.eigen.df <- summary(ur.df(y2.coint.eigen[[1]])); y2.coint.eigen.df 
y3.coint.eigen.df <- summary(ur.df(y3.coint.eigen[[1]])); y3.coint.eigen.df 
y4.coint.eigen.df <- summary(ur.df(y4.coint.eigen[[1]])); y4.coint.eigen.df 
y5.coint.eigen.df <- summary(ur.df(y5.coint.eigen[[1]])); y5.coint.eigen.df 
y6.coint.eigen.df <- summary(ur.df(y6.coint.eigen[[1]])); y6.coint.eigen.df

#Create qq plotfor y1,y5,y6.
qqplotz(x,johansen,title.gg)
y1.coint.eigen.qq.1 <- qqplotz(x=1,johansen=y1.coint.eigen[[2]],title.gg=TeX('$\\mathbf{y}_{1,p}$, Binance Coin'));y1.coint.eigen.qq.1
y1.coint.eigen.qq.2 <- qqplotz(x=2,johansen=y1.coint.eigen[[2]],title.gg=TeX('$\\mathbf{y}_{1,p}$, Bitcoin'));y1.coint.eigen.qq.2

y4.coint.eigen.qq.1 <- qqplotz(x=1,johansen=y4.coint.eigen[[2]],title.gg=TeX('$\\mathbf{y}_{4,p}$, Bitcoin'));y4.coint.eigen.qq.1
y4.coint.eigen.qq.2 <- qqplotz(x=2,johansen=y4.coint.eigen[[2]],title.gg=TeX('$\\mathbf{y}_{4,p}$, Etherium'));y4.coint.eigen.qq.2

y5.coint.eigen.qq.1 <- qqplotz(x=1,johansen=y5.coint.eigen[[2]],title.gg=TeX('$\\mathbf{y}_{5,p}$, Bitcoin'));y5.coint.eigen.qq.1
y5.coint.eigen.qq.2 <- qqplotz(x=2,johansen=y5.coint.eigen[[2]],title.gg=TeX('$\\mathbf{y}_{5,p}$, Litecoin'));y5.coint.eigen.qq.2

ggarrange(y1.coint.eigen.qq.1,y4.coint.eigen.qq.1,y5.coint.eigen.qq.1,y1.coint.eigen.qq.2,y4.coint.eigen.qq.2,y5.coint.eigen.qq.2, ncol=3,nrow=2)

#Testing for coint, individually with trace
y1.coint.trace <- make.comb(y1,1,res=F,type.jo="trace")
summary(y1.coint.trace[[2]])
y2.coint.trace <- make.comb(y2,1,res=F,type.jo="trace")
summary(y2.coint.trace[[2]])
y3.coint.trace <- make.comb(y3,1,res=F,type.jo="trace")
summary(y3.coint.trace[[2]])
y4.coint.trace <- make.comb(y4,1,res=F,type.jo="trace")
summary(y4.coint.trace[[2]])
y5.coint.trace <- make.comb(y5,1,res=F,type.jo="trace")
summary(y5.coint.trace[[2]])
y6.coint.trace <- make.comb(y6,1,res=F,type.jo="trace")
summary(y6.coint.trace[[2]])





#Now we wish to do the same, but with ecdet=const
#Using the function on all 4 crypto currencies. Doing it for 1st and 2nd linear combination. with eigen
all.crypto <- list(BNB,BTC,ETH,LTC)
coint.all.eigen.const.1 <- make.comb(all.crypto,lin.comb.numb=1,res=F,type.jo="eigen",ecdet="const")
coint.all.eigen.const.2 <- make.comb(all.crypto,lin.comb.numb=2,res=F,type.jo="eigen",ecdet="const")
summary(coint.all.eigen.1[[2]])
#Doing it for the residuals with eigen
coint.all.eigen.const.res.1 <- make.comb(all.crypto,lin.comb.numb=1,res=T,type.jo="eigen",ecdet="const")
coint.all.eigen.const.res.2 <- make.comb(all.crypto,lin.comb.numb=2,res=T,type.jo="eigen",ecdet="const")
library(plm)
get.linearly.independent(coint.all.eigen.const.1[[2]]@V)
detect.lindep(coint.all.eigen.const.1[[2]]@V)
summary(coint.all.eigen.const.1[[2]])

#Making the lists.
y1 <- list(BNB,BTC)
y2 <- list(BNB,ETH)
y3 <- list(BNB,LTC)
y4 <- list(BTC,ETH)
y5 <- list(BTC,LTC)
y6 <- list(ETH,LTC)

#Testing for coint with eigen
y1.coint.const.eigen <- make.comb(y1,1,res=F,type.jo="eigen",ecdet="const")
summary(y1.coint.eigen[[2]])
y2.coint.const.eigen <- make.comb(y2,1,res=F,type.jo="eigen",ecdet="const")
summary(y2.coint.eigen[[2]])
y3.coint.const.eigen <- make.comb(y3,1,res=F,type.jo="eigen",ecdet="const")
summary(y3.coint.eigen[[2]])
y4.coint.const.eigen <- make.comb(y4,1,res=F,type.jo="eigen",ecdet="const")
summary(y4.coint.eigen[[2]])
y5.coint.const.eigen <- make.comb(y5,1,res=F,type.jo="eigen",ecdet="const",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1}^T\\mathbf{y}_{5-p}$'))
summary(y5.coint.eigen[[2]])
y6.coint.const.eigen <- make.comb(y6,1,res=F,type.jo="eigen",ecdet="const",title.gg = TeX('$\\hat{\\mathbf{\\beta}}_{1}^T\\mathbf{y}_{6-p}$'))
summary(y6.coint.eigen[[2]])

#Testing for trend with eigen
y1.coint.eigen.trend <- notrend_test(y1.coint.eigen[[1]])
y2.coint.eigen.trend <- notrend_test(y2.coint.eigen[[1]])
y3.coint.eigen.trend <- notrend_test(y3.coint.eigen[[1]])
y4.coint.eigen.trend <- notrend_test(y4.coint.eigen[[1]])
y5.coint.eigen.trend <- notrend_test(y5.coint.eigen[[1]])
y6.coint.eigen.trend <- notrend_test(y6.coint.eigen[[1]])

#Testing for stationarity with eigen
y1.coint.eigen.df <- summary(ur.df(y1.coint.eigen[[1]])); y1.coint.eigen.df 
y2.coint.eigen.df <- summary(ur.df(y2.coint.eigen[[1]])); y2.coint.eigen.df 
y3.coint.eigen.df <- summary(ur.df(y3.coint.eigen[[1]])); y3.coint.eigen.df 
y4.coint.eigen.df <- summary(ur.df(y4.coint.eigen[[1]])); y4.coint.eigen.df 
y5.coint.eigen.df <- summary(ur.df(y5.coint.eigen[[1]])); y5.coint.eigen.df 
y6.coint.eigen.df <- summary(ur.df(y6.coint.eigen[[1]])); y6.coint.eigen.df

#Testing for coint, individually with trace
y1.coint.trace <- make.comb(y1,1,res=F,type.jo="trace")
summary(y1.coint.trace[[2]])
y2.coint.trace <- make.comb(y2,1,res=F,type.jo="trace")
summary(y2.coint.trace[[2]])
y3.coint.trace <- make.comb(y3,1,res=F,type.jo="trace")
summary(y3.coint.trace[[2]])
y4.coint.trace <- make.comb(y4,1,res=F,type.jo="trace")
summary(y4.coint.trace[[2]])
y5.coint.trace <- make.comb(y5,1,res=F,type.jo="trace")
summary(y5.coint.trace[[2]])
y6.coint.trace <- make.comb(y6,1,res=F,type.jo="trace")
summary(y6.coint.trace[[2]])









#Er ikke sikker på om alt dette er nødvendigt.
#Testing for cointegration with trace
#First for all coin
coint.all.trace.1 <- make.comb(all.crypto,lin.comb.numb=1,res=F,type.jo="trace",ecdet="trend")
coint.all.trace.2 <- make.comb(all.crypto,lin.comb.numb=2,res=F,type.jo="trace",ecdet="trend")

#Doing it for the residuals with trace
coint.all.trace.res.1 <- make.comb(all.crypto,lin.comb.numb=1,res=T,type.jo="trace",ecdet="trend")
coint.all.trace.res.2 <- make.comb(all.crypto,lin.comb.numb=2,res=T,type.jo="trace",ecdet="trend")

#Testing for trend(Careful about the notrend. takes a long time) with trace
coint.all.trace.1.trendtest <- notrend_test(coint.all.trace.1[[1]])
coint.all.trace.2.trendtest <- notrend_test(coint.all.trace.2[[1]])
coint.all.trace.res.1.trendtest <- notrend_test(coint.all.trace.res.1[[1]])
coint.all.trace.res.2.trendtest <- notrend_test(coint.all.trace.res.2[[1]])

coint.all.trace.1.trendtest
coint.all.trace.2.trendtest
coint.all.trace.res.1.trendtest
coint.all.trace.res.2.trendtest

#Testing for drift with trace

#Performing ADF test with no trend nor drift. with trace
summary(ur.df(coint.all.trace.1[[1]],type="none"))
summary(ur.df(coint.all.trace.2[[1]],type="none"))
summary(ur.df(coint.all.trace.res.1[[1]],type="none"))
summary(ur.df(coint.all.trace.res.2[[1]],type="none"))

#Creating qq-plots. with trace

#Testing for coint, individually with trace
y1.coint.trace <- make.comb(y1,1,res=F,type.jo="trace")
summary(y1.coint.trace[[2]])
y2.coint.trace <- make.comb(y2,1,res=F,type.jo="trace")
summary(y2.coint.trace[[2]])
y3.coint.trace <- make.comb(y3,1,res=F,type.jo="trace")
summary(y3.coint.trace[[2]])
y4.coint.trace <- make.comb(y4,1,res=F,type.jo="trace")
summary(y4.coint.trace[[2]])
y5.coint.trace <- make.comb(y5,1,res=F,type.jo="trace")
summary(y5.coint.trace[[2]])
y6.coint.trace <- make.comb(y6,1,res=F,type.jo="trace")
summary(y6.coint.trace[[2]])

#Testing for trend with trace
y1.coint.trace.trend <- notrend_test(y1.coint.trace[[1]])
y2.coint.trace.trend <- notrend_test(y2.coint.trace[[1]])
y3.coint.trace.trend <- notrend_test(y3.coint.trace[[1]])
y4.coint.trace.trend <- notrend_test(y4.coint.trace[[1]])
y5.coint.trace.trend <- notrend_test(y5.coint.trace[[1]])
y6.coint.trace.trend <- notrend_test(y6.coint.trace[[1]])

#Testing for drift with trace

#Testing for stationarity with trace
y1.coint.trace.df <- summary(ur.df(y1.coint.trace[[1]])); y1.coint.trace.df 
y2.coint.trace.df <- summary(ur.df(y2.coint.trace[[1]])); y2.coint.trace.df 
y3.coint.trace.df <- summary(ur.df(y3.coint.trace[[1]])); y3.coint.trace.df 
y4.coint.trace.df <- summary(ur.df(y4.coint.trace[[1]])); y4.coint.trace.df 
y5.coint.trace.df <- summary(ur.df(y5.coint.trace[[1]])); y5.coint.trace.df 
y6.coint.trace.df <- summary(ur.df(y6.coint.trace[[1]])); y6.coint.trace.df



#3.75*5.2
