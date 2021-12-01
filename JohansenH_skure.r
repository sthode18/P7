#Load packages
pacman::p_load(readr, tseries, urca, ggplot2, dplyr, lubridate, forecast,
               pracma, vars, stats, tsDyn, ggpubr, ggplotify)
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
    #Use scale 3.75 x 5.2
tsplot <- function(z){
    ggplot(z, aes(x=date, y=close)) + geom_line() + xlab("Date") + ylab("USD") + theme(axis.text.x=element_text(angle=60, hjust=1))
}

tsbnb <- tsplot(BNB)
tsbtc <- tsplot(BTC)
tseth <- tsplot(ETH)
tsltc <- tsplot(LTC)

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

#Create a trend variable
trend <- seq_along(BNB$close)

#Select value of p
all_coins <- data.frame(BNB$close, BTC$close, ETH$close, LTC$close)
VARselect(all_coins)

#Johansen cointegration method
johansen <- ca.jo(all_coins, type="eigen", K=2, 
                  ecdet="none", spec="longrun")
summary(johansen)

#Q-Q plots
qqplotz <- function(x){
  df_stdrs <- data.frame("stdrs" = johansen@R0[,x])
  ggplot(df_stdrs, aes(sample = stdrs)) + stat_qq() + stat_qq_line() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Litecoin")
}
qqplotzz <- qqplotz(1)
qqplotzzz <- qqplotz(2)
qqplotzzzz <- qqplotz(3)
qqplotzzzzz <- qqplotz(4)

ggarrange(qqplotzz, qqplotzzz, qqplotzzzz, qqplotzzzzz, ncol = 2, nrow = 2)

#ACF
whalecum <- function(z){
bacf <- acf(johansen@R0[,z], plot = FALSE)
df_bacf <- with(bacf, data.frame(lag, acf))  

ic_alpha= function(alpha, acf_res){
  return(qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n))
}
lim <- ic_alpha(0.01,bacf)

for (i in 1:length(df_bacf$acf)){
  if (df_bacf$acf[i] > 0.19){
    df_bacf$acf[i] <- 0.19
  }
}
  
  if (z == 1){
    acfdick <- ggplot(data = df_bacf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0))   +
      geom_hline(aes(yintercept = lim), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = -lim), linetype = 2, color = 'blue') +
      labs(x = "Lag", y = "ACF") + ggtitle("Binance Coin")
    return(acfdick)
  }
if (z == 2){
  acfdick <- ggplot(data = df_bacf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))   +
    geom_hline(aes(yintercept = lim), linetype = 2, color = 'blue') +
    geom_hline(aes(yintercept = -lim), linetype = 2, color = 'blue') +
    labs(x = "Lag", y = "ACF") + ggtitle("Bitcoin")
  return(acfdick)
}
if (z == 3){
  acfdick <- ggplot(data = df_bacf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))   +
    geom_hline(aes(yintercept = lim), linetype = 2, color = 'blue') +
    geom_hline(aes(yintercept = -lim), linetype = 2, color = 'blue') +
    labs(x = "Lag", y = "ACF") + ggtitle("Etherium")
  return(acfdick)
}
if (z == 4){
  acfdick <- ggplot(data = df_bacf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))   +
    geom_hline(aes(yintercept = lim), linetype = 2, color = 'blue') +
    geom_hline(aes(yintercept = -lim), linetype = 2, color = 'blue') +
    labs(x = "Lag", y = "ACF") + ggtitle("Litecoin")
  return(acfdick)
}
}
whalecum(1)

ggarrange(whalecum(1), whalecum(2), whalecum(3), whalecum(4), ncol = 2, nrow = 2)

pvalue <- NULL
for(i in 1:30){
  pvalue[i] <- Box.test(johansen@R0[,1],lag=i,type=c("Ljung-Box"))$p.value
}
plot(pvalue,xlim=c(0,30),ylim=c(0,1),xlab="lag",ylab="p-value");abline(h=0.05,lty=2,col="blue")

#Adf test to find order of integration
summary(ur.df(BNB[1:9000, ]$close))
summary(ur.df(BTC[1:9000, ]$close))
summary(ur.df(ETH[1:9000, ]$close))
summary(ur.df(LTC[1:9000, ]$close))

#Check for trend maybe? R^2 adjusted

#Johansen pairwise BNB and BTC
VARselect(data.frame(BNB$close, BTC$close))
j_BNB_BTC <- ca.jo(data.frame(BNB$close, BTC$close), type ="trace", K = 2,
                   ecdet = "trend", spec = "longrun")
summary(j_BNB_BTC)
#Johansen pairwise BNB and ETH
VARselect(data.frame(BNB$close, ETH$close))
j_BNB_ETH <- ca.jo(data.frame(BNB$close, ETH$close), type = "trace", K = 3,
                   ecdet = "trend", spec = "longrun")
summary(j_BNB_ETH)
#Johansen pairwise BNB and LTC
VARselect(data.frame(BNB$close, LTC$close))
j_BNB_LTC <- ca.jo(data.frame(BNB$close, LTC$close), type = "trace", K = 3,
                   ecdet = "trend", spec = "longrun")
summary(j_BNB_LTC)
#Johansen pairwise BTC and ETH
VARselect(data.frame(BTC$close, ETH$close))
j_ETH_BTC <- ca.jo(data.frame(ETH$close, BTC$close), type = "trace", K = 3,
                   ecdet = "trend", spec = "longrun")
summary(j_ETH_BTC)
#Johansen pairwise BTC and LTC
VARselect(data.frame(LTC$close, BTC$close))
j_LTC_BTC <- ca.jo(data.frame(LTC$close, BTC$close), type = "trace", K = 3,
                   ecdet = "trend", spec = "longrun")
summary(j_LTC_BTC)
#Johansen pairwise ETH and LTC
VARselect(data.frame(ETH$close, LTC$close))
j_ETH_LTC <- ca.jo(data.frame(ETH$close, LTC$close), type = "trace", K = 3,
                   ecdet = "trend", spec = "longrun")
summary(j_ETH_LTC)

#data.frame(johansen@ZK[,1]*johansen@V[1,1], johansen@ZK[,2]*johansen@V[2,1], 
#  johansen@ZK[,3]*johansen@V[3,1], johansen@ZK[,4]*johansen@V[4,1], 
#  johansen@ZK[,5]*johansen@V[5,1])

plot_residuals <- function(z){
  fuck <- johansen@R0[,z]
  if (z == 1){
    newdf_BNB_n <- data.frame(fuck, BNB[3:11000,]$date)
    print(ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Binance residuals"))
  }
  if (z == 2){
    newdf_BNB_n <- data.frame(fuck, BTC[3:11000,]$date)
    print(ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Bitcoin residuals"))
  }
  if (z == 3){
    newdf_BNB_n <- data.frame(fuck, ETH[3:11000,]$date)
    print(ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Etherium residuals"))
  }
  if (z == 4){
    newdf_BNB_n <- data.frame(fuck, LTC[3:11000,]$date)
    print(ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle("Litecoin residuals"))
  }
}

#1=BNB, 2=BTC, 3=ETH, 4=LTC
plot_residuals(1)

plot_qq <- function(z){
qqnorm(johansen@R0[,z], pch = 1, frame = FALSE) 
qqline(johansen@R0[,z], col = "steelblue", lwd = 2)
}

#1=BNB, 2=BTC, 3=ETH, 4=LTC
plot_qq(1)

plot(density(fuck, width = 100))
#Try some shit, så vi ikke behøver ctrl c + ctrl v hvert tal
#Aka linear combinations wrt. cointegration vector
finalplots <- function(z, ecdet){
  if (ecdet == "none"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z])
  }
  if (ecdet == "trend"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z] + johansen@ZK[,5]*johansen@V[5,z])
  }
  newdf_BNB <- data.frame(wtf, BNB[3:11000,]$date)
  if (z == 1)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("First linear combination")
  if (z == 2)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("Second linear combination")
  if (z == 3)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("Third linear combination")
  if (z == 4)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("Fourth linear combination")
  return(list(p,summary(ur.df(wtf))))
}

finalplots_dt <- function(z, ecdet){
  if (ecdet == "none"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z])
  }
  if (ecdet == "trend"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z] + johansen@ZK[,5]*johansen@V[5,z])
  }
  wtf_dt <- detrend(wtf)
  newdf_BNB_n <- data.frame(wtf_dt, BNB[3:11000,]$date)
  if (z == 1)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("First linear combination")
  if (z == 2)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("Second linear combination")
  if (z == 3)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("Third linear combination")
  if (z == 4)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("Fourth linear combination")
  return(list(p,summary(ur.df(wtf))))
}

#1=BNB, 2=BTC, 3=ETH, 4=LTC
finalplots(1, ecdet = "trend")

finalplots_dt(1, ecdet = "trend")

#Final plots with residuals instead of the entire process
finalplots_res <- function(z, ecdet){
  if (ecdet == "none"){
    wtf <- johansen@R0[,1]*johansen@V[1,z] + (johansen@R0[,2]*johansen@V[2,z] + johansen@R0[,3]*johansen@V[3,z] + johansen@R0[,4]*johansen@V[4,z])
  }
  if (ecdet == "trend"){
    wtf <- johansen@R0[,1]*johansen@V[1,z] + (johansen@R0[,2]*johansen@V[2,z] + johansen@R0[,3]*johansen@V[3,z] + johansen@R0[,4]*johansen@V[4,z] + johansen@R0[,5]*johansen@V[5,z])
  }
  newdf_BNB <- data.frame(wtf, BNB[3:11000,]$date)
  if (z == 1)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("First linear combination")
  if (z == 2)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("Second linear combination")
  if (z == 3)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("Third linear combination")
  if (z == 4)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle("Fourth linear combination")
  return(list(p,summary(ur.df(wtf))))
}

finalplots_res_dt <- function(z, ecdet){
  if (ecdet == "none"){
    wtf <- johansen@RK[,1]*johansen@V[1,z] + (johansen@RK[,2]*johansen@V[2,z] + johansen@RK[,3]*johansen@V[3,z] + johansen@RK[,4]*johansen@V[4,z])
  }
  if (ecdet == "trend"){
    wtf <- johansen@RK[,1]*johansen@V[1,z] + (johansen@RK[,2]*johansen@V[2,z] + johansen@RK[,3]*johansen@V[3,z] + johansen@RK[,4]*johansen@V[4,z] + johansen@RK[,5]*johansen@V[5,z])
  }
  wtf_dt <- detrend(wtf)
  newdf_BNB_n <- data.frame(wtf_dt, BNB[3:11000,]$date)
  if (z == 1)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("First linear combination")
  if (z == 2)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("Second linear combination")
  if (z == 3)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("Third linear combination")
  if (z == 4)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("Fourth linear combination")
  return(list(p,summary(ur.df(wtf))))
}

finalplots_res(2, ecdet="none")

finalplots_res_dt(1, ecdet="trend")














#Construct VECM to determine strategy??
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
Coint_BNB_n <- johansen@ZK[,1] + johansen@V[2, 1]*johansen@ZK[,2] + johansen@V[3, 1]*johansen@ZK[,3] + johansen@V[4, 1]*johansen@ZK[,4] + johansen@V[5,1]*johansen@ZK[,5]
newdf_BNB_n <- data.frame(Coint_BNB_n, BNB[3:11000,]$date)
ggplot(newdf_BNB_n, aes(x=BNB.3.11000....date, y=Coint_BNB_n)) + geom_line() + ggtitle("Binance Coin-ish")
summary(ur.df(Coint_BNB_n))

detrended_coint_BNB_n <- detrend(Coint_BNB_n)
newdf_BNB_n <- data.frame(detrended_coint_BNB_n, BNB$date)[9001:11000, ]
ggplot(newdf_BNB_n, aes(x=BNB.date, y=detrended_coint_BNB_n)) + geom_line() + ggtitle("Detrended Binance Coin-ish")

summary(ur.df(detrended_coint_BNB_n))

#First linear combination with trend
Coint_BNB <- (BNB$close) - (johansen@V[2, 2]*(BTC$close) + johansen@V[3, 2]*(ETH$close) + johansen@V[4, 2]*(LTC$close) + johansen@V[5, 2]*trend)
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

