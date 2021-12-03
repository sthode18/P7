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
tsplot <- function(z, gg.title){
    q <- ggplot(BNB, aes(x=date, y=close)) + geom_line() + xlab("Date") + ylab("USD") + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle(gg.title)
    return(q)
}

tsbnb <- tsplot(1, "Binance Coin")
tsbtc <- tsplot(2, "Bitcoin")
tseth <- tsplot(3, "Etherium")
tsltc <- tsplot(4, "Litecoin")

ggarrange(tsbnb, tsbtc, tseth, tsltc, ncol = 2, nrow = 2)
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
                  ecdet="trend", spec="longrun")
summary(johansen)

#Q-Q plots
qqplotz <- function(x, gg.title){
  df_stdrs <- data.frame("stdrs" = johansen@R0[,x])
    q <- ggplot(df_stdrs, aes(sample = stdrs)) + stat_qq() + stat_qq_line() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle(gg.title)
    return(q)
}

ggarrange(qqplotz(1, "Binance Coin"), qqplotz(2, "Bitcoin"), 
          qqplotz(3, "Etherium"), qqplotz(4, "Litecoin"), ncol = 2, nrow = 2)

#ACF
whalecum <- function(z, title.gg){
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
    acfdick <- ggplot(data = df_bacf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0))   +
      geom_hline(aes(yintercept = lim), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = -lim), linetype = 2, color = 'blue') +
      labs(x = "Lag", y = "ACF") + ggtitle(title.gg)
    return(acfdick)
}

whalecum(1, "Binance Coin")

ggarrange(whalecum(1, "Binance Coin"), whalecum(2, "Bitcoin"), 
          whalecum(3, "Etherium"), whalecum(4, "Litecoin"), ncol = 2, nrow = 2)

pvalue <- NULL
for(i in 1:30){
  pvalue[i] <- Box.test(johansen@R0[,1],lag=i,type=c("Ljung-Box"))$p.value
}
plot(pvalue,xlim=c(0,30),ylim=c(0,1),xlab="lag",ylab="p-value");abline(h=0.05,lty=2,col="blue")

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

plot_residuals <- function(z, title.gg){
  fuck <- johansen@R0[,z]
    newdf_BNB_n <- data.frame(fuck, BNB[3:11000,]$date)
    return(ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle(title.gg))
}

ggarrange(plot_residuals(1), plot_residuals(2), 
          plot_residuals(3), plot_residuals(4), ncol = 2, nrow = 2)

#1=BNB, 2=BTC, 3=ETH, 4=LTC

plot_density <- function(z, title.gg){
  fuck <- johansen@R0[,z]
    df <- data.frame(Price = fuck, Date = BNB[3:11000,]$date)
    return(ggplot(df, aes(x=Price)) + geom_density() + geom_vline(aes(xintercept=mean(Price)), 
                                                           color="blue", linetype="dashed", size=1) + ggtitle(title.gg))
}

ggarrange(plot_density(1, "Binance Residual Density"), plot_density(2, "Bitcoin Residual Density"), 
          plot_density(3, "Etherium Residual Density"), plot_density(4, "Litecoin Residual Density"), 
          ncol = 2, nrow = 2)


#Linear combinations wrt. cointegration vector
finalplots <- function(z, ecdet, title.gg){
  if (ecdet == "no"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z])
  }
  if (ecdet == "yes"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z] + johansen@ZK[,5]*johansen@V[5,z])
  }
  newdf_BNB <- data.frame(wtf, BNB[3:11000,]$date)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("Date") + ylab("USD") + geom_line() + ggtitle(title.gg)
  
  return(list(p,summary(ur.df(wtf, type = "trend"))))
}

finalplots_dt <- function(z, ecdet, title.gg){
  if (ecdet == "no"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z])
  }
  if (ecdet == "yes"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z] + johansen@ZK[,5]*johansen@V[5,z])
  }
  wtf_dt <- detrend(wtf)
  newdf_BNB_n <- data.frame(wtf_dt, BNB[3:11000,]$date)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("Date") + ylab("USD") + geom_line() + ggtitle(title.gg)
  return(list(p,summary(ur.df(wtf))))
}

fp1 <- finalplots(1, ecdet = "yes", "First Linear Combination")
fp2 <- finalplots(2, ecdet = "yes", "Second Linear Combination")
fp3 <- finalplots(3, ecdet = "yes", "Third Linear Combination")
fp4 <- finalplots(4, ecdet = "yes", "Fourth Linear Combination")
fp1[[1]]
ggarrange(fp1[[1]], fp2[[1]], ncol = 2, nrow = 1)

ggarrange(fp1[[1]], fp2[[1]], fp3[[1]], fp4[[1]], ncol = 2, nrow = 2)

fpdt1 <- finalplots_dt(1, ecdet = "yes", "Detrended First Linear Combination")
fpdt2 <- finalplots_dt(2, ecdet = "yes", "Detrended Second Linear Combination")
fpdt3 <- finalplots_dt(3, ecdet = "yes", "Detrended Third Linear Combination")
fpdt4 <- finalplots_dt(4, ecdet = "yes", "Detrended Fourth Linear Combination")

ggarrange(fpdt1[[1]], fpdt2[[1]], fpdt3[[1]], fpdt4[[1]], ncol = 2, nrow = 2)

#Final plots with residuals instead of the entire process
finalplots_res <- function(z, ecdet, title.gg){
  if (ecdet == "no"){
    wtf <- johansen@RK[,1]*johansen@V[1,z] + (johansen@RK[,2]*johansen@V[2,z] + johansen@RK[,3]*johansen@V[3,z] + johansen@RK[,4]*johansen@V[4,z])
  }
  if (ecdet == "yes"){
    wtf <- johansen@RK[,1]*johansen@V[1,z] + (johansen@RK[,2]*johansen@V[2,z] + johansen@RK[,3]*johansen@V[3,z] + johansen@RK[,4]*johansen@V[4,z] + johansen@RK[,5]*johansen@V[5,z])
  }
  newdf_BNB <- data.frame(wtf, BNB[3:11000,]$date)
    p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf)) + xlab("") + ylab("") + geom_line() + ggtitle(title.gg)
  return(list(p,summary(ur.df(wtf, type = "trend"))))
}

finalplots_res_dt <- function(z, ecdet, title.gg){
  if (ecdet == "no"){
    wtf <- johansen@RK[,1]*johansen@V[1,z] + (johansen@RK[,2]*johansen@V[2,z] + johansen@RK[,3]*johansen@V[3,z] + johansen@RK[,4]*johansen@V[4,z])
  }
  if (ecdet == "yes"){
    wtf <- johansen@RK[,1]*johansen@V[1,z] + (johansen@RK[,2]*johansen@V[2,z] + johansen@RK[,3]*johansen@V[3,z] + johansen@RK[,4]*johansen@V[4,z] + johansen@RK[,5]*johansen@V[5,z])
  }
  wtf_dt <- detrend(wtf)
  newdf_BNB_n <- data.frame(wtf_dt, BNB[3:11000,]$date)
    p <- ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=wtf_dt)) + xlab("") + ylab("") + geom_line() + ggtitle("First linear combination")
  return(list(p,summary(ur.df(wtf, type = "trend"))))
}

finalplots_res(1, ecdet="no", "First Linear Combination")

finalplots_res_dt(1, ecdet="yes", "Second Linear Combination")















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
)