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
#Plotting Bitcoin and Litecoin
df1 <- data.frame(BTC$close, BTC$date)[10000:11000,]
df2 <- data.frame(LTC$close, LTC$date)[10000:11000,]

ggplot() + 
  geom_line(data = df1, aes(x = BTC.date, y = BTC.close/309,colour = "red")) +
  geom_line(data = df2, aes(x = LTC.date, y = LTC.close,colour = "blue")) +
  xlab('Date') +
  ylab('Percent Change') + 
  theme(legend.position = c(0.12,0.8)) + 
  scale_colour_manual(values = c("red", "blue"), name = "Currencies",
                                                     labels = c("Bitcoin", "Litecoin"))
#Plotting all the processes
#Use scale 3.75 x 5.2
tsplot <- function(z, gg.title){
  df <- data.frame(z$close,z$date)[5061:11000,]
  q <- ggplot(df, aes(x=z.date, y=z.close)) + geom_line() + xlab("Date") + ylab("USD") + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle(gg.title)
  return(q)
}

tsbnb <- tsplot(z = BNB, "Binance Coin")
tsbtc <- tsplot(z = BTC, "Bitcoin")
tseth <- tsplot(z = ETH, "Ethereum")
tsltc <- tsplot(z = LTC, "Litecoin")

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
auto.arima(johansen@V[1,1]*BNB$close, max.q=0)
auto.arima(johansen@V[1,2]*BTC$close, max.q=0)
auto.arima(johansen@V[1,3]*ETH$close, max.q=0)
auto.arima(johansen@V[1,4]*LTC$close, max.q=0)

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
          qqplotz(3, "Ethereum"), qqplotz(4, "Litecoin"), ncol = 2, nrow = 2)

#ACF
plot_acf <- function(z, title.gg){
  bac <- data.frame(johansen@R0[,z])
  bacf <- acf(bac, plot = FALSE)
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

plot_acf(1, "Binance Coin")

ggarrange(plot_acf(1, "Binance Coin"), plot_acf(2, "Bitcoin"), 
          plot_acf(3, "Ethereum"), plot_acf(4, "Litecoin"), ncol = 2, nrow = 2)

pvalue <- NULL
for(i in 1:30){
  pvalue[i] <- Box.test(johansen@R0[,1],lag=i,type=c("Ljung-Box"))$p.value
}
plot(pvalue,xlim=c(0,30),ylim=c(0,1),xlab="lag",ylab="p-value");abline(h=0.05,lty=2,col="blue")

#Johansen pairwise BNB and BTC
VARselect(data.frame(BTC$close, BNB$close))
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
VARselect(data.frame(BTC$close, LTC$close))
j_LTC_BTC <- ca.jo(data.frame(BTC$close, LTC$close), type = "trace", K = 3,
                   ecdet = "trend", spec = "longrun")
summary(j_LTC_BTC)
#Johansen pairwise ETH and LTC
VARselect(data.frame(ETH$close, LTC$close))
j_ETH_LTC <- ca.jo(data.frame(ETH$close, LTC$close), type = "trace", K = 3,
                   ecdet = "trend", spec = "longrun")
summary(j_ETH_LTC)

#Plotting residuals and their density
plot_residuals <- function(z, title.gg){
  fuck <- johansen@R0[,z]
  newdf_BNB_n <- data.frame(fuck, BNB[5061:11000,]$date)
  return(ggplot(newdf_BNB_n, aes(x=newdf_BNB_n[,2], y=fuck)) + geom_line() + xlab("") + ylab("") + ggtitle(title.gg))
}

ggarrange(plot_residuals(1, "Binance Coin Residuals"), plot_residuals(2, "Bitcoin Residuals"), 
          plot_residuals(3, "Ethereum Residuals"), plot_residuals(4, "Litecoin Residuals"), ncol = 2, nrow = 2)

#1=BNB, 2=BTC, 3=ETH, 4=LTC

plot_density <- function(z, title.gg){
  fuck <- johansen@R0[,z]
  df <- data.frame(Price = fuck, Date = BNB[3:11000,]$date)
  return(ggplot(df, aes(x=Price)) + geom_density() + geom_vline(aes(xintercept=mean(Price)), 
                                                                color="blue", linetype="dashed", size=1) + ggtitle(title.gg))
}

ggarrange(plot_density(1, "Binance Residual Density"), plot_density(2, "Bitcoin Residual Density"), 
          plot_density(3, "Ethereum Residual Density"), plot_density(4, "Litecoin Residual Density"), 
          ncol = 2, nrow = 2)


#Linear combinations wrt. cointegration vector
finalplots <- function(z, ecdet, title.gg, typer = "trend"){
  if (ecdet == "no"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z])
  }
  if (ecdet == "yes"){
    wtf <- johansen@ZK[,1]*johansen@V[1,z] + (johansen@ZK[,2]*johansen@V[2,z] + johansen@ZK[,3]*johansen@V[3,z] + johansen@ZK[,4]*johansen@V[4,z] + johansen@ZK[,5]*johansen@V[5,z])
  }
  newdf_BNB <- data.frame(wtf[5059:10998], BNB[5061:11000,]$date)
  p <- ggplot(newdf_BNB, aes(x=newdf_BNB[,2], y=wtf[5059:10998])) + xlab("Date") + ylab("USD") + geom_line() + ggtitle(title.gg)
  
  return(list(p,summary(ur.df(wtf[5059:10998], type = typer))))
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

fp1 <- finalplots(1, ecdet = "yes", "First Linear Combination", type = "none")
fp2 <- finalplots(2, ecdet = "yes", "Second Linear Combination", typer = "none")
fp3 <- finalplots(3, ecdet = "yes", "Third Linear Combination", typer = "none")
fp4 <- finalplots(4, ecdet = "yes", "Fourth Linear Combination", typer = "none")
fp4[[2]]
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

#IDKIDKDIDKDIDK
all_coins <- data.frame(BTC$close, BNB$close, ETH$close, LTC$close)
VARselect(all_coins)

#Johansen cointegration method
johansen2 <- ca.jo(all_coins, type="eigen", K=2, 
                  ecdet="trend", spec="longrun")
summary(johansen2)

#IDK
johansen.r1 <- cajorls(johansen2, r = 1)

summary(johansen.r1$rlm)

alpha <- coef(johansen.r1$rlm)[1, ]  # the coefficients on ecm1
beta <- johansen.r1$beta  # the point estimates of beta

resids <- resid(johansen.r1$rlm)
#Don't think this works for more than 2 relationships being tested
johansen.r2$rlm <- lm(formula = BTC.close.d ~ ect1 + constant + BTC.close.dl1 + 
     BNB.close.dl1 + ETH.close.dl1 + LTC.close.dl1 - 1, data = all_coins)
btc.d <- diff(BTC$close)[-1]
bnb.d <- diff(BNB$close)[-1]
eth.d <- diff(ETH$close)[-1]
ltc.d <- diff(LTC$close)[-1]
btc.d1 <- diff(BNB$close)[-length(BNB$close) - 1]
bnb.d1 <- diff(BNB$close)[-length(BNB$close) - 1]
eth.d1 <- diff(ETH$close)[-length(ETH$close) - 1]
ltc.d1 <- diff(LTC$close)[-length(LTC$close) - 1]
error.ecm.1 <- resids[-1:-2]
ecm.btc <- lm(btc.d ~ error.ecm.1 + btc.d1 + bnb.d1 + eth.d1 + ltc.d1)

N <- nrow(resids) 
sigma <- crossprod(resids) / N 
##t-stats for alpha
alpha.se <- sqrt(solve(crossprod(cbind(johansen2@ZK %*% beta, 
                                       johansen2@Z1)))[1, 1] * diag(sigma))
alpha.t <- alpha/alpha.se
rho <- 1/(1-(34.4838876 + 10.560605 + 183.982707 - 3.826786))
rho*beta
## t-stats for beta 
beta.se <- sqrt(diag(kronecker(solve(crossprod(johansen2@RK[, -1])), 
                               solve(t(alpha) %*% solve(sigma) %*% alpha)))) 
beta.t <- c(NA, beta[-1] / beta.se) 
names(beta.t) <- rownames(johansen.r1$beta) 

## Print alpha and beta + t-statistics
print(rbind(alpha, alpha.t))
print(t(cbind(beta, beta.t)))

?cajorls


plot.ts(cajorls(j_BNB_BTC, r = 1)$rlm$residuals)



#Construct VECM to determine strategy??
trend <- seq_along(BNB$close)
est_tsdyn <- VECM(all_coins, 2, r = 1, include = "none", estim = "ML", exogen = trend)
summary(est_tsdyn)

#Impulse response analysis
var <- vec2var(johansen, r = 1)
#Irf for BNB
ir <- irf(var, n.ahead = 20, impulse = "BNB.close", response = "BTC.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "BNB.close", response = "ETH.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "BNB.close", response = "LTC.close", ortho = FALSE, runs = 500)
#Irf for BTC
ir1 <- irf(var, n.ahead = 20, impulse = "BTC.close", response = "BNB.close", ortho = FALSE, runs = 1000)
ir2 <- irf(var, n.ahead = 20, impulse = "BTC.close", response = "ETH.close", ortho = FALSE, runs = 1000)
ir3 <- irf(var, n.ahead = 20, impulse = "BTC.close", response = "LTC.close", ortho = FALSE, runs = 1000)
#Irf for ETH
ir <- irf(var, n.ahead = 20, impulse = "ETH.close", response = "BNB.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "ETH.close", response = "BTC.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "ETH.close", response = "LTC.close", ortho = FALSE, runs = 50)
#Irf for LTC
ir <- irf(var, n.ahead = 20, impulse = "LTC.close", response = "BNB.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "LTC.close", response = "BTC.close", ortho = FALSE, runs = 50)
ir <- irf(var, n.ahead = 20, impulse = "LTC.close", response = "ETH.close", ortho = FALSE, runs = 50)

plot(ir1)
