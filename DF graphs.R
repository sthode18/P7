library(ggplot2)
library(urca)

#To do an OLS fit yourself
dick <- function(nobs,rep){
  DF.100 <- rep(NA, rep)
  for(i in 1:rep){
    yt <- cumsum(rnorm(nobs))
    numerator <- 0
    denominator <- 0
    for(j in 2:nobs){
      numerator <- numerator + yt[j-1]*(yt[j]-yt[j-1])
      denominator <- denominator + yt[j-1]^2
    }
    beta <- numerator/denominator
    ssr <- 0
    for(j in 2:nobs){
      ssr <- ssr + (yt[j]- yt[j-1]*beta)^2
    }
    se <- sqrt(ssr / (nobs-3)) / sqrt(sum((yt - mean(yt))^2)) 
    DF.100[i] <- beta / se
  }
  return(DF.100)
}

dick_intercept <- function(nobs,rep){
  DF.100 <- rep(NA, rep)
  for(i in 1:rep){
    yt <- cumsum(rnorm(nobs))
    numerator <- 0
    denominator <- 0
    xt <- yt[-length(yt)]
    yt_mean <- mean(yt)
    xt_mean <- mean(xt)
    for(j in 2:nobs){
      numerator <- numerator + (yt[j-1]-yt_mean)*(yt[j]-yt[j-1])
      denominator <- denominator + (yt[j-1]-yt_mean)^2
    }
    beta <- numerator/denominator
    ssr <- 0
    for(j in 2:nobs){
      ssr <- ssr + (yt[j]- yt[j-1]*beta)^2
    }
    se <- sqrt(ssr / (nobs-3)) / sqrt(sum((yt - mean(yt))^2)) 
    DF.100[i] <- beta / se
  }
  return(DF.100)
}

dick_time_trend <- function(nobs,rep){
  DF.100 <- rep(NA, rep)
  for(i in 1:rep){
    yt <- cumsum(rnorm(nobs))
    numerator <- 0
    denominator <- 0
    yt_mean <- mean(yt)
    trModel <- lm(yt ~ c(1:length(yt)))
    #k_t <- detrend(detrend(yt, 'constant'),'linear')
    for(j in 2:nobs){
      numerator <- numerator + (yt[j-1]-trModel$coefficients[2]*j-trModel$coefficients[1])*(yt[j]-yt[j-1])
      denominator <- denominator + (yt[j-1]-trModel$coefficients[2]*j-trModel$coefficients[1])^2
    }
    beta <- numerator/denominator
    ssr <- 0
    for(j in 2:nobs){
      ssr <- ssr + (yt[j]- yt[j-1]*beta)^2
    }
    se <- sqrt(ssr / (nobs-3)) / sqrt(sum((yt - mean(yt))^2)) 
    DF.100[i] <- beta / se
    print(i)
  }
  return(DF.100)
}

df <- as.data.frame(dick(1000,100000))
df2 <- as.data.frame(dick_intercept(1000,100000))
df3 <- as.data.frame(dick_time_trend(1000,100000))


#This is the graph!!
p <- ggplot() + geom_freqpoly(data=df,aes(df[,1],colour="No Intercept"), bins=bw) +
  geom_freqpoly(data=df2,aes(df2[,1], colour="With Intercept"), bins=bw) + 
  geom_freqpoly(data=df3,aes(df3[,1], colour="With Time Trend"), bins=bw) +
  xlim(-0.6, 0.2); p



#Everything down here is just practise!
nrow(df)
bw <- 70
#Histogram plot
ggplot(data=df, aes(df[,1])) + 
  geom_histogram(bins=bw) +
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(df[,1]), sd = sd(df[,1])) * (n*0.7))

ggplot(data=df3, aes(df3[,1])) + 
  geom_histogram(bins=bw) +
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(df3[,1]), sd = sd(df3[,1])) * (n*0.7))

df
df[,1]
graph_1 <-  ggplot(data=df, aes(df[,1]))+
  geom_freqpoly(bins=bw); graph_1
graph_2 <-  ggplot(data=df2, aes(df2[,1]))+
  geom_freqpoly(bins=bw); graph_2
graph_3 <-  ggplot(data=df3, aes(df3[,1]))+
  geom_freqpoly(bins=bw); graph_3

graph_done <- ggplot(data=df_super, aes(x=df_super[,1],y=df_super[,2],z=df_super[,3]))+
  geom_freqpoly(bins=bw); graph_done


n = 1000
t = 100
No.Ex = 10
steps = seq(0,t,length=n+1)
A = replicate(No.Ex, {
  bm <- c(0, cumsum(rnorm(n,0,sqrt(t/n))))
}) 
A
bm
t <- 0:100  # time
sig2 <- 0.01
## first, simulate a set of random deviates
x <- rnorm(n = length(t) - 1, sd = sqrt(1))
## now compute their cumulative sum
x <- c(0, cumsum(x))

y <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## now compute their cumulative sum
y <- c(0, cumsum(y))

z <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## now compute their cumulative sum
z <- c(0, cumsum(z))



t <- 1:9
x <- c(0, 2, 0, 4, 4, 4, 0, 2, 0)
x - detrend(x, 'constant')
x - detrend(x, 'linear')
yt <- cumsum(rnorm(nobs))
y <- detrend(yt, 'linear', 5)
y2 <- detrend(yt, 'constant')
y3 <- detrend(detrend(yt, 'constant'),'linear')
plot(y)
plot(yt)
plot(y2)
plot(y3)
y <- detrend(x, 'linear')
## Not run: 
 plot(t, y, col="blue")
 lines(t, y - y, col="red")
 grid()## End(Not run)

 