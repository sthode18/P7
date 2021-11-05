library(ggplot2)
library(urca)
library(latex2exp)

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
    print(i)
  }
  return(DF.100)
}

dick_intercept <- function(nobs,rep){
  counter <- 0
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
    if(round((i/rep)*100,digits=1)-0.1>counter){
      counter <- round((i/rep)*100,digits=1)
      print(round((i/rep)*100,digits=1))
    }
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
TeX(r'($DF_t$)')
df <- as.data.frame(dick(1000,100000))
df2 <- as.data.frame(dick_intercept(1000,100000))
df3 <- as.data.frame(dick_time_trend(1000,100000))

#This is the graph!!
bw <- 55
p <- ggplot() + geom_freqpoly(data=df,aes(df[,1],colour='red'), bins=bw) +
  geom_freqpoly(data=df2,aes(df2[,1], colour='green'), bins=bw) + 
  geom_freqpoly(data=df3,aes(df3[,1], colour='blue'), bins=bw) +
  xlim(-0.6, 0.2)+ labs(colour = "Time Series")+
  theme(legend.position = c(0.32, 0.75),axis.title.x=element_blank(),legend.text = element_text(size=15))+ 
  labs(  y="Count", x = "Sepal length (cm)") +
  scale_colour_manual(values = c('red' = 'red','blue' = 'blue','green'='green'),name = '', 
                    labels = expression(DF[t],DF[t]^mu,DF[t]^tau)); p

pdf(file = "/Users/sorenthode/Dropbox/Mac/Desktop/P7/Projekt/Rstuff/DFgraph.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 4)
file.choose()
dev.off()

 
