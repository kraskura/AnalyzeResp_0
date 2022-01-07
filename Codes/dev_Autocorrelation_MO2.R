library(lmtest)
library(stats)
library(MARSS)
library(forecast)
library(datasets)

#  https://rpubs.com/JSHAH/481706
# https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-autoregressive-ar-models.html
# https://online.stat.psu.edu/stat501/lesson/14/14.1


## better ACF plot
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk),rr,type="h",lwd=2,yaxs="i",xaxs="i",
       ylim=c(floor(min(rr)),1),xlim=c(0,kk+1),
       xlab="Lag",ylab="Correlation",las=1)
  abline(h=-1/nn+c(-2,2)/sqrt(nn),lty="dashed",col="blue")
  abline(h=0)
}

## better PACF plot
plot.pacf <- function(PACFobj) {
  rr <- PACFobj$acf
  kk <- length(rr)
  nn <- PACFobj$n.used
  plot(seq(kk),rr,type="h",lwd=2,yaxs="i",xaxs="i",
       ylim=c(floor(min(rr)),1),xlim=c(0,kk+1),
       xlab="Lag",ylab="PACF",las=1)
  abline(h=-1/nn+c(-2,2)/sqrt(nn),lty="dashed",col="blue")
  abline(h=0)
}

setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/AUTOCORR/csv_input_files")

data<-read.csv("Jun22_2019_Opadiet_Box1_1.csv")
data<-read.csv("Jun23_2019_Opadiet_Box1_1.csv")
back<-read.csv("Jun22_2019_Opadiet_Box1_Back2.csv")
# data<-read.csv("jun23_2019_Opadiet_Box3_1.csv")

data<-data[1:280,]
back<-back[1:280,]

# turn the data into a time series data:
data.ts<-ts(data=data[, "Ch1_O2"], start=data[1, "Ch1_O2"], frequency=1)
back.ts<-ts(data=back[, "Ch1_O2"], start=back[1, "Ch1_O2"], frequency=1)

model.data<-lm(data$Ch1_O2 ~ data$time_sec, data=data)
model.back<-lm(back$Ch1_O2 ~ back$time_sec, data=back)

plot(data$time_sec,  data$Ch1_O2)
lines(data$time_sec, predict(model.data), col="red")
plot(back$time_sec,  back$Ch1_O2)
lines(back$time_sec, predict(model.back), col="red")


### Time Series --------- 

# Are the data stationary? 
# it must be for autcorrelation / autoregression 
# 1. is the mean constant across the time ? 
# 2. is the variance constant across the time?
# 3. the autocorrelation does not change over time 

# plot the timeseries
plot.ts(data.ts)
plot.ts(back.ts)

# decomposition of time series 
# 1. mt: the overall trends  
# 2. st: the 'seasonal trend'
# 3. et: random error (zero-mean, correlated over time)

# xt = mt + st + et 

# get teh mean trend; apply moving average filter function 
## weights for moving avg| used every 10 points? - guess; the filter weights are necessarry for the filter function
fltr <- c(1/2,rep(1,times=11),1/2)/12
## estimate of trend
data.trend <- stats::filter(data.ts, filter=fltr, method="convo", sides=2)
back.trend<- stats::filter(back.ts, filter=fltr, method="convo", sides=2)
## plot the trend
plot.ts(data.trend, ylab="Trend", cex=1)
plot.ts(back.trend, ylab="Trend", cex=1)

# would it rather makes sense to have two parameter model?
# xt = mt + et 
data.et <- data.ts - data.trend
back.et <- back.ts - back.trend

plot.ts(data.et, ylab="Trend", cex=1)
plot.ts(back.et, ylab="Trend", cex=1)

plot(cbind(data.ts, data.trend, data.et), main="", yax.flip=TRUE)
  
# [o2] data are violating # 1 - need data transformation. 

# plots to see if there is autocorrelation in the data
lag.plot(data$Ch1_O2)
lag.plot(back$Ch1_O2)

lag.plot(model.data$residuals, lags=1, do.lines=F, labels=F)
lag.plot(model.back$residuals, lags=1, do.lines=F, labels=F)

# the residuals from OLR vs the time. 
plot(data$time_sec, model.data$residuals)
plot(back$time_sec, model.back$residuals)

# Durbin -Watson test
dwtest(model.data) # value between 0-2 indicate negative error correlation 
dwtest(model.back) # value between 0-2 indicate negative error correlation 
# can reject the hypothesis that the tru autocorrelation is zero. 

# estimate rho
# is autoregressive function is appropriate?
# why residuals?: Large sample partial autocorrelations that are significantly different from 0 indicate lagged terms of ϵ that may be useful predictors of ϵt
rho<-acf(model.data$residuals, type="partial") # plot is autocorrelation plot, the dotted lines in which the autoregressive point is significantly unlikely to be "independent" of the previous points.
rho<-acf(model.back$residuals, type="partial") 

# PACF
# the PACF is the most useful in determining the orger of autoregressive model 
pacf.data<-acf(data$Ch1_O2, type="partial")
pacf.back<-acf(back$Ch1_O2, type="partial")
#  In a plot of PACF versus the lag, the pattern will usually appear random, but large PACF values at a given lag indicate this value as a possible choice for the order of an autoregressive model

# ACF
acf.data<-acf(data$Ch1_O2, type="correlation")
acf.back<-acf(back$Ch1_O2, type="correlation")
# In a plot of ACF versus the lag, if you see large ACF values and a non-random pattern, then likely the values are serially correlated

plot.acf(acf.data)
plot.acf(acf.back)

plot.pacf(pacf.data)
plot.pacf(pacf.back)


# RW - simulating random walk model ----------
## set random number seed
set.seed(123)
## length of time series
TT <- last(data$time_sec)
## initialize {x_t} and {w_t}
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)
## compute values 2 thru TT
for (t in 2:TT) {
    xx[t] <- xx[t - 1] + ww[t]
}

## setup plot area
par(mfrow = c(1, 2))
## plot line
plot.ts(xx, ylab = expression(italic(x[t])))
## plot ACF
plot.acf(acf(xx, plot = FALSE))
x2<-cumsum(ww)

## setup plot area
par(mfrow = c(1, 2))
## plot 1st RW
plot.ts(xx, ylab = expression(italic(x[t])))
## plot 2nd RW
plot.ts(x2, ylab = expression(italic(x[t])))


# ARIMA models/ simulations --------- 
# AR - "autoregressive"
# I - "integrated"
# MA - "moving average"

# arima.sim (model) argument has 4 components: 
# 1. order: vector (p,d,q)
# 2. ar: vector of length p (AR(p) coefficients)
# 3. ma: vector of length q (MA(q) coefficients) - can ommit if I have MA(q) model
# 4. sd: scalar indicating the sd of Gaussian errors - default norm distr. SD=1; can also pass the entire timeseries 


# simulating autoregressive model:
# Simulate an AR model with 0.5 slope
x <- arima.sim(model = list(ar = -0.9), n = 100)
plot(x)
# ACF help us determine what type of series we have, whether it is a White noise, Random walk, Auto regressive or Moving average.
## list description for AR(1) model with small AR coef, other pars same
AR.sm <- list(order = c(1, 0, 0), ar = 0.1, sd = 0.1)
## list description for AR(1) model with large AR coef
AR.lg <- list(order = c(1, 0, 0), ar = 0.9, sd = 0.1)
set.seed(123)
## list description for AR(1) model with postive AR coef
AR.pos <- list(order = c(1, 0, 0), ar = 0.5, sd = 0.1)
## list description for AR(1) model with negative AR coef
AR.neg <- list(order = c(1, 0, 0), ar = -0.5, sd = 0.1)

## simulate AR(1)
AR1.sm <- arima.sim(n = 50, model = AR.sm)
AR1.lg <- arima.sim(n = 50, model = AR.lg)
AR1.pos <- arima.sim(n = 50, model = AR.pos)
AR1.neg <- arima.sim(n = 50, model = AR.neg)


## setup plot region
par(mfrow = c(2, 2))
## get y-limits for common plots
ylm <- c(min(AR1.sm, AR1.lg), max(AR1.sm, AR1.lg))
## plot the ts
plot.ts(AR1.sm, ylim = ylm, ylab = expression(italic(x)[italic(t)]), 
    main = expression(paste(phi, " = 0.1")))
plot.ts(AR1.lg, ylim = ylm, ylab = expression(italic(x)[italic(t)]), 
    main = expression(paste(phi, " = 0.9")))
## get y-limits for common plots
ylm <- c(min(AR1.pos, AR1.neg), max(AR1.pos, AR1.neg))
## plot the ts
plot.ts(AR1.pos, ylim = ylm, ylab = expression(italic(x)[italic(t)]), 
    main = expression(paste(phi[1], " = 0.5")))
plot.ts(AR1.neg, ylab = expression(italic(x)[italic(t)]), main = expression(paste(phi[1], 
    " = -0.5")))

set.seed(123)
## the 4 AR coefficients
ARp <- c(0.7, 0.2, -0.3, -0.1)
## empty list for storing models
AR.mods <- list()

## loop over orders of p 
for (p in 1:4) {
    ## assume SD=1, so not specified
    AR.mods[[p]] <- arima.sim(n = 10000, list(ar = ARp[1:p]))
    print( list(ar = ARp[1:p]))
}

## set up plot region
par(mfrow = c(4, 3))
## loop over orders of p
for (p in 1:4) {
    plot.ts(AR.mods[[p]][1:50], ylab = paste("AR(", p, ")", sep = ""))
    acf(AR.mods[[p]], lag.max = 12)
    pacf(AR.mods[[p]], lag.max = 12, ylab = "PACF")
}
