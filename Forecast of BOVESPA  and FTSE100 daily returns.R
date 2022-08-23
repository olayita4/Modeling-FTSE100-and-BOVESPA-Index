#Packages
require(tidyquant)
require(ggplot2)
require(broom)
library(tidyquant)
library(timetk)
library(fGarch)
library(astsa)

#Load data (BOVESPA Index and FTSE100 Index)
tickers = c('^FTSE', '^BVSP')
getSymbols(tickers,
           from = "2017-06-10",
           to = "2022-06-10")

close_prices = merge.xts(Cl(FTSE), Cl(BVSP))

#rename to simplify references
stock_names = c("FTSE100", "BOVESPA")
names(close_prices) = stock_names
index(close_prices) = as.Date(index(close_prices))

close_prices1 = na.omit(close_prices)

close_priceB = close_prices1[,c(2)]
close_priceF = close_prices1[,c(1)]

close_pricesB <- tidy(close_priceB)
close_pricesF = tidy(close_priceF)

colnames(close_pricesB)[1]="date"
colnames(close_pricesF)[1]="date


# log transform BOVESPA Index
bovespa_log1=log(close_pricesB$value)

#take the difference
bovespa_log = diff(bovespa_log1)

# plot the data
ts.plot(bovespa_log)

# ACF and PACF
acf(bovespa_log)
pacf(bovespa_log)
acf(bovespa_log^2) 
pacf(bovespa_log^2, lag=50)

#Fitting an AR(1) since AR components were observed
sarima(bovespa_log, 1,0,0, details=TRUE)$fit
#Checking its residuals
resi=resid(sarima(bovespa_log, 1,0,0, details=FALSE)$fit)
acf(resi^2) # squared residuals are highly correlated
pacf(resi^2)

# Modeling the mean and variance jointly
# Estimating both AR(1) and Garch(1,1) models simultaneously-first candidate
m1 <- garchFit(~arma(1,0)+ garch(1,1), data=bovespa_log, cond.dist = 'std')
summary(m1)
plot(m1, which=3)

# Estimating both MA(1) and Garch(1,1) models simultaneously - second candidate
m2 <- garchFit(~arma(0,1)+ garch(1,1), data=bovespa_log, cond.dist = 'std')
summary(m2)
plot(m2,which=3)

# Checking residuals and squared residuals of the models
resi=residuals(m1,standardize=T)
acf(resi,lag=20)
Box.test(resi,lag=20,type='Ljung') 
acf(resi^2,lag=20)
Box.test(resi^2,lag=20,type='Ljung')

res=residuals(m2,standardize=T)
acf(res,lag=20)
Box.test(res,lag=20,type='Ljung') 
acf(res^2,lag=20)
pacf(res^2,lag=20) 
Box.test(res^2,lag=20,type='Ljung')

# Although both models had similar AIC and BIC, the first candidate was selected for further analysis since unlike the second candidate, its squared residuals were uncorrelated at the 95% level.

# plot of the volatility and standardized residuals
v=volatility(m1)
vol=ts(v,frequency=365,start = c(2017,6))
volu=data.frame(vol)
res=ts(resi,frequency=365,start = c(2017,6))
dev.new()
par(mfcol=c(2,1))
plot(vol,xlab='year',ylab='volatility',type='l')
plot(res,xlab='year',ylab='standardized residuals',type='l')

#30-Day Forecast of the mean and variance 
predict(m1,5)
pred <- predict(m1, n.ahead = 30, trace = FALSE, plot = TRUE)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# log transform FTSE100 Index
FTSE100_log1=log(close_pricesF$value)

##take the difference
FTSE100_log= diff(FTSE100_log1)

#plot the data
ts.plot(FTSE100_log)

#ACF and PACF
acf(FTSE100_log)
pacf(FTSE100_log, lag=50)

#Fitting an ARMA(1,1) model since some ARMA components were observed
#Checking its residuals and squared residuals
u = resid(sarima(FTSE100_log, 1,0,1, details=FALSE)$fit)
acf(u)
acf(u^2) 
pacf(u^2)

## Modeling the mean and variance jointly
# Estimating both ARMA(1,1) and Garch(1,1) models simultaneously - first candidate
m3 <- garchFit(~arma(1,1)+ garch(1,1), data=FTSE100_log, cond.dist = 'std')
summary(m3)
plot(m3, which=3)
# Estimating both ARMA(1,1) and ARCH(1) models simultaneously - second candidate
m4 <- garchFit(~arma(1,1)+ garch(1,0), data=FTSE100_log, cond.dist = 'std')
summary(m4)
plot(m4,which=3)

# Checking residuals and squared residuals of the models

resi=residuals(m3,standardize=T)
acf(resi,lag=20)
Box.test(resi,lag=20,type='Ljung')
acf(resi^2,lag=20)
pacf(resi^2,lag=20)
Box.test(resi^2,lag=20,type='Ljung')

# Since the second candidate has a bigger AIC and BIC than the first candidate, we disregarded this model and chose the first model for further analysis.

# plot of the volatility and standardized residuals
v=volatility(m3)
vol=ts(v,frequency=365,start = c(2017,6))
res=ts(resi,frequency=365,start = c(2017,6))
par(mfcol=c(2,1))
plot(vol,xlab='year',ylab='volatility',type='l')
plot(res,xlab='year',ylab='standardized residuals',type='l')

#30-Day Forecast of the mean and variance
predict(m3,5)
pred <- predict(m3, n.ahead = 30, trace = FALSE, plot = TRUE)






