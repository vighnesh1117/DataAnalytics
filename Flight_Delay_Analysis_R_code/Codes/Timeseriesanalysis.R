
##loading data from csv file into R
flight = read.csv("C:/Users/vsawa/Desktop/FinalData.csv",header = TRUE)


##loading data into variables
SCHEDULED_DEPARTURE=flight$SCHEDULED_DEPARTURE
carrier=flight$carrier
DEPARTURE_TIME=flight$DEPARTURE_TIME
dest=flight$dest
distance=flight$distance
DATE =flight$DATE
flightnumb=flight$flightnumb
origin=flight$origin
weather=flight$weather
dayweek=flight$dayweek
daymonth=flight$daymonth
tailnu=flight$tailnu
delay=flight$delay
delaytimeweathergood=flight$delaytimeweathergood
delaytimeweatherbad=flight$delaytimeweatherbad
DEPARTURE_DELAY=flight$DEPARTURE_DELAY
SCHEDULED_TIME=flight$SCHEDULED_TIME
ELAPSED_TIME=flight$ELAPSED_TIME
AIR_TIME=flight$AIR_TIME
SCHEDULED_ARRIVAL=flight$SCHEDULED_ARRIVAL
ARRIVAL_TIME=flight$ARRIVAL_TIME
ARRIVAL_DELAY=flight$ARRIVAL_DELAY
delaytimeweatherbad=flight$delaytimeweatherbad
delaytimeweathergood=flight$delaytimeweathergood
YEAR=flight$YEAR
MONTH=flight$MONTH
DAY=flight$DAY

##selecting the required data
date = flight[,25]
arrivaltime = flight[,18]


#importing library
library(zoo)

##creating time series object
ratets= zoo(x=arrivaltime, as.Date(as.character(flight$DATE), format = "%m/%d/%Y"))

#importing libraries
library(tseries)
library(fBasics)

##calculating statistics
basicStats(arrivaltime)


##plotting histogram
par(mfcol=c(2,2))
hist(arrivaltime, xlab="ARRIVAL_TIME", prob=TRUE, main="Histogram")

xfit<-seq(min(arrivaltime),max(arrivaltime),length=40)
yfit<-dnorm(xfit,mean=mean(arrivaltime),sd=sd(arrivaltime))
lines(xfit, yfit, col="red", lwd=2)

#6 CREATE NORMAL PROBABILITY PLOT
qqnorm(arrivaltime)
qqline(arrivaltime, col = 2)

#7 CREATE TIME PLOT
# use time series object lnattsto draw time plot indexed with time
plot(ratets, type='l', xlab='time', ylab='ARRIVAL_TIME')

#8 COMPUTE ACF and PACF AND PLOT CORRELOGRAM
#prints acfto console
acf(arrivaltime, plot=F, lag=20)

# creates 2 by 1 display for 2 plots
par(mfcol=c(2,1))
#plots acf(correlogram)
acf(arrivaltime, plot=T, lag=20)
# plots pacfvalues up to lag 15.
pacf(arrivaltime, lag = 15)

#10 COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)
# to Lag 6
Box.test(arrivaltime,lag=6,type='Ljung')
# to Lag 12
Box.test(arrivaltime,lag=12,type='Ljung')


#Fit a AR (2) Model
m1= arima(arrivaltime, order=c(2,0,0), method='ML', include.mean=T)
m1


#fit a MA(1)
m3= arima(arrivaltime, order=c(0,0,1), method='ML', include.mean=T)
m3

##residualanalysis: 
##JB test
jarque.bera.test(m1$residual)

##White noise or not by Ljung box test
Box.test(m1$residuals,lag=6,type='Ljung')
Box.test(m1$residuals,lag=12,type='Ljung')

#ARMA model, choosing p and q values 

#solution1: plotting ACf and PACF
#p = 2 and q  = 1

##building ARMA model
m7= arima(arrivaltime, order=c(2,0,1), method='ML', include.mean=T)
m7

#residual analysis:
jarque.bera.test(m7$residual)
Box.test(m7$residuals,lag=6,type='Ljung')

#solution2 : automatically by EACF.R

source("E:/Data Analytics/EACF.R")
EACF(arrivaltime)

#p = 2, q = 2

#building ARMA model
m8= arima(arrivaltime, order=c(2,0,2), method='ML', include.mean=T)
m8


#residual analysis:
#JB test
jarque.bera.test(m8$residual)
#ljung box test
Box.test(m8$residuals,lag=6,type='Ljung')
Box.test(m8$residuals,lag=12,type='Ljung')
Box.test(m8$residuals,lag=15,type='Ljung')

library(forecast)
#Solution3: by minimizing AIC/BIC
m9 = auto.arima(arrivaltime, max.P=8, max.Q=8, ic="aic")
m9

##ARIMA model

auto.arima(coredata(ratets))


##predictions:
##best model is from solution 3 ie ARMA(1,1,1) ie m9 having aic = 28794.65
##to predict/forecast the future arrivsl time
pr = predict(m9, n.ahead = 10, se.fit=T)
pr

