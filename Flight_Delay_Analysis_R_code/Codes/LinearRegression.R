#First we will read the data and store it in variable flight.

flight<-read_csv('D:/FlightData_Linear_Hypothesis.csv')
# Now we will divide the dataset into Training(80%) and Testing Data(20%)
flight=flight[sample(nrow(flight)),]
select.data = sample (1:nrow(flight), 0.8*nrow(flight))
train.data = flight[select.data,]
test.data = flight[-select.data,]

#Loading the variables into R
SCHEDULED_DEPARTURE=train.data$SCHEDULED_DEPARTURE
carrier=train.data$carrier
DEPARTURE_TIME=train.data$DEPARTURE_TIME
dest=train.data$dest
distance=train.data$distance
flightnumb=train.data$flightnumb
origin=train.data$origin
weather=train.data$weather
dayweek=train.data$dayweek
daymonth=train.data$daymonth
tailnu=train.data$tailnu
delay=train.data$delay
delaytimeweathergood=train.data$delaytimeweathergood
delaytimeweatherbad=train.data$delaytimeweatherbad
DEPARTURE_DELAY=train.data$DEPARTURE_DELAY
SCHEDULED_TIME=train.data$SCHEDULED_TIME
ELAPSED_TIME=train.data$ELAPSED_TIME
AIR_TIME=train.data$AIR_TIME
SCHEDULED_ARRIVAL=train.data$SCHEDULED_ARRIVAL
ARRIVAL_TIME=train.data$ARRIVAL_TIME
ARRIVAL_DELAY=train.data$ARRIVAL_DELAY
delaytimeweatherbad=train.data$delaytimeweatherbad
delaytimeweathergood=train.data$delaytimeweathergood
YEAR=train.data$YEAR
MONTH=train.data$MONTH
DAY=train.data$DAY
date=train.data$date


#Correlation  Graph
data.flight=flight[,c("ARRIVAL_DELAY","SCHEDULED_DEPARTURE","DEPARTURE_TIME","distance","weather","DEPARTURE_DELAY","SCHEDULED_TIME","ELAPSED_TIME","AIR_TIME","SCHEDULED_ARRIVAL","ARRIVAL_TIME")]
plot(data.flight)

# Now we will Build Full model

full=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+DEPARTURE_TIME+distance+weather+DEPARTURE_DELAY+SCHEDULED_TIME+ELAPSED_TIME+AIR_TIME+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(full)

# USing Approach : Backward Elimination By P-value
m1=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+DEPARTURE_TIME+distance+DEPARTURE_DELAY+SCHEDULED_TIME+ELAPSED_TIME+AIR_TIME+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(m1)

m2=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+distance+DEPARTURE_DELAY+SCHEDULED_TIME+ELAPSED_TIME+AIR_TIME+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(m2)

m3=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+distance+DEPARTURE_DELAY+SCHEDULED_TIME+ELAPSED_TIME+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(m3)

m4=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+distance+DEPARTURE_DELAY+SCHEDULED_TIME+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(m4)

m5=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+distance+DEPARTURE_DELAY+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(m5)

m6=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+DEPARTURE_DELAY+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(m6)
# m6 is the final model we have got using elimination by p-value approach



# Now we will use appproach Backward Elimination by AIC 
step(full, direction="backward", trace=F)

aicmodel=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+DEPARTURE_DELAY+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(aicmodel)


# Now we will use appproach Stepwise Regression

base=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE)
step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F)

stepwiseRegressionModel=lm(ARRIVAL_DELAY ~ SCHEDULED_DEPARTURE + DEPARTURE_DELAY +SCHEDULED_ARRIVAL + ARRIVAL_TIME)
summary(stepwiseRegressionModel)

#Now We will calculate accuracy of each model by using testing data

y1=predict.glm(m6,test.data)
y=test.data[,22]
rmse_1=sqrt((y-y1)%*%(y-y1))/nrow(test.data)
rmse_1
#Accuracy of Backward Elimination By P-value 


y2=predict.glm(aicmodel,test.data)
y=test.data[,22]
rmse_2=sqrt((y-y2)%*%(y-y2))/nrow(test.data)
rmse_2

#Accuracy of Backward Elimination by AIC

y3=predict.glm(stepwiseRegressionModel,test.data)
y=test.data[,22]
rmse_3=sqrt((y-y3)%*%(y-y3))/nrow(test.data)
rmse_3

#Accuracy of Stepwise regression

#We can choose any model as the RMSE value is same for all three approaches:
#We will build Scatter Plots for each independent variable.

m6=lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE+DEPARTURE_DELAY+SCHEDULED_ARRIVAL+ARRIVAL_TIME)
summary(m6)

plot(DEPARTURE_DELAY, ARRIVAL_DELAY, cex=1,pch=16,col="red", xlab="Departure delay of flights(minutes)", ylab="Arrival delay of flights", main="Arrival delay vs. Departure delay")
lm.one<-lm(ARRIVAL_DELAY~DEPARTURE_DELAY)
abline(lm.one$coef, lwd=2)

plot(SCHEDULED_DEPARTURE, ARRIVAL_DELAY, cex=1,pch=16,col="red", xlab="Scheduled departure of flights(minutes)", ylab="Arrival delay of flights", main="Arrival delay vs. Scheduled departure")
lm.one<-lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE)
abline(lm.one$coef, lwd=2)

plot(SCHEDULED_ARRIVAL, ARRIVAL_DELAY, cex=1,pch=16,col="red", xlab="Scheduled arrival of flights(minutes)", ylab="Arrival delay of flights", main="Arrival delay vs. Scheduled arrival")
lm.one<-lm(ARRIVAL_DELAY~SCHEDULED_ARRIVAL)
abline(lm.one$coef, lwd=2)


plot(ARRIVAL_TIME, ARRIVAL_DELAY, cex=1,pch=16,col="red", xlab="Arrival Time of flights(minutes)", ylab="Arrival delay of flights", main="Arrival delay vs. Arrival Time")
lm.one<-lm(ARRIVAL_DELAY~ARRIVAL_TIME)
abline(lm.one$coef, lwd=2)



# Residual Analysis
resid<-resid(m6)
st.resid<-rstandard(m6)
par(mfrow=c(1,2))
plot(resid, pch=16, col="red", main="Residuals")
abline(0,0)
plot(st.resid, pch=16, col="blue", main="Standardized Residuals")
abline(0,0)

# Histogram

par(mfrow=c(1,1))
hist(st.resid, breaks = 50, main="Distribution of std. residuals")

#Boxplot
boxplot(st.resid, main="Boxplot of std. residuals")

# Normality Check
qqnorm(st.resid, ylab="Std. residuals", xlab="Normal scores", main="QQplot of std. residuals")
qqline(st.resid, col="red")
