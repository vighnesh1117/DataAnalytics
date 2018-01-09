library(BSDA)
#First we will read the data and store it in variable flight
flight=read.csv("D:/FlightData_Linear_Hypothesis.csv")

SCHEDULED_DEPARTURE=flight$SCHEDULED_DEPARTURE
carrier=flight$carrier
DEPARTURE_TIME=flight$DEPARTURE_TIME
dest=flight$dest
distance=flight$distance
date=flight$date
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

#Performing hypothesis Testing On delaytimeweathergood and delaytime weatherbad

z.test(delaytimeweathergood,delaytimeweatherbad,alternative="greater",mu=0,sigma.x=sd(delaytimeweathergood),sigma.y=sd(delaytimeweatherbad),conf.level=0.95)

