#DELAY GRAPHS
library(tidyr)
library(dplyr )
library(ggplot2) # flight visualization
library(readr) # CSV file I/O, e.g. the read_csv function
flight<-read_csv('D:/FlightData_Linear_Hypothesis.csv')
summary(flight)
SCHEDULED_DEPARTURE=flight$SCHEDULED_DEPARTURE
carrier=flight$carrier
DEPARTURE_TIME=flight$DEPARTURE_TIME
dest=flight$dest
distance=flight$distance
date=flight$date

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

flight$WeekDay <- flight$dayweek

flight$MonthQual <- flight$daymonth


flight$WeekDay[flight$WeekDay == 1] = 'Monday'
flight$WeekDay[flight$WeekDay == 2] = 'Tuesday'
flight$WeekDay[flight$WeekDay == 3] = 'Wednesday'
flight$WeekDay[flight$WeekDay == 4] = 'Thursday'
flight$WeekDay[flight$WeekDay == 5] = 'Friday'
flight$WeekDay[flight$WeekDay == 6] = 'Saturday'
flight$WeekDay[flight$WeekDay == 7] = 'Sunday'

flight$MonthQual[flight$MonthQual == 1] = 'January'
flight$MonthQual[flight$MonthQual == 2] = 'February'
flight$MonthQual[flight$MonthQual == 3] = 'March'
flight$MonthQual[flight$MonthQual == 4] = 'April'
flight$MonthQual[flight$MonthQual == 5] = 'May'
flight$MonthQual[flight$MonthQual == 6] = 'June'
flight$MonthQual[flight$MonthQual == 7] = 'July'
flight$MonthQual[flight$MonthQual == 8] = 'August'
flight$MonthQual[flight$MonthQual == 9] = 'September'
flight$MonthQual[flight$MonthQual == 10] = 'October'
flight$MonthQual[flight$MonthQual == 11] = 'November'
flight$MonthQual[flight$MonthQual == 12] = 'December'

flight %>% group_by(WeekDay) %>% 
  tally %>% arrange(desc(n))

#The most frequent flying day is on friday, which isn't surprising as it's also one of the 
#most common days to fly.

flight %>% group_by(MonthQual) %>% 
  tally %>% arrange(desc(n))

flight$delay=recode(flight$delay,"'delayed'=1;else=0")
flight$delay=as.numeric(levels(flight$delay)[flight$delay])
table(flight$delay)


flight$delay[flight$delay == 0] = 'No'
flight$delay[flight$delay == 1] = 'Yes'

qplot(factor(delay), flight=flight, geom="bar", fill=factor(delay))

flight %>% group_by(delay) %>%
  tally %>% arrange(desc(n))

Flights.flight  = read.table('D:/FlightData_Linear_Hypothesis.csv', sep =",", header = TRUE)
flights.delay.sub.new <- Flights.flight[,c('YEAR','MONTH','DAY','dayweek','carrier','origin','dest','DEPARTURE_DELAY','ARRIVAL_DELAY')]
head(flights.delay.sub.new)

flights.delay.sub <- Flights.flight[,c('YEAR','MONTH','DAY','dayweek','carrier','origin','dest','SCHEDULED_DEPARTURE','SCHEDULED_ARRIVAL','DEPARTURE_DELAY','ARRIVAL_DELAY')]
flights.delay.sub <- flights.delay.sub[complete.cases(flights.delay.sub), ]
flights.delay.sub.orig.dca.dest.jfk <- flights.delay.sub[ which(flights.delay.sub$origin=='DCA' & flights.delay.sub$dest=='JFK'), ]

flights.delay.sub.new <- flights.delay.sub.new[complete.cases(flights.delay.sub.new), ]
flights.delay.sub.orig.dca.dest.jfk <- flights.delay.sub.new[ which(flights.delay.sub.new$origin=='DCA' & flights.delay.sub.new$dest=='JFK'), ]
numcols <- c('DEPARTURE_DELAY','ARRIVAL_DELAY')

#Generating DATE column derived from YEAR, MONTH and DAY

flights.delay.sub.orig.dca.dest.jfk$date <- as.Date(paste(flights.delay.sub.orig.dca.dest.jfk$YEAR, flights.delay.sub.orig.dca.dest.jfk$MONTH,flights.delay.sub.orig.dca.dest.jfk$DAY , sep = "."),  format = "%Y.%m.%d")

head(flights.delay.sub.orig.dca.dest.jfk)
summary(flights.delay.sub.orig.dca.dest.jfk)
str(flights.delay.sub.orig.dca.dest.jfk)

#Lets look into the density of Arrival Delays
library(ggplot2)
ggplot(flights.delay.sub.orig.dca.dest.jfk, aes(ARRIVAL_DELAY)) + geom_density() + xlab('Arrival Delay in minutes') + ylab('Density of Arrival Delay') + ggtitle('Density charr arrival delay by Airline for 2015 flights from DCA to JFK')

#Above density chart shows me that most of the flights either reach before time or on-time. #But, there are delays. Now lets check which 
#airlines are delayed the most.

options(repr.plot.width=12, repr.plot.height=8)
ggplot(flights.delay.sub.orig.dca.dest.jfk, aes(x = factor(carrier), y = ARRIVAL_DELAY)) + geom_boxplot() + ylab('Arrival Delay in minutes') + xlab('Airline Abbreviations') + ggtitle('Arrival delay by Airline for 2015 flights from DCA to JFK')

#Relationship between departure delay and arrival delay with airline/carrier shown:
ggplot(flights.delay.sub.orig.dca.dest.jfk, aes(DEPARTURE_DELAY, ARRIVAL_DELAY)) + geom_point(aes(color = factor(carrier),size  = ARRIVAL_DELAY, alpha = 0.5)) + xlab('Departure delay in minutes') + ylab('Arrival delay in minutes') + ggtitle('Relationship between Departure delay and Arrival delay with Carrier shown')

#Scatter plot above provides view of density as well as distribution of departure versus #arrival delay, proving the hypothesis that most 
#of the arrival delays occurs due departure #delay when flying from DCA to JFK based on flight from 2015.

ggplot(flights.delay.sub.orig.dca.dest.jfk, aes(dayweek, ARRIVAL_DELAY)) + geom_point(aes(color = factor(carrier), shape = factor(carrier),size  = ARRIVAL_DELAY, alpha = 0.5)) + xlab('Day of week in numbers (MON=1...SUN=7)') + ylab('Arrival delay in minutes') + ggtitle('Relationship between Departure delay and Arrival delay with carrier shown')

#Based on above plot, least amount of delay's happen on Tuesday's, Wednesday's, thursday's and #Saturday's while there is a changes flight 
#being delayed on Monday's, Friday's and Sunday's #if you fly from DCA to JFK based on 2015 Flights flight.
#Conclusion of Exploratory Analysis

#1. Departure delay has significant impact on Arrival delay.

#2. DH and DL are least reliable and MQ airlines is most reliable in terms of arrival delay.