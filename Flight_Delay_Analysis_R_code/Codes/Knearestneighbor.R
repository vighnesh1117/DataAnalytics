
##loading data from CSV file into R
gc = read.csv("C:/Users/vsawa/Desktop/finaldataprateek.csv", header = T)
##printing the head of CSV file
head(gc)

##loading data into variables
SCHEDULED_DEPARTURE=gc$SCHEDULED_DEPARTURE
carrier=gc$carrier
DEPARTURE_TIME=gc$DEPARTURE_TIME
dest=gc$dest
distance=gc$distance
DATE =gc$DATE
flightnumb=gc$flightnumb
origin=gc$origin
weather=gc$weather
dayweek=gc$dayweek
daymonth=gc$daymonth
tailnu=gc$tailnu
delay=gc$delay
delaytimeweathergood=gc$delaytimeweathergood
delaytimeweatherbad=gc$delaytimeweatherbad
DEPARTURE_DELAY=gc$DEPARTURE_DELAY
SCHEDULED_TIME=gc$SCHEDULED_TIME
ELAPSED_TIME=gc$ELAPSED_TIME
AIR_TIME=gc$AIR_TIME
SCHEDULED_ARRIVAL=gc$SCHEDULED_ARRIVAL
ARRIVAL_TIME=gc$ARRIVAL_TIME
ARRIVAL_DELAY=gc$ARRIVAL_DELAY
delaytimeweatherbad=gc$delaytimeweatherbad
delaytimeweathergood=gc$delaytimeweathergood
YEAR=gc$YEAR
MONTH=gc$MONTH
DAY=gc$DAY

##imporing library car for recode function
library(car)

##recoding ontime as 0 and delayed as 1
gc$delay=recode(gc$delay, "delayed" = 1,"ontime"=0)
##printing no of delayed and ontime flights
table(gc$delay)


##normalizing function
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

##applying normalizing function on data
gc_n <- as.data.frame(lapply(gc[15:18], normalize))


##splitting data into training and testing
gc_train <- gc_n[1:1761,]
gc_test <- gc_n[1762:2201,]
gc_train_labels <- gc[1:1761, 14]
gc_test_labels <- gc[1762:2201, 14]


##importing library class
library(class)

##predicting on test data at k=20
gc_test_pred <- knn(train = gc_train, test = gc_test,cl = gc_train_labels, k=20)


##importing library gmodels for displaying result in table format
library(gmodels)

##displaying result in table format
CrossTable(x= gc_test_labels, y=gc_test_pred,prop.chisq=FALSE)