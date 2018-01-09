##loading CSV file into R
features = read.csv("C:/Users/vsawa/Desktop/features.csv", header = T)


#loading data from CSV file into variables
SCHEDULED_DEPARTURE=features$SCHEDULED_DEPARTURE
DEPARTURE_TIME=features$DEPARTURE_TIME
delay=features$delay
SCHEDULED_ARRIVAL=features$SCHEDULED_ARRIVAL
ARRIVAL_TIME=features$ARRIVAL_TIME

##importing library caret
library("caret")

##displaying heads
head(features)
names(features)

#putting data in x and y
x = features[,-5]
y = features$delay

#building the model
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

#predict fucntion to predict labels
predict(model$finalModel,x)

#predict function to predict probabilities
predict(model$finalModel,x)