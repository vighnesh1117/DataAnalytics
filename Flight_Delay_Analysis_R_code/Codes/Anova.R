
##loading CSV file into R
anova = read.csv("C:/Users/vsawa/Desktop/ANOVA.csv", header = T)


##loadin data of CSV file into variables
ARRIVAL_DELAY = anova$ARRIVAL_DELAY
DELAY_LABEL = anova$DELAY_LABEL

##plotting box plot of different delay labels Method1
boxplot(ARRIVAL_DELAY~DELAY_LABEL,data=anova, main="DELAY ANALYSIS", xlab="DELAY_LABEL", ylab="ARRIVAL_DELAY")

##plotting box plot of different delay labels Method1
plot(ARRIVAL_DELAY~DELAY_LABEL)

##seeing which columns of data set are in used
attach(anova)


#defining delay labels
high = anova[which(DELAY_LABEL=='high'),]
medium = anova[which(DELAY_LABEL=='medium'),]
low = anova[which(DELAY_LABEL=='low'),]
ontime = anova[which(DELAY_LABEL=='ontime'),]

##obtaining summary of delay labels
summary(high)
summary(medium)
summary(low)
summary(ontime)


#fitting anova model
anov = lm(ARRIVAL_DELAY~DELAY_LABEL)
summary(anov)


#plottting predicted vs residuals in Residual analysis
res = rstandard(anov)
plot(fitted(anov),res, main = "Predicted vs residuals plot")
abline(a=0, b=0, col='red')



#qqplot on best fitted model anov
res = rstandard(anov)
qqnorm(res)
qqline(rstandard(anov),col='red')