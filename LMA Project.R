######################
#####LMA Project######
######################

rm(list=ls()) #clears variables
dev.off() #clears all plots

## Read CSV
airport.arrival<- read.csv("/Users/hallehghaemi/Desktop/LMAProjectDataR.csv", header=T)

## Add index and randomly order based on index
airport.arrival$index <- sample(1:29070, 29070, replace = F)
X<- airport.arrival[order(airport.arrival$index),]

## Random subset jbased on 
airport<-X[c(1:1000),]

## Plot 
cor(airport)
plot(airport)

## Subsetting data, only using observations where Arrival Delay >15 minutes
attach(airport.arrival)
airport.new<-airport.arrival[ which(airport.arrival$ArrDelay > 15), ]
attach(airport.new)

#Extrapolate outliers out of arrival delay
outlier_values <- boxplot.stats(airport.new$ArrDelay)$out
min(outlier_values)
airport.new.new<-airport.new[ which(airport.new$ArrDelay > 155), ]
attach(airport.new.new)

## Changing carriers into a factor variable and running linear model
carriers<- as.factor(airport.new.new$Carrier)
airport.carrier.lm<- lm(airport.new.new$ArrDelay ~ carriers, data = airport.new.new)
summary(airport.carrier.lm)

#Log transform of Arrival Delay
log.transform.arrdelay<-log(airport.new.new$ArrDelay)
airport.carrier.logtransform<- lm(log.transform.arrdelay ~ carriers, data = airport.new.new)
summary(airport.carrier.logtransform)

#Square root transformation of Arrival Delay times
squareroot.arrdelay<- sqrt(airport.new.new$ArrDelay)
airport.carrier.sqrt<- lm(squareroot.arrdelay ~ carriers, data = airport.new.new)
summary(airport.carrier.sqrt)

############################
## Description of Problem ##
############################
## We are predicting arrival delay based on variables: Month, DayofWeek, Carrier, 
## DestAirportID, CRSDepTime, DepDelay, ArrDelay

############
## Source ##
############
## = in Microsoft Azure Machine Learning Studio called 'Flight Delays Data'
## https://1drv.ms/u/s!Aps4xVzIwpewlP8a_QB21WmsXy8thA 

#####################################################
## Description of response and predictor variables ##
#####################################################
## See correlation plot