# babyplanes
Linear Models

rm(list=ls()) #clears variables
dev.off() #clears all plots

## load libraries
library(dplyr)
library(ggplot2)
library(car)
library(GGally)

## Read CSV
airport.arrival.original<- read.csv("/Users/hallehghaemi/Desktop/delaysrev4.csv", header=T) 
summary(airport.arrival.original) 

## Outlier removal in response variable (arrdel)
outlier_values <- boxplot.stats(airport.arrival.original$arrdel)$out
airport.arrival.outliersremoved <- subset(airport.arrival.original, !(arrdel %in% outlier_values))
max(airport.arrival.outliersremoved$arrdel)

## Correlation plot for initial relationship determination as well as variable dropping determination
ggpairs(airport.arrival.outliersremoved[sample(1:nrow(airport.arrival.outliersremoved),2000),])

## Creating linear model
lm.one<- lm(arrdel~Month+dow+dephr+adclass+Carrier,data=airport.arrival.outliersremoved %>% 
  mutate(Month=as.factor(Month), dow=as.factor(dow), adclass=as.factor(adclass), Carrier=as.factor(Carrier)))
summary(lm.one)
plot(lm.one)

## Full model 
lm.full<- lm(arrdel~Month+dow+dephr+adclass+Carrier+destid+CRSDepTime+dephrt+depdel+CRSArrTime,data=airport.arrival.outliersremoved %>% 
              mutate(Month=as.factor(Month), dow=as.factor(dow), adclass=as.factor(adclass), dephr=as.factor(dephr), adclass=as.factor(adclass)
                     , dephrt=as.factor(dephrt), Carrier=as.factor(Carrier)))
summary(lm.full)
head(airport.arrival.outliersremoved)


##Square root transformation arrdel
lm.two<- lm((arrdel^2)~Month+dow+dephr+adclass+Carrier,data=airport.arrival.outliersremoved %>% 
              mutate(Month=as.factor(Month), dow=as.factor(dow), adclass=as.factor(adclass), Carrier=as.factor(Carrier)))
summary(lm.two)
plot(lm.one)

##Step function: MODEL SELECTION
step(lm.one)
step(lm.two)
step(lm.full, data=airport.arrival.outliersremoved, direction="backward")
step(lm.interaction)

## Plots to determine quadratic/square relationships
month.asfactor<-as.factor(airport.arrival.outliersremoved$Month)
dow.asfactor<-as.factor(airport.arrival.outliersremoved$dow)
dephr.asfactor<-as.factor(airport.arrival.outliersremoved$dephr)

plot(month.asfactor, airport.arrival.outliersremoved$arrdel, main = "Month vs. Arrival Delay", xlab = "Month", ylab = "Arrival Delay")
plot(dephr.asfactor, airport.arrival.outliersremoved$arrdel, main = "Departure Hour vs. Arrival Delay", xlab = "Departure Hour", ylab = "Arrival Delay")
plot(airport.arrival.outliersremoved$Carrier, airport.arrival.outliersremoved$arrdel, main = "Carrier vs. Arrival Delay", xlab = "Carrier", ylab = "Arrival Delay")
plot(airport.arrival.outliersremoved$adclass, airport.arrival.outliersremoved$arrdel, main = "Airport Delay Class vs. Arrival Delay", xlab = "Airport Delay Class", ylab = "Arrival Delay")
plot(dow.asfactor, airport.arrival.outliersremoved$arrdel, main = "Day of Week vs. Arrival Delay", xlab = "Day of Week", ylab = "Arrival Delay")


## Interactions
ggpairs(airport.arrival.outliersremoved[sample(1:nrow(airport.arrival.outliersremoved),2000),])

lm(arrdel~Month*dow, data=airport.arrival.outliersremoved %>% mutate(Month=as.factor(Month),
                                                            dow=as.factor(dow))) %>%
  summary()


lm.interaction<- lm(arrdel~Month*dow+dephr+adclass+Carrier,data=airport.arrival.outliersremoved %>% 
              mutate(Month=as.factor(Month), dow=as.factor(dow), adclass=as.factor(adclass), Carrier=as.factor(Carrier)))
summary(lm.interaction)


step(lm.interaction)

## Confidence Intervals
confint(lm.interaction,level=0.99)

## Diagnostic Tests
summary(lm.interaction)

