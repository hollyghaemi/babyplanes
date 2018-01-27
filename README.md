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

########################
#######ANALYSIS#########
########################
## Significant correlations with arrdel:
## month = 0.0401
## dow = -0.0204
## dephr = 0.0569
## chose 3 with highest absolute correlation values
## besides obvious (depdel, dephr, crsarrtime, dephrt)
## and 2 factor variables adclass and carrier
## Arrdelay has an almost normal distribution, with mean lying a bit 
## lower than 0. 
## There are no linear or quadratic relations between arrival
## delay and the other variables seen from the correlation plots. 
#####################

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


########################
#######ANALYSIS#########
########################
## month6, dow4, dow5, dow6, dephr, adclassLOW, adclassMEDIUM, adclassVERY HIGH, adclassVERY LOW, CarrierAS
## were all significant 
########################

##Square root transformation arrdel
lm.two<- lm((arrdel^2)~Month+dow+dephr+adclass+Carrier,data=airport.arrival.outliersremoved %>% 
              mutate(Month=as.factor(Month), dow=as.factor(dow), adclass=as.factor(adclass), Carrier=as.factor(Carrier)))
summary(lm.two)
plot(lm.one)

########################
#######ANALYSIS#########
########################
## Squared transformation of response variable 
## shows greater normality as the values in the QQ plot lie
## closer to the line
########################

##Step function: MODEL SELECTION
step(lm.one)
step(lm.two)
step(lm.full, data=airport.arrival.outliersremoved, direction="backward")
step(lm.interaction)

########################
#######ANALYSIS#########
########################
## linear model with nontransformed response variable has
## a lower AIC. All Variables kept
########################

## Plots to determine quadratic/square relationships
month.asfactor<-as.factor(airport.arrival.outliersremoved$Month)
dow.asfactor<-as.factor(airport.arrival.outliersremoved$dow)
dephr.asfactor<-as.factor(airport.arrival.outliersremoved$dephr)

plot(month.asfactor, airport.arrival.outliersremoved$arrdel, main = "Month vs. Arrival Delay", xlab = "Month", ylab = "Arrival Delay")
plot(dephr.asfactor, airport.arrival.outliersremoved$arrdel, main = "Departure Hour vs. Arrival Delay", xlab = "Departure Hour", ylab = "Arrival Delay")
plot(airport.arrival.outliersremoved$Carrier, airport.arrival.outliersremoved$arrdel, main = "Carrier vs. Arrival Delay", xlab = "Carrier", ylab = "Arrival Delay")
plot(airport.arrival.outliersremoved$adclass, airport.arrival.outliersremoved$arrdel, main = "Airport Delay Class vs. Arrival Delay", xlab = "Airport Delay Class", ylab = "Arrival Delay")
plot(dow.asfactor, airport.arrival.outliersremoved$arrdel, main = "Day of Week vs. Arrival Delay", xlab = "Day of Week", ylab = "Arrival Delay")

########################
#######ANALYSIS#########
########################
## Many outliers seen in a majority of the boxplots, therefore will 
## compare means of populations with anova. 
########################

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

########################
#######ANALYSIS#########
########################
## Interaction found between month and day of week as several of them showed significance when 
## a summary of their interaction model was run. Initially month and day of week were tested because
## of domain knowledge. Interaction model as the lowest AIC thus far at a value of 144081. This is higher 
## than the full model's AIC but we will not use that model because of domain knowledge such as obvious 
## and therefore insignificant correlation betweeen arrival delay and also departure delay CRSDepTime, dephr, 
## and dephrt are all based on the same time so only one was chosen (dephr). 
########################

## Diagnostic Tests
summary(lm.interaction)

########################
#######ANALYSIS#########
########################
## F-TEST FOR LACK OF FIT: Assumptions are (1) the observations are independent
## (2) normally distributed (3) the distributions of Y- have same variance sigma squared
## Hypotheses: H0: b1=b2=..., Ha: E{Y} =/ betas are not all zeros 
## The F-stat is 80.73 on 34 and 26395 degrees of freedom with a p-value 
## that is < 2.2e-16. Therefore we can conclude that we can reject Ho in favor of Ha
## the model is significant and is a good fit for the data.
## The anova test showed that all the variables were significant....WHAT DOES THIS MEAN? 
########################


## Statistical test

