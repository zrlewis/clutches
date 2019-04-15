library(tidyverse)
library(lubridate) # to work with dates, for instance the day of year = yday(Collection.Date)
library(ggplot2)
library(grid)
library(gridExtra)
library(broom)

# Regression Analyses of Environmental Variables (Precipitation, Minimum Relative Humidity, 
# Maximum Relative Humidity, and Average Relative Humidity) on Clutch Size


#import clutch size data file - “All-Clutch-Size-Data-With-All-Climate-Data.csv” by navigating to the file 

#data.all.Clutch.Size=read.table(file.choose(), header=T, sep = ",")
#attach(data.all.Clutch.Size)

data.all.Clutch.Size <-read_csv("climateData.csv", 
                       col_types=cols('Collection.Date'=col_date(format="%m/%d/%y")))


# some renaming

Clutch.Number=data.all.Clutch.Size[,1]
Collection.Date=data.all.Clutch.Size[,2]
Calendar.Date=data.all.Clutch.Size[,3]
Collection.Site=data.all.Clutch.Size[,4]
Stage=data.all.Clutch.Size[,5]
Clutch.Size=data.all.Clutch.Size[,6]
Year=data.all.Clutch.Size[,7]
Simple.Site=data.all.Clutch.Size[,8] #HF is a bin of Harvard Pond, Slab City, North of Slab City and Rutland Brook
#defining levels for categorical variables
yearLevels<-c("2012","2013","2014","2015")
#cat.Year<-parse_factor(data.all.Clutch.Size$Year,yearLevels)
siteLevels <- c("HF", "WM", "WB")
cat.Simple.Site <-parse_factor(data.all.Clutch.Size$Simple.Site,siteLevels)

# basic plot of temp over time

ggplot(data=data.all.Clutch.Size, aes(x=yday(Collection.Date), y= Air.Temp.Avg.Day.Of.Sampling)) + geom_point()


# basic plot of RH over time

ggplot(data=data.all.Clutch.Size, aes(x=yday(Collection.Date), y=Rh.Avg.Day.Of.Sampling )) + geom_point()

#checkCorr <- lm(formula = Rh.Avg.Day.Of.Sampling ~ yday(Collection.Date), data=data.all.Clutch.Size)
#summary(checkCorr)

#################
# Precipitation #
#################

ggplot(data=data.all.Clutch.Size, aes(x=yday(Collection.Date), y=Precip.Prior.Seven.Days )) + geom_point()


head(data.all.Clutch.Size)

# linear regression model assessing relationship between all precipitation variables as predictor
# variables and clutch size as response variable

modPrecipAllVarsClutchSize = lm(formula = Clutch.Size ~ Precip.Day.Of.Sampling + 
                                  Precip.Prior.Day +
                                  Precip.Prior.Three.Days + 
                                  Precip.Prior.Seven.Days + 
                                  Precip.Prior.Thirty.Days + 
                                  No.Precip.Event.Prior.Seven.Days + 
                                  No.Precip.Event.Prior.Thirty.Days, data=data.all.Clutch.Size)
summary(modPrecipAllVarsClutchSize)

#output data
#write.csv(data.frame(summary(modPrecipAllVarsClutchSize)$coefficients), file="modPrecipAllVarsClutchSize.csv")
write.csv(tidy(modPrecipAllVarsClutchSize), file="modPrecipAllVarsClutchSize.csv")

# plot pairs of precip data to see correlations

#precipTable <- select(data.all.Clutch.Size, Precip.Day.Of.Sampling, Precip.Prior.Day, Precip.Prior.Three.Days, Precip.Prior.Seven.Days, Precip.Prior.Thirty.Days, No.Precip.Event.Prior.Seven.Days, No.Precip.Event.Prior.Thirty.Days )

#pairs(precipTable)

# the no precip are highly corr and negatively correlated with precipitation events in the prior days
# try dropping them from the model 
# also precip prior 3 days, 7 days and 30 days are highly correlated. So try dropping all but one

modPrecipReducedVarsClutchSize = lm(formula = Clutch.Size ~  Precip.Prior.Day +
                                  No.Precip.Event.Prior.Seven.Days, data=data.all.Clutch.Size)
summary(modPrecipReducedVarsClutchSize)

# with a reduced model, the Precip.Prior.Day has a significant effect on clutch size, but the No.Precip does not.
# the R-squared more closely approaches the Adjusted R-squared value


# linear regression model assessing relationship between precipitation on the day prior to
# sampling as an individual predictor variable and clutch size as response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger precipitation model above

modPrecipPriorDayClutchSize = lm(formula = Clutch.Size ~ Precip.Prior.Day, data=data.all.Clutch.Size)
summary(modPrecipPriorDayClutchSize)

#output data
#write.csv(data.frame(summary(modPrecipPriorDayClutchSize)$coefficients), file="modPrecipPriorDayClutchSize.csv")
write.csv(tidy(modPrecipPriorDayClutchSize), file="modPrecipPriorDayClutchSize.csv")

# linear regression model assessing relationship between precipitation on the three days prior to
# sampling as an individual predictor variable and clutch size as response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger precipitation model above

modPrecipPriorThreeDaysClutchSize = lm(formula = Clutch.Size ~ Precip.Prior.Three.Days, data=data.all.Clutch.Size)
summary(modPrecipPriorThreeDaysClutchSize )

#output data
write.csv(tidy(modPrecipPriorThreeDaysClutchSize), file="modPrecipPriorThreeDaysClutchSize.csv")
          

modPrecipPriorSevenDaysClutchSize = lm(formula = Clutch.Size ~ Precip.Prior.Seven.Days, data=data.all.Clutch.Size)
summary(modPrecipPriorSevenDaysClutchSize)
    
modPrecipPriorThirtyDaysClutchSize = lm(formula = Clutch.Size ~ Precip.Prior.Thirty.Days, data=data.all.Clutch.Size)
summary(modPrecipPriorThirtyDaysClutchSize)

# linear regression model assessing relationship between number of days out of the seven prior
# to sampling with no precipitation event as an individual predictor variable and clutch size as 
# response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger precipitation model above

modNoPrecipEventPriorSevenDaysClutchSize = lm(formula = Clutch.Size ~ No.Precip.Event.Prior.Seven.Days, data=data.all.Clutch.Size)
summary(modNoPrecipEventPriorSevenDaysClutchSize)
#output data
write.csv(tidy(modNoPrecipEventPriorSevenDaysClutchSize), file="modNoPrecipEventPriorSevenDaysClutchSize.csv")

####################
# relative humidity
####################

# linear regression model assessing relationship between all minimum relative humidity variables
# as predictor variables and clutch size as response variable

modRhMinAllVarsClutchSize = lm(formula = Clutch.Size ~ Rh.Min.Day.Of.Sampling + 
                                 Rh.Min.Prior.Day + 
                                 Rh.Min.Prior.Three.Days + 
                                 Rh.Min.Prior.Seven.Days + 
                                 Rh.Min.Prior.Thirty.Days, data=data.all.Clutch.Size)

summary(modRhMinAllVarsClutchSize)

#output data
write.csv(tidy(modRhMinAllVarsClutchSize), file="modRhMinAllVarsClutchSize.csv")


# linear regression model assessing relationship between average minimum relative on the three days
# prior to sampling as an individual predictor variable and clutch size as response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger minimum relative humidity model above

modRhMinPriorThreeDaysClutchSize = lm(formula = Clutch.Size ~ Rh.Min.Prior.Three.Days, data=data.all.Clutch.Size)

summary(modRhMinPriorThreeDaysClutchSize)


#output data
write.csv(tidy(modRhMinPriorThreeDaysClutchSize), file="modRhMinPriorThreeDaysClutchSize.csv")


# linear regression model assessing relationship between average minimum relative on the seven days
# prior to sampling as an individual predictor variable and clutch size as response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger minimum relative humidity model above

modRhMinPriorSevenDaysClutchSize = lm(formula = Clutch.Size ~ Rh.Min.Prior.Seven.Days, data=data.all.Clutch.Size)
summary(modRhMinPriorSevenDaysClutchSize)

#output data
write.csv(tidy(modRhMinPriorSevenDaysClutchSize), file="modRhMinPriorSevenDaysClutchSize.csv")


# linear regression model assessing relationship between all maximum relative humidity variables
# as predictor variables and clutch size as response variable

modRhMaxAllVarsClutchSize = lm(formula = Clutch.Size ~ Rh.Max.Day.Of.Sampling + Rh.Max.Prior.Day + Rh.Max.Prior.Three.Days + Rh.Max.Prior.Seven.Days + Rh.Max.Prior.Thirty.Days, data=data.all.Clutch.Size)

summary( modRhMaxAllVarsClutchSize )

#output data
write.csv(tidy(modRhMaxAllVarsClutchSize), file="modRhMaxAllVarsClutchSize.csv")



# linear regression model assessing relationship between all average relative humidity variables as
# predictor variables and clutch size as response variable

modRhAvgAllVarsClutchSize = lm(formula = Clutch.Size ~ Rh.Avg.Day.Of.Sampling + Rh.Avg.Prior.Day + Rh.Avg.Prior.Three.Days + Rh.Avg.Prior.Seven.Days + Rh.Avg.Prior.Thirty.Days, data=data.all.Clutch.Size)
summary(modRhAvgAllVarsClutchSize)

#output data
write.csv(tidy(modRhAvgAllVarsClutchSize), file="modRhAvgAllVarsClutchSize.csv")


# linear regression model assessing relationship between all average relative humidity in day prior to 
# sampling as predictor variables and clutch size as response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger average relative humidity model above

modRhAvgPriorDayClutchSize = lm(formula = Clutch.Size ~ Rh.Avg.Prior.Day, data=data.all.Clutch.Size)
summary(modRhAvgPriorDayClutchSize )

#output data
write.csv(tidy(modRhAvgPriorDayClutchSize), file="modRhAvgPriorDayClutchSize.csv")


# linear regression model assessing relationship between average relative humidity variables on the 
# three days prior to sampling as predictor variables and clutch size as response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger average relative humidity model above

modRhAvgPriorThreeDaysClutchSize = lm(formula = Clutch.Size ~ Rh.Avg.Prior.Three.Days, data=data.all.Clutch.Size)
summary(modRhAvgPriorThreeDaysClutchSize)

#output data
write.csv(tidy(modRhAvgPriorThreeDaysClutchSize), file="modRhAvgPriorThreeDaysClutchSize.csv")


# linear regression model assessing relationship between average relative humidity on the seven days # prior to sampling as predictor variables and clutch size as response variable
# this variable was assessed individually because it was found to have a significant relationship in 
# the larger average relative humidity model above

modRhAvgPriorSevenDaysClutchSize = lm(formula = Clutch.Size ~ Rh.Avg.Prior.Seven.Days, data=data.all.Clutch.Size)
summary(modRhAvgPriorSevenDaysClutchSize)

#output data
write.csv(tidy(modRhAvgPriorSevenDaysClutchSize), file="modRhAvgPriorSevenDaysClutchSize.csv")

