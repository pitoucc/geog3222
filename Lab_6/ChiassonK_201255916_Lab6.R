# Geog 3222 - Lab 6
# Karl Chiasson
# 201255916

#Set working directory
setwd('D:\\Class\\geog_3222\\Labs\\Lab_6')

#Load data/scripts/libraries
library(extRemes)
library(MASS)
load('Dataset3.Rdata')
source('decluster.g3222.R')
source('event.decluster.R')
source('pareto.thresh.decluster.R')
source('grouping.ts.R')

#Applies grouping.ts to get daily maxima
nl_daily_maxima <- grouping.ts(data,period='Day',operation='max')

#Declusters wind data to identify "wind storms"
wind_storm_eventlist <- decluster.g3222(nl_daily_maxima, thresh = 50, lull = 2, vlag = 5, event.list = TRUE)
wind_storm_eventlist
wind_storms <- decluster.g3222(nl_daily_maxima, thresh = 50, lull = 2, vlag = 5, event.list = FALSE)
wind_storms

#Prints a paragraph explaining the output from decluster
print("filler")

#Applies fitdistr to identify the parameters of best fitting Poison and NB distributions
param_poi <- fitdistr(wind_storms$Count,densfun="Poisson")
param_nb <- fitdistr(wind_storms$Count,densfun="negative binomial")

#Prints the parameters and the statistics of annual count data.
mean(wind_storms$Count)
sd(wind_storms$Count)
param_poi
param_nb

print("filler")

#Plots a histogram of annual windstorm counts
hist(wind_storms$Count, ylim = c(0,10), breaks=10)

#Uses parameters to add lines to show poisson and nb distribution
dpois(wind_storms$Count, param_poi$estimate)

#Calculates the probability of anniual windstorm counts using poisson and nb distributions

#Prints a paragraph discussing the suitability of the distributions for the data
print("filler")

#Identifies the parameters of best fitting NB and geometric distributions of windstorm duration data
param_nb <- fitdistr(wind_storm_eventlist$events$Dur,densfun="negative binomial")
param_geo <- fitdistr(wind_storm_eventlist$events$Dur,densfun="geometric")

#Prints the parameters
param_nb
param_geo

#Plots a histogram of windstorm durations
hist(wind_storm_eventlist$events$Dur, ylim =c(0,140), breaks = 5)

#Uses the parameters to add lines to the histrogram for geometric and nb distributions

#Calculates the probability of the windstorm counts using geometric and NB distributions

#Prints a paragraph discussing the suitability of the distributions for the data
print("filler")

#Prints a paragraph discussing the probabilities
print("filler")