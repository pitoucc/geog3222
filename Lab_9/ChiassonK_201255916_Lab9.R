#Karl Chiasson
#201255916
#Lab 9 - Preproject

#set working directory
setwd("/home/pitoucc/Documents/geog3222/Lab_9")

#load data/scripts
source("grouping.ts.R")
source("decluster.g3222.R")
source("event.decluster.R")
source("pareto.thresh.decluster.R")
load("Dataset1.Rdata") #St. John's data
dataSJ <-data
load("Dataset2.Rdata") #Gander data
dataGD <-data

boxplot(dataSJ$WindSpd,dataGD$WindSpd)