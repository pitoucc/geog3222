#LAb 3
#Karl Chiasson
#201255916

labPC <- FALSE

#Set the working directory
if (labPC) {setwd('D:\\geog3222_201255916\\lab3')
} else {setwd('D:\\Class\\geog_3222\\Labs\\Lab_3')}

#Load the required data/code
load('Dataset3.Rdata')
source('grouping.ts.R')

#Gets the daily maximum windspeed
dailyMax <- grouping.ts(data, period = "Day", operation = "max")

#Gets the daily mode of the wind drirection
dailyMode <- grouping.ts(data, period = "Day", operation = "mode")

#Creates two events and places the daily data in each 
#event based on windspeed
E1 <- dailyMax[which(dailyMax$WindSpd<37),]
E2 <- dailyMax[which(dailyMax$WindSpd>=37),]

#Prints paragraph discussing the events E1 and E2


#Assigns each day to a specific category
A1<-which(dailyMode$WindDir > 315 | dailyMode$WindDir <= 45)
A2<-which(dailyMode$WindDir > 45 & dailyMode$WindDir <= 135)
A3<-which(dailyMode$WindDir > 135 & dailyMode$WindDir <= 225)
A4<-which(dailyMode$WindDir > 225 & dailyMode$WindDir <= 315)

#Prints a paragraph discussing events A1-A4

#Calculats P{wind watch}
Pwindwarn <- length(E2$WindSpd)/length(dailyMax$Day)

#Calculates and reports P{west wind}
length(A4)/length(dailyMax$Day)

#Calculates and reports P{north wind}
length(A1)/length(dailyMax$Day)

#Calculates and reports P{E2} given certain criteria
(Pwindwarn*length(A1)/length(dailyMax$Day))/Pwindwarn
(Pwindwarn*length(A2)/length(dailyMax$Day))/Pwindwarn
(Pwindwarn*length(A3)/length(dailyMax$Day))/Pwindwarn
(Pwindwarn*length(A4)/length(dailyMax$Day))/Pwindwarn

#Reports on the relationship of wind hazards and wind direction

#Calculates and reports P{A4} given E2 is occuring

#Proves Bayes Theorem applies to the wind data set

#Prints a paragraph discussing Frequentist vs Bayesianwarnings