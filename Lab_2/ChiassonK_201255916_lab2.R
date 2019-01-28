# Lab 2
# Karl Chiasson
# 201255916

labPC <- FALSE

#Set the working directory
if (labPC) {setwd('D:\\geog3222_201255916\\lab2')
} else {setwd('D:\\Class\\geog_3222\\Labs\\Lab_2')}

#Load the required data/code
load('Dataset3.Rdata')
source('plot.windrose.R')
source('lab2.boxplot.R')
source('grouping.ts.R')

#Create a pdf with the seasonal box plot of the wind data
pdf('dataset3_seasonal_boxplot_wind_obeservations.pdf')
lab2.boxplot(data)
dev.off()

print("From the box plots it is possible to see that the frequency of strongs winds occurs 
       more often during the winter and spring months. It also shows that winter is more likely to
       be at risk for the strongest winds while summer is the least likely to be at risk for strong winds, therefore
       having the smallest winds.")
      
#Create logical subsets of data for a given season 
Winter <- c(which(data$Month == 12),which(data$Month <= 2))
Summer <- which(data$Month >= 6 & data$Month <= 8)

#Create data subsets for each season using logical subsets
data.djf <- data[Winter,]
data.jja <- data[Summer,]

#Create pdfs with seasonal wind oberservation histogams
#These histograms shows ranges of wind speed and their frequency
pdf("dataset3_winter_wind_observation_histogram.pdf")
hist(data.djf$WindSpd,breaks=20,xlim=c(0,80),ylim=c(0,25000),xlab="Wind speed (km\\h)",main="Histogram for Winter Wind Speeds")
dev.off()
pdf("dataset3_summer_wind_observation_histogram.pdf")
hist(data.jja$WindSpd,breaks=20,xlim=c(0,80),ylim=c(0,25000),xlab="Wind speed (km\\h)",main="Histogram for Summer Wind Speeds")
dev.off()

print("Looking at both windrose plots, the winds most refrequently south westernly winds followed bty
       north easternly winds and westernly winds. The windspeeds of winter are greater then that of summer
       and it shows that winter has a higher chance for strong winds")

#Create pdfs which plots windrose diagrams of each season
#This diagram shows frequency of wind direction and intensity
pdf("dataset3_winter_wind_direction_frequency_and_intensity.pdf")
plot.windrose(data.djf)
dev.off()
pdf("dataset3_summer_wind_direction_frequency_and_intensity.pdf")
plot.windrose(data.jja)
dev.off()

print("Boxplots are very useful to identify if data is relative to each other, based on thier
       means being in the same range as each others boxes, and to identify any outliers but it may 
       not be good at showing the general distribution of the data or differences in frequencies. 
       Histograms are good at showing the distribution of data and show the frequencies to where data
       may lie, but they show all the data and make it difficult to identify which data are outliers.
       Windrose are good at showing data which could be represented in some form of vector, but they 
       do not show information related to data distribution or show outliers in data.")