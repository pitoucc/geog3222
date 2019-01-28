lab2.boxplot <- function(data, threshold = 37){
#---
# A custom function to create a barplot for Lab 2. 
# This makes lab 2 a little easier - but also serves as a 
# model for creating your own barplots. 
# Creates seasonal wind event counts for all years (i.e. counts of 
# high wind observations);
# Repeats this calculation with individual seasons - so you get:
# WINTER: 1980, 1981, ...2017
# SPRING: 1980, 1981, ...2017
#  ...
# FALL:   1980, 1981, ...2017
# For each season, we draw a box/whisker plot.
# Allows QUICK comparison of which seasons see high winds MOST OFTEN
# OPTIONS: set a different threshold:
#    lab2.boxplot(data, threshold = VALUE)
# Default threshold is 37 (Strong WInd Warning)
#---

source('grouping.ts.R')  #- Used to consolidate info over seasons. 

#-- build seasonal data sets:
djf <- data[(data$Month == 12 | data$Month <= 2),]
mam <- data[data$Month >= 3 & data$Month <= 5, ]
jja <- data[data$Month >= 6 & data$Month <= 8, ]
son <- data[data$Month >= 9 & data$Month <= 11, ]

#--- Pull out number of obs > 37 km/hr (Strong Wind Warning). 
#    For each year, we get the number of hours that recorded strong winds.
t.djf <- grouping.ts(djf, 
                     operation = 'threshold', 
                     thresh = threshold, period = 'Year')
t.mam <- grouping.ts(mam, 
                     operation = 'threshold', 
                     thresh = threshold, period = 'Year') 

t.jja <- grouping.ts(jja, 
                     operation = 'threshold', 
                     thresh = threshold, period = 'Year')
t.son <- grouping.ts(son, 
                     operation = 'threshold', 
                     thresh = threshold, period = 'Year')


#-- Use WindSpd threshold counts from each season to build a new data frame
#   for plotting. 
tmp <- data.frame(DJF = t.djf$WindSpd, 
                  MAM = t.mam$WindSpd,
                  JJA = t.jja$WindSpd, 
                  SON = t.son$WindSpd)

 boxplot(tmp, 
         main = "Annual Hours w/Strong Winds, by Season", 
         xlab = "Season", 
         ylab = "Number of Strong Wind Observations", 
         col = c('deepskyblue', 'chartreuse3', 'cornsilk', 'darkorange'))

}
