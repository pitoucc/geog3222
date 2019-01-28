plot.windrose <- function(data){
#------------------------------------------------------------------------------#
# A function to quickly take GEOG3222 data sets (provided by Finnis), reformat #
# the wind speed/direction data, and create a windrose. 
# Uses the windRose function in the openair package.
# (should be installed ahead of time). 
# Converts speed (km/hr) to m/s
# Converts direction (10s of deg) to degrees
# Merges these two converted fields into a new dataframe
# then plots.
#------------------------------------------------------------------------------#
library('openair')

#--- Reformat data & create a new dataframe with the correct setup for 
#    windRose

tmp <- data.frame(ws = data$WindSpd*1000/60/60,   #-- km/hr => m/s 
                  wd = data$WindDir*10)           #-- tens of deg => deg

#windRose(tmp, breaks = c(0,2,4,6,8,10,12))
pollutionRose(tmp, pollutant = 'ws', breaks = c(0,2,4,6,8,10,12))

}
