plot.c.freq <- function(vec, main = NULL, xlab = "Value", breaks = 1 ){
#------------------------------------------------------------------------#
# Plots the cumulative frequencya given vector.
# Input:
#     vec - a data vector. Like, say, a vector with a bunch of windspeed 
#           observations. 
# Options: main - A character string with the main title. 
#          xlab - label for the x-axis
#          breaks - Similar to breaks in Histogram; sets up the increments
#                   for the cumulative freq plot. Default is:
#                     [min(vec)-1:max(vec)+1]
#                   Should be a vectl'or.
# 
#------------------------------------------------------------------------#

#--- Set default breaks:
if(length(breaks) == 1){
  breaks <- seq(min(vec, na.rm = TRUE)-1, max(vec, na.rm = TRUE)+2, 1)
}

h <- hist(vec, breaks = breaks)

plot(h$mids, cumsum(h$counts)/sum(is.finite(vec)),
    main = main, 
    xlab = xlab, 
    ylab = "Probability", 
    type = 'l' )
  
}