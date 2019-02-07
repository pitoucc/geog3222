grouping.ts <- function(data, period = "Month", operation = "mean", 
                        climate = FALSE, thresh = 50){

#----------------------------------------------------------------------#
# A fairly flexible function to take your hourly data and pull out     #
# useful daily, monthly, or annual data.                 
#                                                                      #
# Input:                                                               #
#   dataset: one of the hourly datasets provided for GEOG 3222.        #
#   period:  A period over which to collate your data; default is      #
#              Monthly. 
#                May be "Day", "Month" or "Year"                       #
#   operation: What function will you use grouping your data?          #
#                mean (default): average over the period of interest   #
#                mode: Get the mode over period of interest            #
#                      REQUIRES Mode.R IN WORKING DIRECTORY            #
#                max: maximum over period of interest
#                min: minimum over period of interest                  #
#                sum: total over period of interest                    #
#                threshold: count the instances above a threshold.     #
#                           Set threshold value with thresh =...       #
#                           Default is 50.                             #
#  climate: if TRUE, creates a climatology (avg over all years.)       #
#              Default is false.
#              
#   EXAMPLE: Suppose you want the ANNUAL MAXIMUM value:             
#              > ndata <- grouping.ts(data, period = "Month",          #        
#                                       operation = "max")             #
#----------------------------------------------------------------------#

#-- Create unique identify for each period to be analyzed.
if(climate == FALSE){

 if(period == "Day"){
   grouplist <- list(data$Year*10000+data$Month*100+data$Day)
  }
  if(period == "Month"){
   grouplist <- list(data$Year*100+data$Month)
  }
  if(period == "Year"){
   grouplist <- list(data$Year)
  }
}

#--- Lists for creating climatologies. ---#
if(climate == TRUE){
 if(period == "Day"){
   grouplist <- list(data$Month*100+data$Day)
  }
  if(period == "Month"){
   grouplist <- list(data$Month)
  }
  if(period == "Year"){
   print("Error: climate = TRUE & period = Year makes no sense.")
   print("       Setting climate = FALSE & Proceeding.")
   grouplist <- list(data$Year)
  }
}

# This is my clumsy approach; there are neater ways to do this. But for now,
# this works. Use 'if' to determine which operations to run.
#-- First check that the indicated operation is an option:
if(sum(c("mean", "min", "max", "sum", "mode", "threshold") == operation) != 1){
   print("ERROR: Operation must be mean, max, min, or sum.")
   print("       Resetting operation to default (mean)")
   operation = "mean"
}
#- Now do the calculations; efficiently performed with the 
#  aggregate function.  
#  Only complex calc is for threshold operation; and even then that
#  just entails making a copy of the data for safety and rewriting
#  data with logical ( > thresh)
if(operation == "mean"){
  x <- aggregate(data[,6:12], by = grouplist, mean, na.rm = TRUE)
}
if(operation == "max"){
  x <- aggregate(data[,6:12], by = grouplist, max, na.rm = TRUE)
}
if(operation == "min"){
  x <- aggregate(data[,6:12], by = grouplist, min, na.rm = TRUE)
}
if(operation == "mode"){
  Mode <- function(x) {
   ux <- na.omit(unique(x))
   ux[which.max(tabulate(match(x, ux)))]
  }
  #x <- by(data[,6:12], grouplist, FUN = Mode)
  x <- aggregate(data[,6:12], by = grouplist, Mode)
}
if(operation == "sum"){
  x <- aggregate(data[,6:12], by = grouplist, sum, na.rm = TRUE)
} 
if(operation == 'threshold'){
  data.t <- data # Copy data
  data.t[,6:12] <- data.t[,6:12] > thresh   # replace data w/T/F
  x <- aggregate(data.t[,6:12], by = grouplist, sum, na.rm = TRUE) # Count T's
}

#- Rename the first column:
names(x)[1] <- "Date"

#- Convert Date into year/month/day etc
if(max(x$Date) >= 10000000){
  yr <- floor(x$Date/10000) 
  mo <- floor(x$Date/100) - yr*100
  da <- x$Date - yr*10000 - mo*100

  date.info <- data.frame(Year = yr, Month = mo, Day = da)
}
if(max(x$Date) < 10000000 & max(x$Date) > 100000){
  yr <- floor(x$Date/100)
  mo <- x$Date - yr*100

  date.info <- data.frame(Year = yr, Month = mo)
}
if(max(x$Date) < 100000){
  yr <- x$Date
  date.info <- data.frame(Year = yr)
}

#--- Combine date info with aggregated data
x <- cbind(date.info, x)

return(x)
}
