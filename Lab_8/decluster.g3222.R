decluster.g3222 <- function(data, 
                            col.v = 9, 
                            thresh = 50, 
                            vlag = 5, 
                            lull = 2, 
                            event.list = FALSE){
#--------------------------------------------------------------------------#
# Function to implement existing point-process scripts for GEOG 3222
# GEOG3222 STUDENTS:
# This function adapts some existing functions to your data.  It's fairly #
# flexible, and has a lot of potential uses...notably, it can identify
# stretches of time where some key conditions are met - like, say, 
# winds are above a specific threshold. We call this 'declustering'.
# Inputs: data = a data set in the standard hourly form we've been using - run 
#                grouping.ts with period = 'Day'.
#         col.v = column number, indicating the variable you want to analyse. 
#                 e.g. 6 = Temp,...10 = WindSpd. Default is windspeed
#         thresh = you can set a specific threshold defining your event.
#         event.list = determines output.  If 'FALSE', then a vector giving the 
#                  number of events per 'group' (grp) is given.  If 'TRUE', 
#                  we get a list of events - gives the start date (YMD), 
#                  duration (# of time steps), and maximum value.
#         lull  - the minimum required time steps to separate events.  E.g. for 
#                lull = 3 in hourly data, the time series would have to drop 
#                below thresh (or thresh - vlag) for at least 3 hours for 
#                two events to be considered separate. 
#        vlag   - An option that allows small drops below thresh to be ignored. 
#                E.g. if thresh is 10, and vlag is 2, then drops below 8 (10-2)
#                would be
#                necessary to be considered as candidates for event end or 
#                separation.  This basically then allows 'thresh' to be a value 
#                that must be exceeded during an event, and thresh - vlag to set
#                a lower limit on event inclusion -  e.g. 'events include dates
#                with values greater than 8, but must have a maximum value above
#                (or equal to) 10. 
# 
#   
#
# Outputs: 
  
  source('grouping.ts.R')
  source('event.decluster.R')
  source('pareto.thresh.decluster.R')
  
  #-- If requested, convert hourly data to daily.  Uses 'max' operation by default.
  if(dim(data)[2] != 11){
    stop("Needs daily data: Run through grouping.ts(period = 'Day')")
  }
  
  #--- Prep stuff for this event.decluster
  tmp.date <- data[, 1:3]
  tmp.data <- data[, col.v]

  #--- Remove NA; replace with a value thresh - vlag ---#
  if(sum(is.na(tmp.data)) > 0){
    i <- which(is.na(tmp.data))
    tmp.data[i] <- thresh - vlag
  }
  
  #--- Implement event.decluster
  out <- event.decluster(tmp.data, 
                         tmp.date, 
                         thresh = thresh, 
                         event.list = event.list,
                         lull = lull,
                         vlag = vlag,
                         grp = tmp.date$Year)

  return(out)  
}
