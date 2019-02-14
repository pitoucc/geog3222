event.decluster <- function(dx, dates, 
                         lull = 1, 
                         vlag = 0, 
                         thresh = -9999, 
                         grp = -9999, 
                         min.length = 1, 
                         event.list = TRUE){

#--- A function that declusters a time series, grouping connected events
#    together.  It's a relatively versatile declustering program.  
#    In it's simplest form, it groups consecutive days above a basic threshold 
#    (either set manually, or selected with the pareto.thresh.decluster
#    algorithm); events are separated by at least one time step below thresh.
#    Required input: 
#      dx    - input time series to be declustered. 
#      dates - dates corresponding to the time series; used to time stamp 
#              events in the final output. It should have Year, Month, Day
#              as time columns, and entries as row. 
#    Options: 
#      lull  - the minimum required time steps to separate events.  E.g. for 
#              lull = 3 in hourly data, the time series would have to drop 
#              below thresh (or thresh - vlag) for at least 3 hours for 
#              two events to be considered separate. 
#     vlag   - An option that allows small drops below thresh to be ignored. 
#              E.g. if thresh is 10, and vlag is 2, then drops below 8 (10-2)
#              would be
#              necessary to be considered as candidates for event end or 
#              separation.  This basically then allows 'thresh' to be a value 
#              that must be exceeded during an event, and thresh - vlag to set
#              a lower limit on event inclusion -  e.g. 'events include dates
#              with values greater than 8, but must have a maximum value above
#              (or equal to) 10. 
#     grp    - Accounts for discontinuities in the data, e.g. due to different
#              years. E.g. consider winter data (DJF) - you'd want to group
#              consecutive "DJF" together.  grp is a vector of same length as 
#              dx, with integers identifying unique 'groups'
#              Default is to treat as single large group.
#     event.list - determines output.  If 'FALSE', then a vector giving the 
#                  number of events per 'group' (grp) is given.  If 'TRUE', a
#                  we get a list of events - gives the start date (YMD), 
#                  duration (# of time steps), and maximum value.  
#
#    min.length - minimum number of time steps for something to be called an
#                 event

# DEPENDENCIES: pareto.thresh.decluster.R (my program), unless thresh is 
#               specificed
#               extRemes library

source('pareto.thresh.decluster.R')
library(extRemes)

#print(dates[1:10,])

#--- First: Select an appropriate threshold
# Set Range #
rng <- c(quantile(dx, 0.85, na.rm = TRUE), quantile(dx, 0.975, na.rm = TRUE))
rng <- round(rng)
if(grp[1] < -1000){
 grp = dx*0. + 1
}
nint <- (rng[2] - rng[1] + 1)
rng
#print(nint)
#print('stop')

#--- If threshold is undefined, find it ---$
if(thresh < 0){
thresh <- pareto.thresh.decluster(dx, rng, 
                                  nint = (rng[2] - rng[1] + 1), 
                                  tol = 0.85, 
                                  lull = lull, 
                                  grp = grp)
}
#print(thresh)

#thresh = 55


#-- Create output array ---#
event_data = c(1900, 1,1,0,0)

#--- Now: Decluster the data. 
#    The idea is to use this to clump data that isn't separated by 'r'. 
#    Decluster at 5km/hr below the ID'd pareto threshold;
#    this means that drops 5km/hr or less are not included in my 
#    my 'lull' analyses. 
dwspd <- decluster(dx, thresh-vlag, groups = grp, r = lull) 
#--- Loop through 'groups'
#  I need to do this to ensure that there isn't an accidental 
#  extended event that stretches across 'groups' (e.g. data 
#  blocks with end points that are sequential; like Feb 28, 1999 and 
#  Dec. 1, 1999 in a DJF analysis. 
grp_lst <- unique(grp)
grp_cnt <- grp_lst*0.
cum.day <- grp_cnt*0.
for(igrp in 1:length(grp_lst)){
  tkeep <- which(grp == grp_lst[igrp]) 
 
  twspd  <- dwspd[tkeep]
  tdates <- dates[tkeep, ]

 
  #--- Flag runs in the declustered data. 
  #    ID all entries in which the windspeed is (thresh-5) or greater. 
  #    This is consistent with my pareto.thresh.decluster algorithm, which 
  #    ignores drops below 5km as same 'event'. Note:  I'll have to remove 
  #    events that don't acheive 'thresh' later.  
  tsh_flg <- twspd*0.
  tsh_flg[twspd >= (thresh - vlag)] = 1  

  # Look for runs... ID change points
            #drop first       #Drop last
  chg_pts <- tsh_flg[-1L] != tsh_flg[-length(tsh_flg)];
  idx <- c(which(chg_pts), length(tsh_flg)) #-- Marks LAST point in a run

  #--- get lengths using the 'diff' function; gives difference between 
  #    consecutive entries. 
  rlen <- diff(c(0, idx))
  rval <- tsh_flg[idx]
  ###############
  #  KEY SUBSECTION!  USING THE 'LULL' VARIABLE!
  # OK - Remove 'zero' runs that are shorter than 'lull'
  #  Ignore the first change pnt 
  #  Because I don't care if opening 'zero' run is less than lull
  idmp <- which(rval[2:length(rval)] == 0 & rlen[2:length(rlen)] < lull) + 1
  if(length(idmp) > 0){
  for(irm in 1:length(idmp)){
    tsh_flg[(idx[idmp[irm]] - (rlen[idmp[irm]]-1)):idx[idmp[irm]]] = 1
  }
  # NOW: Rerun the change points calc: 
  chg_pts <- tsh_flg[-1L] != tsh_flg[-length(tsh_flg)];
  idx <- c(which(chg_pts), length(tsh_flg)) #-- Marks LAST point in a run
  rlen <- diff(c(0, idx))
  rval <- tsh_flg[idx]
  }
  ###### SUBSECTION COMPLETE 
  

  #--- OK! Now pull in the number of events
  i2 <- which(rval == 1 & rlen >= 1)
  if(length(i2) > 0){
    ivnt <- idx[i2]
    ilen <- rlen[i2]

    for(ii in 1:length(i2)){
      lvnt <- ivnt[ii]
      fvnt <- ivnt[ii] - (ilen[ii] - 1)
      
      

      #--- Here's where I ensure that a specific 'event' has a value that 
      #    exceeds my threshold. 
      if((max(twspd[fvnt:lvnt]) >= thresh) & (ilen[ii] >= min.length)){
        #iii <- which(yl == iyr)
        grp_cnt[igrp] = grp_cnt[igrp]+1       #--- Not currently used.  May be added later.
                                            #    Just a count per group (e.g. per year)
        cum.day[igrp] = cum.day[igrp] + ilen[ii]

        #--- Append to wind event list ---#
        nvec <- c(grp_lst[igrp], tdates[fvnt,2:3], max(twspd[fvnt:lvnt]), ilen[ii])
        event_data = rbind(event_data, nvec)
       }
     }
   }  #--- END IF: check any events occured that were strong and long enough
  

} #- igrp (groups loop)

event_data[1,]


dd <- dim(event_data)
event_data <- event_data[2:dd[1],]

if(event.list==TRUE){
  event_data <- matrix(unlist(event_data), ncol = 5)
  event.df <- data.frame(Year = event_data[,1], Month = event_data[,2],
                         Day  = event_data[,3], 
                         Max = event_data[,4],
                         Dur  = event_data[,5])

  out <- list(threshold = thresh, events = event.df)
  return(out)
} else {
  out.cnt <- data.frame(Year = grp_lst, Count = grp_cnt, Days = cum.day)
  return(out.cnt)

}


} 

