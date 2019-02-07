calc.anomalies <- function(data,w.len = 7){
  #--------------------------------------------------------------------------------#
  # Given one of our data sets, pull out a daily annual cycle (mean) and standard  #
  # deviation (uncertainty)
  # There's a smoothing step, based on a moving window of w.len (must be odd). This#
  # allows us to pull info from surrounding (similar) days.
  # Initial results are then smoothed, with iterative moving averages; 
  # each iteration decreases w.len by 1, until we get to 3. 
  # It's clunky, but easier to automate than an early FFT approach.
  # Input: a data frame with our env canada format, used in GEOG3222
  #       w.len = moving average window.
  # Output: A list, with means, std dev, anomalies, and std anomalies. First two are
  #         cyclic; the 2nd two are not.
  #
  #         out$annual.mean
  #         out$annual.sd
  #         out$anomalies
  #         out$std.anomalies
  
  # Dependencies: grouping.ts.R
  #               ann.mean (above)
  #               ann.sd (above)
  
  
  source('grouping.ts.R')
  if(dim(data)[2]==13){
    data <- grouping.ts(data,period = "Day", operation = "mean")
  }
  if(dim(data)[2] < 11){
    stop("Input data doesn't have the correct dimensions; try your raw 3222 Dataset data")
  }
  
  #--- Ready! Start by running my moving average/sd formulas:
  dy.mn <- ann.mean(data, w.ln = w.len)
  dy.sd <- ann.sd(data, w.ln = w.len)
  
  #--- Repeat, but incrementally decreasing w.len:
  while(w.len >= 3){
    dy.mn <- ann.mean(dy.mn, w.ln = w.len)
    dy.sd <- ann.mean(dy.sd, w.ln = w.len)
    w.len = w.len - 1
  }
  
  #- Empty target arrays (or arrays to be rewritten)
  anoms     <- data - dy.mn
  anoms[, 1:4] <- data[, 1:4]
  std.anoms <- (data - dy.mn)/dy.sd
  std.anoms[, 1:4] <- data[,1:4]
  clim      <- dy.mn
  clim.err  <- dy.sd
  
  
  # Merge into a list
  out <- list( annual.mean = clim, 
               annual.sd = clim.err, 
               anomalies = anoms, 
               std.anomalies = std.anoms)
  
  return(out)
}

#--- Annual cycles: Calculate 'em. Also, Std Dev. 
ann.sd <- function(data, w.ln = 5){
  #----
  # Function to calculate annual cycle of standard deviation of daily data;
  # it's based on all days in a w.ln length window around a calendar date.
  # Returns a time series
  # Prep output array
  
  out <- data*NA
  out$Year <- data$Year
  out$Month <- data$Month
  out$Day <- data$Day
  
  hlen <- ceiling((w.ln-1)/2)
  
  #--- Loop through all days of the year
  ndays <- c(31,28,31,30,31,30, 31, 31, 30, 31, 30, 31)
  for(im in 1:12){
    for(id in 1:ndays[im]){
      i <- which(data$Month == im & data$Day == id)
      imin <- i - hlen
      imin[imin < 0] <- 1
      imax <- i + hlen
      imax[imax > dim(data)[1]] <- dim(data)[1]
      
      ind <- c(imin, i, imax)
      for(iv in 5:11){
        out[ind,iv] <- sd(data[ind,iv], na.rm = TRUE)
      } 
    }
  }
  
  return(out)
}
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Two functions, necessary for calc.anomalies to work
ann.mean <- function(data, w.ln = 5){
  #----
  # Function to calculate annual cycle of standard deviation of daily data;
  # it's based on all days in a w.ln length window around a calendar date.
  # Returns a time series
  # Prep output array
  
  out <- data*NA
  out$Year <- data$Year
  out$Month <- data$Month
  out$Day <- data$Day
  
  hlen <- ceiling((w.ln-1)/2)
  
  #--- Loop through all days of the year
  ndays <- c(31,28,31,30,31,30, 31, 31, 30, 31, 30, 31)
  for(im in 1:12){
    for(id in 1:ndays[im]){
      i <- which(data$Month == im & data$Day == id)
      imin <- i - hlen
      imin[imin < 0] <- 1
      imax <- i + hlen
      imax[imax > dim(data)[1]] <- dim(data)[1]
      
      ind <- c(imin, i, imax)
      for(iv in 5:11){
        out[ind,iv] <- mean(data[ind,iv], na.rm = TRUE)
      } 
    }
  }
  
  return(out)
}
