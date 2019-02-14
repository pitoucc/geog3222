pareto.thresh.decluster <- function(raw.ts, r, 
                                    nint = 10, 
                                    tol = 1,
                                    lull = 1, 
                                    vlag = -9999, 
                                    grp = -9999){
#-- REFILL THE CI MATRIX WITH ESTIMATES FROM DECLUSTERED DATA ---
# A function to select suitable thresholds for a Generalized Pareto 
# distribution.  The function uses the idea that above suitable thresholds
# the GP parameters remain constant in a large population. See the 
# threshrange.plot function in extRemes package...

# inputs
# raw.ts: Data to which the GP distribution will be fit
# r: 2D vector; (min, max) for range of thresholds to test
# nint: number of threshold to test; evenly split in output
# tol: "acceptance" tolerance - we want to check that all larger 
#      optional thresholds. 
# lull: necessary lull to distinguish between events
# grp:  Deals with discontinuities when looking for clusters. 
#       Assign a vector with same length as raw.ts, with integers 
#       marking different groups (e.g. 'winter 1, winter 2, etc)
# vlag: To better separate events in declustering, it is a good idea to use
#       a lower declustering threshold than POT (GP) threshold. 
#       This means that temporary weakening below the high thresholds 
#       doesn't accidentally create too many (inter-related) events. 
#       Set vlag to a the desired difference between declustering/
#       pareto thresholds. Default is 5x the increment between thresholds.

library('extRemes')

#--- Set default values. 
if(vlag < 0){
 vlag = 5*(r[2] - r[1])/nint
}

#-- Use the output of the 'threshrange.plot' program
#-- REFILL THE CI MATRIX WITH ESTIMATES FROM DECLUSTERED DATA ---

if(grp[1] < 0){
  grp = raw.ts*0. + 1
}

arr = threshrange.plot(raw.ts, r, nint = nint)

thresh = seq(r[1], r[2], length.out = nint)
for(i in 1:length(thresh)){
 ttmp <- decluster(raw.ts, thresh[i]-vlag, r = lull, groups = grp)
 ttmp <- ttmp[which(ttmp > thresh[i])]
 #tmp <- threshrange.plot(decluster(raw.ts, thresh[i], r = lull, groups = grp), 
 tmp <- threshrange.plot(ttmp,
                         c(thresh[i], (thresh[i])), 
                         nint = 1)
 arr[i, ] <- tmp
}

scale.chg = mat.or.vec(nint,nint)
shape.chg = mat.or.vec(nint,nint) 

#--- For the test: I want to look at ALL higher thresholds, and 
#    examine whether we have a difference or not. 

for(i in 2:length(thresh)){
 #if(arr[i,4] >= arr[i-1,6] | 
 #   arr[i,4] <= arr[i-1,2]){
 #     shape.chg[i] = 1
 #}

 #if(arr[i,3] >= arr[i-1,5] | 
 #   arr[i,3] <= arr[i-1,1]){
 #     scale.chg[i] = 1
 #}

 #-- For threshold i, find other thresholds that give values 
 #   indistinguishable from the ith vlaue. 

 #-SHAPE
 flg <- which(arr[i,4] >= arr[,2] & arr[i,4] <= arr[,6])
 shape.chg[flg,i] <- 1  

 #-SCALE 
 flg <- which(arr[i,3] >= arr[,1] & arr[i,3] <= arr[,5]) 
 scale.chg[flg,i] <- 1
 
}

#--- Now I need a decision rule... 
shape_dec <- thresh*0.
scale_dec <- thresh*0.

for(i in 1:(length(thresh)-1)){
  #--- Check for thresholds in which a certain percent (tol) of higher 
  #    thresholds are indistinguishable. 

  chk <- (sum(shape.chg[(i+1):nint,i])/(nint - i) >= tol)
  shape_dec[i] <- chk
  chk <- (sum(scale.chg[(i+1):nint,i])/(nint - i) >= tol)
  scale_dec[i] <- chk
}

#print(arr)

#-- Lowest Guess: bg
ibg <- which(scale_dec*shape_dec == 1)

#--- Return best guess. 
if(length(ibg) > 0){
 #--- NOW: Replot, with declustering.
   #threshrange.plot(decluster(raw.ts, thresh[min(ibg)]-vlag, r = lull),
   #                r, nint=nint)
 return(thresh[min(ibg)])
}else{
 print('No stable thresholds found')
 return(NaN)
}

}
