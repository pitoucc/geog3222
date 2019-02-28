dual.qq <- function(x){
# Function to create side-by-side qq plots for gaussian & weibull fits
# Input: x - a vector of observations; we'll fit parametric distributions
#            to this data. 
#
  
#-- Prep to make side-by-side plots
par(mfrow = c(1,2))

#-- Prep to fit distributions; Weibull requires values > 0; fix 
#   zeros
x[x == 0] <- 0.01
#x <- x[x > 0]

#- Fit Gaussian dist:
G.fit <- fitdistr(na.exclude(x), 'normal')

#- Fit Weibull dist:
W.fit <- fitdistr(na.exclude(x), 'weibull')

#-- Sort the input data:
x.srt <- sort(na.exclude(x))

#-- Get prob of each sort x value:
p.srt <- (c(1:length(x.srt))-1/3)/(length(x.srt)+1/3)

y.gaus <- qnorm(p.srt, 
                mean = G.fit$estimate[1], 
                sd = G.fit$estimate[2])

y.wb <- qweibull(p.srt, 
              shape = W.fit$estimate[1], 
              scale = W.fit$estimate[2])

#PLOT QQ Gaussian
qqplot(x.srt, y.gaus, 
       main = 'QQ Plot; Gaussian', 
       xlab = 'Observed Quantiles', 
       ylab = 'Predicted Quantiles')

#PLOT QQ Weibull

qqplot(x.srt, y.wb, 
       main = 'QQ Plot; Weibull', 
       xlab = 'Observed Quantiles', 
       ylab = 'Predicted Quantiles')

#Reset plotting parameter: one image only.
par(mfrow=c(1,1))
}
