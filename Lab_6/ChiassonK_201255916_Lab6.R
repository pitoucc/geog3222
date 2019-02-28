# Geog 3222 - Lab 6
# Karl Chiasson
# 201255916

#Set working directory
setwd('D:\\Class\\geog_3222\\Labs\\Lab_6')

#Load data/scripts/libraries
library(extRemes)
library(MASS)
load('Dataset3.Rdata')
source('decluster.g3222.R')
source('event.decluster.R')
source('pareto.thresh.decluster.R')
source('grouping.ts.R')

#Applies grouping.ts to get daily maxima
nl_daily_maxima <- grouping.ts(data,period='Day',operation='max')

#Declusters wind data to identify "wind storms"
wind_storm_eventlist <- decluster.g3222(nl_daily_maxima, thresh = 50, lull = 2, vlag = 5, event.list = TRUE)
wind_storm_eventlist
wind_storms <- decluster.g3222(nl_daily_maxima, thresh = 50, lull = 2, vlag = 5, event.list = FALSE)
wind_storms

#Prints a paragraph explaining the output from decluster
print("When event.list is true it returns a set of wind data, for a given threshold, that shows us the date of when a wind event has\n
       has occured, the duration of that wind event, and how long the occurance took place. When event.list is false it returns a set\n
       of wind data which groups events together and shows the counts and duration of these events.")

#Applies fitdistr to identify the parameters of best fitting Poison and NB distributions
param_poi <- fitdistr(wind_storms$Count,densfun="Poisson")
param_nb <- fitdistr(wind_storms$Count,densfun="negative binomial")

#Prints the parameters and the statistics of annual count data.
wsmean <- mean(wind_storms$Count)
wssd <-sd(wind_storms$Count)
wsmean
wssd
param_poi
param_nb
paste("Based on the parameters and the statistics that the values between the mean:",wsmean,", lambda",param_poi$estimate, "and mu:", param_nb$estimate[[2]],"are approximately equal also that\n
       the standard deviation:",wssd," and the negative binomial size:",param_nb$estimate[[1]], " are approximately equal, leaving us to conclude that these distributions\n
       may be suitable for this data.")

#Plots a histogram of annual windstorm counts
pdf("Wind_storms_hist_poi_nb_distributions.pdf",width = 11, height = 8)
hist(wind_storms$Count, ylim = c(0,0.3), breaks=10, prob =T,xlab="Number of Wind Storms per Year", main="Histogram of Wind Storm Frequency")

#Uses parameters to add lines to show poisson and nb distribution
range_pnb <- min(wind_storms$Count):max(wind_storms$Count)
lines(dpois(range_pnb,lambda = param_poi$estimate), type = "p", col = "red")
lines(dnbinom(range_pnb,mu = param_nb$estimate[[2]], size = param_nb$estimate[[1]]), type = "p", col = "blue")
legend(7.5,0.3, legend = c("Poisson", "Negative Binomial"), col=c("red","blue"), cex=0.8, pch=1, title = "Distributions")
dev.off()

#Calculates the probability of anniual windstorm counts using poisson and nb distributions
ppois(range_pnb,lambda = param_poi$estimate,lower.tail =F)
pnbinom(range_pnb,mu = param_nb$estimate[[2]], size = param_nb$estimate[[1]],lower.tail = F)

#Prints a paragraph discussing the suitability of the distributions for the data
print("From the appearance of both histograms it may seem that the distributions may not be suitable for the data sicnce the\n
       distributions do not always line up with the counts shown in the historgrams. There are some sections where the points for the\n
       distributions do match but if we refer to the probabilities, we see that as count of wind storms increases the probabilities of\n
       of those counts decrease, thus fitting to the distributions. In this case the negative binomial may be slightly more suited given\n
       the shape of the data.")

#Identifies the parameters of best fitting NB and geometric distributions of windstorm duration data
param_nb2 <- fitdistr(wind_storm_eventlist$events$Dur,densfun="negative binomial")
param_geo <- fitdistr(wind_storm_eventlist$events$Dur,densfun="geometric")

#Prints the parameters
param_nb2
param_geo

#Plots a histogram of wind storm durations
pdf("Wind_storm_event_hist_geo_nb_distributions.pdf",width = 11, height = 8)
hist(wind_storm_eventlist$events$Dur, ylim=c(0,1), breaks = 5, prob =T, xlab="Length of Wind Storms (days)", main="Histogram of Wind Storm Duration")

#Uses the parameters to add lines to the histrogram for geometric and nb distributions
range_gnb <- min(wind_storm_eventlist$events$Dur):max(wind_storm_eventlist$events$Dur)
lines(dgeom(range_gnb, p=param_geo$estimate), type = "p", col = "red")
lines(dnbinom(range_gnb,mu = param_nb2$estimate[[2]], size = param_nb2$estimate[[1]]), type = "p", col = "blue")
legend(4,1, legend = c("Geometric", "Negative Binomial"), col=c("red","blue"), cex=0.8, pch=1,title = "Distributions")
dev.off()

#Calculates the probability of the windstorm counts using geometric and NB distributions
pgeom(range_gnb, p=param_geo$estimate,lower.tail = F)
pnbinom(range_gnb,mu = param_nb2$estimate[[2]], size = param_nb2$estimate[[1]],lower.tail = F)

#Prints a paragraph discussing the suitability of the distributions for the data
print("These distributions are farily similar to their fit to the data, though the low amount of counts makes it hard for\n
       this data work well with any distribution, there is not enough spread. The negative binomal distrubition may be better\n
       suited to deal with the large spike of single occurances.")

#Prints a paragraph discussing the probabilities
print("The parameter p is the maximum likelyhood that the storm will be around for another day, if it will persist. As each day\n
       passes the likelyhood will reduce, given the law of multiplicity, but never exceed the maximum likelyhood. The persistance\n
       measured through the geometric distribution is more worrying since it shows a higher likelyhood of a storm persisting.")