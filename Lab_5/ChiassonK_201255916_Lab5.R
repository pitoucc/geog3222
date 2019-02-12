#Geog3222 - Lab5
#Karl Chiasson
#201255916

#Set the working directory
setwd('D:\\Class\\geog_3222\\Labs\\Lab_5')

#Load data/code
load('Dataset3.Rdata')
NL.data <- data
load('Toronto_Reference.Rdata')
TO.data <- data
source('calc.anomalies.R')
source('grouping.ts.R')

#Groups daily means for data
nl_mean_daily <- grouping.ts(NL.data,period = 'Day',operation = 'mean')
to_mean_daily <- grouping.ts(TO.data,period = 'Day',operation = 'mean')
nl_mean_daily <- nl_mean_daily[which((nl_mean_daily$Year >= 1980) & (nl_mean_daily$Year <= 2010)),]
to_mean_daily <- to_mean_daily[which((to_mean_daily$Year >= 1980) & (to_mean_daily$Year <= 2010)),]

#Calculates the correlation of the NL & Toroto daily mean temperatures
cor(nl_mean_daily$Temp, to_mean_daily$Temp)

#Prints a report about the correlation of the data
print("filler")

#Calculates the anomalies of the temperatures of NL and Toronto
long_term_mean <- mean(nl_mean_daily$Temp)
nl_daily_anom <- nl_mean_daily$Temp - long_term_mean 
to_daily_anom <- to_mean_daily$Temp - long_term_mean

#Creates a scatterplot of NL vs Toronto temp and saves it as a pdf
pdf('Scatterplot_NLvsTO_daily_mean_temp.pdf')
plot(nl_mean_daily$Temp,to_mean_daily$Temp,xlim=c(-30,30), ylim=c(-30,30))
abline(lm(to_mean_daily$Temp~nl_mean_daily$Temp), col="red")
dev.off()

#Recalculates the agreement between NL & Toronto temps as a correlation of the anomalies
cor(nl_daily_anom,to_daily_anom)


#Reports on the corelation of anomalies
print("filler")

#Uses cal.anomalies on NL & Toronto data
nl_daily_calcanom <- calc.anomalies(nl_mean_daily)
to_daily_calcanom <- calc.anomalies(to_mean_daily)

#Examine the structure of the data from cal.anomalies
str(nl_daily_calcanom)
str(to_daily_calcanom)
print("filler")

#Creates a scatterplot of the temp. anomalies from cal.anomalies, saves as pdf
pdf('Scatterplot_NLvsTO_daily_temp_anom.pdf')
plot(nl_daily_calcanom[[3]]$Temp,to_daily_calcanom[[3]]$Temp, xlim=c(-30,30), ylim=c(-30,30))
abline(lm(to_daily_calcanom[[3]]$Temp~nl_daily_calcanom[[3]]$Temp), col="red")
dev.off()

#Recalculates the agreement between NL & Toronto as a correlation of calc.anomalies
cor(nl_daily_calcanom[[3]]$Temp,to_daily_calcanom[[3]]$Temp)

#Prints a report about the correlation of anomalies
print("filler")

#Creates a plot about original first year data
year_length <- length(nl_mean_daily[which(nl_mean_daily$Year == 1980),]$Day)
pdf('1980_30y_mean_temp_anomalies_plot.pdf')
plot(1:year_length, head(nl_daily_anom, year_length), type = 'l', col = 'red', xlim = c(1,370),ylim = c(-25,20), xlab='Day', ylab='Temp Anomaly',main='1980 Temperature Anomalies (30 Year Mean)')
lines(1:year_length, head(to_daily_anom, year_length), type = 'l', col = 'blue')
dev.off()

#Creates a plot about calc.anomalies data
pdf('1980_calcanom_temp_anomalies_plot.pdf')
plot(1:year_length, head(nl_daily_calcanom[[3]]$Temp,year_length), type = 'l', col = 'red', xlim = c(1,370),ylim = c(-20,15),xlab='Day', ylab='Temp Anomaly',main='1980 Temperature Anomalies (Calc.Anomalies)')
lines(1:year_length, head(to_daily_calcanom[[3]]$Temp,year_length), col = 'blue')
dev.off()

#Creates a plot about calc.anomalies annual.mean
pdf('1980_calanom_temp_mean_plot.pdf')
plot(1:year_length, head(nl_daily_calcanom[[1]]$Temp,year_length), type = 'l', col = 'red', xlim = c(1,370),ylim = c(-10,25),xlab='Day', ylab='Temp', main='1980 Mean Temperatures')
lines(1:year_length, head(to_daily_calcanom[[1]]$Temp,year_length), col = 'blue')
dev.off()

#Prints a paragraph about the three correlations and their results
print('filler')

#Prints a paragraph that compares anomalies to standard anomalies
print('filler')