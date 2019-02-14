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
daily_temp_cor <- cor(nl_mean_daily$Temp, to_mean_daily$Temp)
daily_temp_cor

#Prints a report about the correlation of the data
paste("The high correlation of", daily_temp_cor, " shows that the temperature values are similar. This may be due to the each location experiencing the similar seasonal effects and encountering weather systems which may not have changed between locations. These locations are also only seperated by ~4 degrees of lattitude with no major geographical obticals seperating them. Futhermore, the differences may be attributed to the proximity of NL to water." )

#Calculates the anomalies of the temperatures of NL and Toronto
long_term_mean_nl <- mean(nl_mean_daily$Temp)
long_term_mean_to <- mean(to_mean_daily$Temp)
nl_daily_anom <- nl_mean_daily$Temp - long_term_mean_nl 
to_daily_anom <- to_mean_daily$Temp - long_term_mean_to

#Creates a scatterplot of NL vs Toronto temp and saves it as a pdf
pdf('Scatterplot_NLvsTO_daily_mean_temp.pdf')
plot(nl_mean_daily$Temp,to_mean_daily$Temp,xlim=c(-30,30), ylim=c(-30,30))
abline(lm(to_mean_daily$Temp~nl_mean_daily$Temp), col="red")
dev.off()

#Recalculates the agreement between NL & Toronto temps as a correlation of the anomalies
longterm_mean_anom_cor <- cor(nl_daily_anom,to_daily_anom)
longterm_mean_anom_cor

#Reports on the corelation of anomalies
paste("The corrilation of the anomalies:",longterm_mean_anom_cor,"and the long term based anomalies did not change since the difference between values is a constant factor, this preserves the ratios between subsequent values")

#Uses cal.anomalies on NL & Toronto data
nl_daily_calcanom <- calc.anomalies(nl_mean_daily)
to_daily_calcanom <- calc.anomalies(to_mean_daily)

#Examine the structure of the data from cal.anomalies
str(nl_daily_calcanom)
str(to_daily_calcanom)
print("Calc.anomalies returned a list of data frames containing the mean, the standard devations, anomalies, and standard anomalies of the data")

#Creates a scatterplot of the temp. anomalies from cal.anomalies, saves as pdf
pdf('Scatterplot_NLvsTO_daily_temp_anom.pdf')
plot(nl_daily_calcanom[[3]]$Temp,to_daily_calcanom[[3]]$Temp, xlim=c(-30,30), ylim=c(-30,30))
abline(lm(to_daily_calcanom[[3]]$Temp~nl_daily_calcanom[[3]]$Temp), col="red")
dev.off()

#Recalculates the agreement between NL & Toronto as a correlation of calc.anomalies
cor(nl_daily_calcanom[[3]]$Temp,to_daily_calcanom[[3]]$Temp)

#Prints a report about the correlation of anomalies
print("This shows that there is a low corraltion between the anomalies between Toronto and Newfoundland. This may be due to regional weather effects and regional geographics characteristice. This also apppears to show that the corraltions based on standard deviation is 1 - anomalies based on mean, meaning the amount of mean disagreement is proportial to the agreement of their standard anomalies")

#Creates a plot about original first year data
year_length <- length(nl_mean_daily[which(nl_mean_daily$Year == 1980),]$Day)
pdf('1980_30y_mean_temp_anomalies_plot.pdf')
plot(1:year_length, head(nl_daily_anom, year_length), type = 'l', col = 'red', xlim = c(1,400),ylim = c(-25,20), xlab='Day', ylab='Temp Anomaly',main='1980 Temperature Anomalies (30 Year Mean)')
lines(1:year_length, head(to_daily_anom, year_length), type = 'l', col = 'blue')
legend(350, 20, legend=c("NL", "TO"),col=c("red", "blue"), lty=1:2, cex=0.8)
dev.off()

#Creates a plot about calc.anomalies data
pdf('1980_calcanom_temp_anomalies_plot.pdf')
plot(1:year_length, head(nl_daily_calcanom[[3]]$Temp,year_length), type = 'l', col = 'red', xlim = c(1,400),ylim = c(-20,15),xlab='Day', ylab='Temp Anomaly',main='1980 Temperature Anomalies (Calc.Anomalies)')
lines(1:year_length, head(to_daily_calcanom[[3]]$Temp,year_length), col = 'blue')
legend(350, 15, legend=c("NL", "TO"),col=c("red", "blue"), lty=1:2, cex=0.8)
dev.off()

#Creates a plot about calc.anomalies annual.mean
pdf('1980_calanom_temp_mean_plot.pdf')
plot(1:year_length, head(nl_daily_calcanom[[1]]$Temp,year_length), type = 'l', col = 'red', xlim = c(1,400),ylim = c(-10,25),xlab='Day', ylab='Temp', main='1980 Mean Temperatures')
lines(1:year_length, head(to_daily_calcanom[[1]]$Temp,year_length), col = 'blue')
legend(350, 25, legend=c("NL", "TO"),col=c("red", "blue"), lty=1:2, cex=0.8)
dev.off()

#Prints a paragraph about the three correlations and their results
print('The plot from part 19 has the mean value included as part of the anomalies since it follows the plot in 21, where as the graph in part 20 has the values normalized to remove the mean')

#Prints a paragraph that compares anomalies to standard anomalies
print('It would appear that Toronto has the lowest anomaly while Newfoundland has the highest anomaly based on the longterm mean, and the same result for their standard anomalies as well. Of these two locations, Toronto experience the most unusual anomalies.')