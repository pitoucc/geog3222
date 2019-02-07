#Geog 3222 - Lab 4
#Karl Chiassson
#201255916

#Set working directory
setwd('D:\\Class\\geog_3222\\Labs\\Lab_4')

#Load data/scripts
load('Dataset3.Rdata')
source('grouping.ts.R')
source('plot.c.freq.R')
source('num.sum.funcs.R')

#Gets the maximum wind speed for each year
annual_max_wind <- grouping.ts(data, period = 'Year', operation = 'max')$WindSpd

#Creates a pdf which contains a histrogram of the yearly maxima data
pdf('histogram_of_wind_yearly_maxima.pdf')
hist(annual_max_wind, xlab="Wind Speed (km/h)", xlim=c(40,100), breaks=20, main="Histogram of Annual Wind Maxima ")
dev.off()

#Creates a pdf which contains a cummulative frequency of the yearly maxima data
pdf('cumulative_frequency_of_wind_yearly_maxima.pdf')
plot.c.freq(annual_max_wind, xlab="Wind Speed (km/h)", main="Cumulative Frequency of Annual Wind Maxima")
dev.off()

#Calculates the mean, median, tri-mean, standard deviation, media absolute deviation, IQR, skewness and Yule-Kendall index
mean(annual_max_wind)
median(annual_max_wind)
tri.mean(annual_max_wind)
sd(annual_max_wind)
medianAbDev(annual_max_wind)
IQR(annual_max_wind)
skewness(annual_max_wind)
YKi(annual_max_wind)

#Prints a paragraph comparing the plots and the numerical summary statistics
print("filler")

#Creates five random samples of yearly wind maxima
list_of_vec <- vector("list",5)
n <- c(1:5)
for(i in n){
  list_of_vec[[i]] <-sample(annual_max_wind,size=15)
}

#Calculates the mean, median, tri-mean, standard deviation, media absolute deviation, IQR, skewness and Yule-Kendall index for each subsample
sub_mean <- vector(,length(list_of_vec))
sub_median <- vector(,length(list_of_vec))
sub_tri <- vector(,length(list_of_vec))
sub_sd <- vector(,length(list_of_vec))
sub_mabDev <- vector(,length(list_of_vec))
sub_iqr <- vector(,length(list_of_vec))
sub_skew <- vector(,length(list_of_vec))
sub_yki <- vector(,length(list_of_vec))

for(i in 1:length(list_of_vec)){
  sub_mean[i] <- mean(list_of_vec[[i]])
  sub_median[i] <- median(list_of_vec[[i]])
  sub_tri[i] <- tri.mean(list_of_vec[[i]])
  sub_sd[i] <- sd(list_of_vec[[i]])
  sub_mabDev[i] <- medianAbDev(list_of_vec[[i]])
  sub_iqr[i] <- IQR(list_of_vec[[i]])
  sub_skew[i] <- skewness(list_of_vec[[i]])
  sub_yki[i] <- YKi(list_of_vec[[i]])
}

#Reports the range of subsample data with comments on agreement and resistance of location measure, spread measures and spread measures
cat(" Mean range:", range(sub_mean),"\n",
      "Meidan range:", range(sub_median),"\n",
      "Tri-mean range:", range(sub_tri),"\n",
      "Standard deviation range:", range(sub_sd),"\n",
      "Median absolue deviation range:", range(sub_mabDev),"\n",
      "IQR range:", range(sub_iqr),"\n",
      "Skewness range:", range(sub_skew),"\n",
      "Yull-Kendall Indec range:", range(sub_yki),"\n")
                    
print("filler")

#Prints a paragraph commenting on the uncertainty for the statistics calculated for the annual wind speed maxima data set
print("The statics calculated for the annual wind speed maxima could vary depending on the performance or the variation in the accuracy
      and percision of equipment. For example, a piece of electronics will operate differently depending on the temperature to which it is exposed
      which may lead to changes in the data it collects")

#Prints a paragraph commenting on the differences between the calcualted statistics and that of Toronto
cat("Given the statistical data for Toronto:",
    "\n Mean   = 66.71 km/h",
    "\n Median = 67 km/hr",
    "\n Trimean  = 66.69 km/hr",
    "\n Std Dev = 5.97 km/hr",
    "\n IQR  = 6.75 km/hr",
    "\n MAD  = 4.00 km/hr",
    "\n Skewness = 0.34",
    "\n Yule-Kendall = -0.19",
    "\n\nand the statistical date for Site 3:",
    "\n Mean =", mean(annual_max_wind),"km/hr", 
    "\n Median = ", median(annual_max_wind),"km/hr",
    "\n Trimean = ",tri.mean(annual_max_wind),"km/hr",
    "\n Std Dev = ",sd(annual_max_wind),"km/hr",
    "\n IQR = ",medianAbDev(annual_max_wind),"km/hr",
    "\n MAD = ",IQR(annual_max_wind),"km/hr", 
    "\n Skewness = ",skewness(annual_max_wind),
    "\n Yule-Kendall = ",YKi(annual_max_wind),
    "\n\n ")