#Geog 3222 Lab 7
#Karl Chiasson
#201255916

#Set working directory
setwd("D:\\Class\\geog_3222\\Labs\\Lab_7")

#Load required scripts/data/libraries
library(MASS)
load("Dataset3.Rdata")
source("dual.qq.R")
source("grouping.ts.R")

#Prints a paragraph about which data set will fit to a normal distribution
cat("The monthly mean data should be closer to a normal distribution as it would be the mean values of the data tending towards a sampling",
    "\ndistribution of the mean")

#Extracts data into winter sets
tempDJF <-data[which((data$Month == 12 | data$Month <= 2) & !is.na(data$WindSpd) & data$WindSpd != 0),]
dailymean <-grouping.ts(tempDJF, period = "Day", operation = "mean")
monthlymean <-grouping.ts(tempDJF, period = "Month", operation = "mean")

#Makes a qq.plot for each data set
pdf("qqplots_winter_daily_mean_wind_speed_gaussian_weibull.pdf", width=10,height=5)
dual.qq(dailymean$WindSpd)
dev.off()

pdf("qqplots_winter_monthly_mean_wind_speed_gaussian_weibull.pdf", width=10,height=5)
dual.qq(monthlymean$WindSpd)
dev.off()

#Prints a paragraph comparing the qq.plots
print("As shown in the qq plots the monthly means have a distribution that appears to fit closer to the line of regression")

#Fits Gaussian and Weibull to each wind vector
param_daily_norm <-fitdistr(dailymean$WindSpd, densfun = "normal")
param_daily_wb <-fitdistr(dailymean$WindSpd, densfun = "weibull")
param_monthly_norm <-fitdistr(monthlymean$WindSpd, densfun = "normal")
param_monthly_wb <-fitdistr(monthlymean$WindSpd, densfun = "weibull")

#Reports the likelyhood of each fit
cat("Log likelihood:",
    "\n\t-Daily mean:",
    "\n\t\tGaussian:",param_daily_norm[[5]],
    "\n\t\tWeibull:",param_daily_wb[[4]],
    "\n\t-Montly mean:",
    "\n\t\tGaussian:",param_monthly_norm[[5]],
    "\n\t\tWeibull:",param_monthly_wb[[4]])

#Performs ks.test to each distribution and reports the p value of each.
unique_dm <-unique(dailymean$WindSpd)
unique_mm <-unique(monthlymean$WindSpd)

dailymean_gaussian_kstest <-ks.test(unique_dm,"pnorm",param_daily_norm[[1]][[1]],param_daily_norm[[1]][[2]] )
dailymean_weibull_kstest <-ks.test(unique_dm,"pweibull", param_daily_wb[[1]][[1]],param_daily_norm[[1]][[2]]) #The p value for this calculation is very small and might be rounded to zero by R
monthlymean_gaussian_kstest <-ks.test(unique_mm,"pnorm",param_monthly_norm[[1]][[1]],param_monthly_norm[[1]][[2]])
monthlymean_weibull_kstest <-ks.test(unique_mm,"pweibull",param_monthly_wb[[1]][[1]],param_monthly_wb[[1]][[2]])

#Prints a paragraph comparing ks.test results
cat("The ks tests show that between the daily distributions that the gaussian distribution is better fitting in comparison to the weibull.",
    "\nIn comparison, the monthly ks tests show the same thing, where the gaussian distribution is better fitting in comparison to the weibull distribution,",
    "\nbut also show that the ks tests of the monthly mean data has a better fit over the daily mean data. This is shown by the very small p values",
    "\nof the daily mean data,", dailymean_gaussian_kstest[[2]], "and",dailymean_weibull_kstest[[2]],"which have been calculated through the ks tests. Since their p values are close to 0,",
    "\nthey are less likely to fit the data. In comparison, the monthly mean data has much higher p values,", monthlymean_gaussian_kstest[[2]],"and",monthlymean_weibull_kstest[[2]], ", are closer to",
    "\n1 and show that they are better fitting. These results hold to the expectations stated")


#Prints paragraph summerizing results
cat("Based on the operations on the data and qq plots it would show that, the Central Limit Theorom holds true for the monthly mean data.",
    "\nSince CLT establishes that a properly normalized sum tends towards a gaussian/normal distribution even if the original components of the sum are",
    "\nnot a normal distribution, we can see in the comparison of daily and monthly means that as we use more samples in the monthly mean",
    "\nthe monthly means are more normally distributed in comparison to the daily means. This is shown throughout the operations completed on the data",
    "\nsuch as the p values for the ks tests on the daily and monthly means, where the daily means resulted in a p value of", dailymean_gaussian_kstest[[2]],
    "\nand the monthly means result in a p value of", monthlymean_gaussian_kstest[[2]], ". If the value is closer to 1 then it is a better fit to a gaussian distribuition",
    "\nfor which the monthly mean is a better fit. Examining the likelihood, the log likelihood of the daily means is", param_daily_norm[[5]],"and the log likelihood",
    "\nof the monthly means is", param_monthly_norm[[5]], ", for which largest value is better, we can see that the monthly means have the largest value",
    "\nindicating that the monthly means are more likely to fit a normal distribution. By examinition the qq plot of the daily and monthly means it shows",
    "\nthat the monthly means have better fit to the line of regression in comparison to the daily means in regards to a gaussian distribution. It is hard",
    "\nto determine this with the qq plots and line of regression alone and is the primary reason to perform other operations to make that determination.",
    "\nWhen these comparisons are made with the daily and monthly means for the Weibull distribution it will also show the montly means give a better fit",
    "\nto the Weibull distribution like the Gaussian counterparts.")
