#-----------------------------------------------------------------------------------#
# Pre-lab Exercise; Lab 4
# Add comments to the following script, explaining what each line of code DOES. 
#-----------------------------------------------------------------------------------#
load("Dataset1.Rdata")
source("grouping.ts.R")

mn.data <- grouping.ts(data, period = "Year", operation = "mean")
mean.temps <- mn.data$Temp

nyrs <- length(mean.temps)

i1 <- sample(c(1:nyrs), 5, replace = FALSE)
i2 <- sample(c(1:nyrs), 5, replace = FALSE)

sample1 <- mean.temps[i1]
sample2 <- mean.temps[i2]

smp.mns <- mean(sample1, na.rm = TRUE)
smp.mns[2] <- mean(sample2, na.rm = TRUE)

max(smp.mns) - min(smp.mns)

smp.sd <- sd(sample1, na.rm = TRUE)
smp.sd[2] <- sd(sample2, na.rm = TRUE)

max(smp.sd) - min(smp.sd)
