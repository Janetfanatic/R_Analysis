library(tidyverse)
library(ggplot2)
library(jsonlite)

suspension_coil <- read.csv('Suspension_Coil.csv') #import dataset
mechacar_mpg <- read.csv('MechaCar_mpg.csv') #import dataset

#MPG Regression
# Multiple Line Regression
lm(mpg ~ spoiler.angle + ground.clearance + AWD + vehicle.length,data=mechacar_mpg) #generate multiple linear regression model
# Statistical Metrics
summary(lm(mpg ~ spoiler.angle + ground.clearance + AWD + vehicle.length,data=mechacar_mpg)) #generate summary statistics


#Suspension Coil Summary
summarize_suspensioncoil <- suspension_coil %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), SD_PSI=sd(PSI), VAR_PSI=var(PSI), MAX_PSI=max(PSI), MIN_PSI=min(PSI))

summarize_grouped_suspensioncoil <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), SD_PSI=sd(PSI), VAR_PSI=var(PSI), MAX_PSI=max(PSI), MIN_PSI=min(PSI))
summarize_grouped_suspensioncoil


#Statistical Difference Model
#Suspension Coil t-test

t.test(suspension_coil$PSI, mu=1500) #compare sample versus population means

t.test(subset(suspension_coil, Manufacturing_Lot=="Lot1")$PSI, mu=1500) #compare subset sample versus population mean

t.test(subset(suspension_coil, Manufacturing_Lot=="Lot2")$PSI, mu=1500) #compare subset sample versus population mean

t.test(subset(suspension_coil, Manufacturing_Lot=="Lot3")$PSI, mu=1500) #compare subset sample versus population mean


