setwd("F:/Data Science/BCAP/2.Fundamental of Business Statistic/Statistics and R/Week 3.Estimation and Hypothesis Testing/Project-1/")
getwd()
problem1= read.csv("Cold_Storage_Temp_Data.csv", header = TRUE)

#library

library(ggplot2)
library(dplyr)
library(lattice)

#general oversvation

attach(problem1)
summary(problem1)
str(problem1)
anyNA(problem1)
anyDuplicated(problem1)
dim(problem1)
head(problem1)
tail(problem1)
by(problem1,INDICES = Season,FUN = summary)

#2.	Find overall mean for the full year 
mean_year=mean(problem1$Temperature)
print(mean_year)

#3.	Find Standard Deviation for the full year 
sd_year=sd(problem1$Temperature)
print(sd_year)

#the probability of temperature having fallen below 2 C? 
temp_below_2C= pnorm(2, mean = mean_year, sd = sd_year)
print(temp_below_2C)

#the probability of temperature having gone above 4 C?
temp_above_4C= 1-pnorm(4, mean = mean_year, sd = sd_year)
print(temp_above_4C)

## Combined Probability for <2 and >4

TotalProb = temp_above_4C+temp_below_2C
print(TotalProb)

####the penalty for the AMC Company
if(TotalProb > 0.025 && TotalProb <= 0.05){ penalty = '10%'} else if(TotalProb > 0.5){penalty = '25%'  }else penalty = '0%'

print(penalty)

#problem 2

problem2 =read.csv("Cold_Storage_Mar2018.CSV", header = TRUE)

summary(problem2)
str(problem2)
anyNA(problem2)
anyDuplicated(problem2)
dim(problem2)
head(problem2)
tail(problem2)


problem2$Season=as.character(problem2$Season)
problem2$Month=as.character(problem2$Month)

# Density plot for sample distribution of Temperatures 

ggplot(data=problem2, aes(Temperature)) + geom_density() + geom_vline(xintercept = 3.9, col="blue")

##Z test 
## sample mean

s.mean = mean(problem2$Temperature)
print(s.mean)

#sample Sd

s.sd= sd(problem2$Temperature)
print(s.sd)

##mu
mu=3.9

#sample
sample.size=35
##z test
z.test=(s.mean-mu)/(s.sd/(sqrt(sample.size)))
print(z.test)   

#acceptable value for mean temperature and at alpha = 0.1 
df=34
z.critical=qnorm(1-0.1)
print(z.critical)
z.test>=z.critical
pvalue= pt(z.test,df,lower.tail = FALSE)
print(pvalue)
Actualt.confint=(1-pvalue)*100
print(Actualt.confint)

# t.test

x= problem2$Temperature
t.test(x,mu= 3.9,conf.level = 0.90,alternative = "greater")

