## Matrix population modelling

# download and load packages
rm(list=ls(all=TRUE))

my_packages <- c('ggplot2', 'popbio')
new_packages <- my_packages[!(my_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

library(ggplot2)
library(popbio)

## Parameterising your Matrix Population Model (MPM)
# Stage-specific survival:

#plot survival rates
# first enter the values into a dataframe 
survival <- data.frame(stage=factor(c('Juvenile','Yearling','Adult'), levels=c('Juvenile','Yearling','Adult')), estimate=c(0.463, 0.510, 0.559), lcl=c(0.404, 0.445, 0.499), ucl=c(0.524, 0.574, 0.618))

# then plot by stage
ggplot(survival, aes(stage, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0.2) + geom_point() + ylim(0,1)

## QUESTION: Which stage has the lowest survival rate? Is this what you would expect?
# Juvenile has the lowest, this is expected as they're more vulnerable on average to predators and other external factors

#Per capita reproduction:

#estimate the number of female offspring produced by each female between censuses (using nest visitation data)
nestdata <- read.table("~/Documents/WorkingD/BUP/Population Dynamics/MPM practical/gjeroynest.txt", header = TRUE, sep = '\t')
head(nestdata)
#clutchno indicates whether it was the first, second or third etc. clutch laid in that nest in that breeding season.
#hatchingsuc indicates whether any live chicks were found for that clutch (yes = 1; no = 0).
#chickno indicates the number of chicks counted on the final visit prior to fledging.

##QUESTION: How might you estimate per capita reproduction from these data?
#calculate the average hatching success and average number of chicks using the mean() function
HatchingSuc <- mean(nestdata$hatchingsuc)
FledglingNo <- mean(nestdata$chickno[nestdata$hatchingsuc==1])

#data frame has a row for each unique nest, column for max number of clutchno for each unique value of nestid
nests <- data.frame(nestid = sort(unique(nestdata$nestid)), numberofclutches=tapply(nestdata$clutchno, nestdata$nestid, max))
#then take the mean to be average number of clutches
ClutchNo <- mean(nests$numberofclutches)

#use these estimates to calculate per capita reproduction (Note! The mean number of chicks prior to fledging is an upwardly biased estimate of the number of fledglings, since not all will likely fledge the nest successfully)
#multiply estimates and divide by 2 as we're only modelling the female segment of the pop
(ClutchNo * HatchingSuc * FledglingNo) / 2 #Our estimate of per capita reproduction R is 1.076


## Deterministic population model
