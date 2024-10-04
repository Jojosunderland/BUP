## Practical 2

#install packages
install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("predicts",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("terra",dependencies=TRUE,repos="https://cloud.r-project.org")

#load packages
library(geodata)
library(predicts)
library(terra)

# species chosen:3 toed sloth
# download species data
occdata <- geodata::sp_occurrence("Bradypus", "Linnaeus*", 
                                  geo=FALSE,removeZeros=TRUE,start=1,end=10000)

dim(occdata)

occdata[1:10,]

#plot the global distribution, make sure it fits with expectation
wrld <- world(path=".")
#this function gives us an outline of the world's political boundaries. Reminder, if ever you want to know more about an R function, you can write ?function.name, e.g., ?world
plot(wrld, xlim=c(-180,180), ylim=c(-80,80), col="light yellow", border="light gray")
# add the points
points(occdata$lon, occdata$lat, col='blue', pch=20)

## Cleaning up occurrence data


