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
occdata<-subset(occdata, lat<20) # removed point from N america

dups <- duplicated(occdata[, c('lon', 'lat')])
#This identifies observations that have already appeared above
sum(dups)

#remove duplicates
occ <- occdata[!dups, ]

# download worldclim data
output_dir<-"~/Documents/WorkingD/BUP/Species distribution modelling"

bio_glob<-worldclim_global(var="bio", res=10,path=output_dir, version="2.1")

dim(bio_glob)

#we will also clip the spatraster so it only covers the spatial extent of our study species. First its longitudes then latitudes
summary(occ$lon)
summary(occ$lat)

e <- ext(-90, -30, -55,20) #min/max lon/lat values


predictors <- crop(bio_glob, e)

names(predictors)<-substring(names(predictors),11,16)
#here we're just shortening the names of predictors by taking the 11th to 16th characters.

#plot first 9 climate variables
plot(predictors,1:9)

# plot climate data for first variable
plot(predictors,1)
points(occ$lon,occ$lat, col='firebrick1',pch=16,cex=0.2)

#here I'm setting the spatial extent to be broadly consistent with that of my study species (you need to make sure it is sampling from the same extent). Remember to find out how a function works you can do ?function
bg<-spatSample(predictors,5000,"random", na.rm=TRUE, as.points=TRUE,ext=e)

#Here we'll plot our background points on a map of climwin variable 1 (you could change this to any of the worldclim variables)
plot(predictors, 1)
points(bg, cex=0.1, col='lightsalmon')


