install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
library(geodata)
library(terra)

# data preparation
#download ring ouzel and climate data from Learn
avi_dat <- read.table('~/Documents/WorkingD/BUP/Species distribution modelling/Data_SwissBreedingBirds copy.csv', header=T, sep=',')
nrow(avi_dat)
summary(avi_dat)

# Subset the data to the columns we will be working with
ouzel_cols <- c('Turdus_torquatus', 'bio_5', 'bio_2', 'bio_14', 'blockCV_tile')

ouzel_df <- data.frame(avi_dat)[ouzel_cols]

summary(ouzel_df)

# Enter working directory for this data, download current and future climate data for switzerland
output_dir<-"~/Documents/WorkingD/BUP/Species distribution modelling"

bio_curr <-worldclim_country("Switzerland",version="2.1", var='bio', res=10, lon=5.5, lat=45.5, path=output_dir)[[c(2,5,14)]]

bio_fut <- cmip6_world(var = "bio", model = "CNRM-CM6-1-HR", ssp = "245", res = 10,  time = "2041-2060",  lon = c(5.96, 10.49),  lat = c(45.82, 47.81),path=output_dir)[[c(2,5,14)]]


# A spatial mask of Switzerland in Swiss coordinates
bg <- rast('/vsicurl/https://damariszurell.github.io/SDM-Intro/CH_mask.tif')

bio_curr <- terra::project(bio_curr, bg)
bio_fut <- terra::project(bio_fut, bg)
#we need to change the projection of our climate data to match that of the bg file.

bio_curr <- terra::resample(bio_curr, bg)
bio_fut <- terra::resample(bio_fut, bg)
#we then need to make the resolution equivalent to bg. 


bio_curr <- terra::mask(bio_curr, bg)
bio_fut <- terra::mask(bio_fut, bg)
#we then need to clip the extent to match an outline of Switzerland

names(bio_curr) <- c('bio_2', 'bio_5', 'bio_14')
names(bio_fut) <- c('bio_2', 'bio_5', 'bio_14')

# plot the climate variables and their projected change
plot(bio_curr) # current
plot(bio_fut) # future

## MODEL FITTING
# Can you code a binomial GLM with all three predictors fitted as linear and squared terms?

model <- glm( Turdus_torquatus ~ bio_2 + I(bio_2^2) + bio_5 + I(bio_5^2) + bio_14 + I(bio_14^2), family='binomial', data=ouzel_df)

summary(model) # bio 5 and 4 has a strong effect from the summary

