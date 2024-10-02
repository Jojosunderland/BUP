install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
library(geodata)
library(terra)

avi_dat <- read.table('~/Documents/WorkingD/BUP/Species distribution modelling/Data_SwissBreedingBirds copy.csv', header=T, sep=',')
nrow(avi_dat)
summary(avi_dat)

# Subset the data to the columns we will be working with
ouzel_cols <- c('Turdus_torquatus', 'bio_5', 'bio_2', 'bio_14', 'blockCV_tile')

ouzel_df <- data.frame(avi_dat)[ouzel_cols]

summary(ouzel_df)

# Enter working directory for this data, download climate data
output_dir<-"~/Documents/WorkingD/BUP/Species distribution modelling"

bio_curr <-worldclim_country("Switzerland",version="2.1", var='bio', res=10, lon=5.5, lat=45.5, path=output_dir)[[c(2,5,14)]]

bio_fut <- cmip6_world(var = "bio", model = "CNRM-CM6-1-HR", ssp = "245", res = 10,  time = "2041-2060",  lon = c(5.96, 10.49),  lat = c(45.82, 47.81),path=output_dir)[[c(2,5,14)]]



