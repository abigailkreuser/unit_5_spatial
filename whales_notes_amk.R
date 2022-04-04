#whales
#gettin ready for 2022-03-31

#vector data
library(sf) # simple features (spatial vector data) st_read, st_transform
# library(broom) # part of tidyverse
library(tidyverse)
library(mapdata)  # map_data
library(marmap) # getNOAA.bathy()


# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html
USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat','North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles
USA_crit_hab
USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) #crs="+proj=longlat +datum=WGS84")
