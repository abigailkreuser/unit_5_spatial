# SPATIAL UNIT WOOOOO 
# notes 


#raster brick or raster stack 

#data product levels tells you how processed the data is 
# L3 is when it has binned nicely to be used in a raster


install.packages("mapdata")


library(tidyverse)
library(raster)
library(marmap)
library(mapdata)
library(ggplot2)

chl_raster = raster('data/A20021822017212.L3m_MC_CHL_chlor_a_9km.nc')
names(chl_raster) = "chl_a" 
chl_pts = rasterToPoints(chl_raster, spatial = TRUE)

class(chl_pts)
chl_df = data.frame(chl_pts)
head(chl_df)


#how to pick some colors
hist(chl_df$chl_a)
# this is why clorophyll is plotted on a log scale
# normally its pretty low but in some areas there is alot 

hist(log10(chl_df$chl_a)) #shows a more even distribtion
# there is a NASA color scheme 
cols = rainbow(7, rev=TRUE) #doing RGBIF backwards!
#NASA doesn't really use purple, so now take off violet 
cols = cols[-1]
cols


#image our chloro

global_chl_map = ggplot() + 
  geom_raster(data=chl_df, aes(x=x, y=y, fill=log_10(chl_a))) + #wouuld change col names to be long and lat instead xy
  scale_fill_gradientn(colors = cols, limits=c(-1.5, 0.75), name="log_10(chl_a)") + #limits tells you how you want the colors to correspond with values
ggsave(global_chl_map, filename="figures/global_chl_map.png", height = 5, width = 9)


### crop to Gulf of Maine
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))# read doc for extent
chl_GOM_df = data.frame(rasterToPoints(chl_GOM_raster, spatial = TRUE))

head(chl_GOM_df)                        
chl_GOM_df = chl_GOM_df %>%
  dplyr::select(-optional)

world_map = map_data("worldHires")

GOM_chl_map = ggplot() + 
  geom_raster(data=chl_GOM_df, aes(x=x, y=y, fill=log10(chl_a))) + 
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill="darkgrey")+
  coord_fixed(1.3, xlim=lon_bounds, ylim = lat_bounds)
ggsave(GOM_chl_map, file = "figures/GOM_chl_map.png")

#quick fix for x axis stretching in this region 
# need to re up these notes 
# maybe redo on work computer