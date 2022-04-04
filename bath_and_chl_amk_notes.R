# SPATIAL UNIT WOOOOO 
# notes 


#raster brick or raster stack 

#data product levels tells you how processed the data is 
# L3 is when it has binned nicely to be used in a raster


#install.packages("mapdata")


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



## 2022-03-31
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

bath_m_raw = marmap::getNOAA.bathy(lon1=lon_bounds[1],
                                   lon2 = lon_bounds[2],
                                   lat1=lat_bounds[1],
                                   lat2=lat_bounds[2],
                                   resolution=4)
class(bath_m_raw)
bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)


bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>%
  dplyr::select(-z)

summary(bath_m)
library(scales)

GOM_bath_map = ggplot()+
  geom_raster(data = bath_m, aes(x=x, y=y, fill=depth_m), ) + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),  fill="darkgrey", color=NA)+
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand=FALSE) +
  scale_fill_gradientn(colors=c("black", "darkblue", "lightblue"),
                       values=scales::rescale(c(-6000, -300, 0)))
ggsave(GOM_bath_map, file = "figures/GOM_bath_map.png")



GOM_bath_map2 = ggplot()+
  #geom_raster(data = bath_m, aes(x=x, y=y, fill=depth_m)) + 
  geom_contour(data=bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-100), size=0.25)+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="darkgrey", color=NA)+
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand=FALSE) +
  scale_fill_gradientn(colors=c("black", "darkblue", "lightblue"),
                       values=scales::rescale(c(-6000, -300, 0)))
ggsave(GOM_bath_map2, file = "figures/GOM_bath_map2.png")



#combine chl an dbath rasters

bath_m_raster=marmap::as.raster(bath_m_raw)
chl_GOM_raster
bath_m_raster 

#resampling 
names(bath_m_raster)="bath_m"

bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster)
raster_stack = stack(chl_GOM_raster, bath_layer_chl_dims)
plot(raster_stack)


stack_df = data.frame( raster::rasterToPoints(raster_stack))
head(stack_df)


#O'Reilley 2019
oligo_chl_a = 0.1
eutro_chl_a = 1.67  #mg/m^3

stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a < oligo_chl_a ~ "oligotrophic",
                                   chl_a >= oligo_chl_a & chl_a <= eutro_chl_a ~ "mesotrophic",
                                   chl_a > eutro_chl_a ~ "eutrophic")) %>%
  mutate(trophic_index = as.factor(trophic_index))

head(stack_df)
summary(stack_df)
table(stack_df$trophic_index)

ggplot()+
  geom_histogram(aes(x=bath_m), data=stack_df)+
  facet_wrap(~trophic_index)

#map the trophic index

trophic_map = ggplot()+
  geom_raster(data = stack_df, aes(x=x, y=y, fill=trophic_index))+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="darkgrey", color=NA)+
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand=FALSE) 
ggsave(trophic_map, file ="figures/troph_map.png")
#yayyyyyy it worked 




