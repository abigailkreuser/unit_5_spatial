#whales
#gettin ready for 2022-03-31

#vector data

library(tidyverse)
library(mapdata)  # map_data hi res coastline
library(marmap) # getNOAA.bathy()
library(sf) # simple features (spatial vector data) st_read, st_transform
# library(broom) # part of tidyverse
library(lubridate)


USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat','North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles
USA_crit_hab
# its a simple feature 
# fields are meta data 
#if CRS is 4269- that is NAD83- which is the US survey in sometime '83
#WGS84 is 4326 - that is a european gas company survey and what canada polygons are in

USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) #crs="+proj=longlat +datum=WGS84")
#transformed to the european CRS

#load in canada data
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)
names(CAN_crit_hab) <- c("lat", "lon", "habitat", "country")

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
  dplyr::group_by(habitat,country) %>%
  dplyr::summarize(do_union=FALSE) %>% #helps draw the polygon correctly #look at summarise.sf
  st_cast("POLYGON")

#im confused about the summarise.sf and not dplyr or is it how dplyer summarize works in sf

#join canada and usa
plot(USA_crit_hab_sf$geometry[1])

USA_crit_hab_sf$habitat = c("GOM", "SEUS")
USA_crit_hab_sf$country = "USA"


USA_crit_hab_sf = USA_crit_hab_sf %>%
  dplyr::select(habitat, country, geometry)

crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)


lat_bounds= c(39, 53)
lon_bounds= c(-72, -54)

world_map= map_data("worldHires", ylim=lat_bounds, xlim=lon_bounds)

crit_map= ggplot()+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="black")+
  geom_sf(data=crit_hab, aes(fill=country), alpha=0.5)+
  coord_sf(1.3, xlim=lon_bounds, ylim=lat_bounds)+
  theme_classic()

ggsave(crit_map, file = 'figures/crit_map4.pdf', height=5, width=9)


# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')

#plot points on our map

world_map= map_data("worldHires", ylim=lat_bounds, xlim=lon_bounds)

crit_map_w_carc= ggplot()+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="black")+
  geom_sf(data=crit_hab, aes(fill=country), alpha=0.5)+
  geom_point(data=carcass, aes(x=Longitude, y=Latitude, color = Carcass_position), size=2)+
  coord_sf(1.3, xlim=lon_bounds, ylim=lat_bounds)+
  theme_classic()

ggsave(crit_map_w_carc, file = 'figures/crit_map_w_carc.pdf', height=5, width=9)



# now for a story about the SE

# read in AIS data
ais = read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais)

Sys.time()
ships_RW_intersect = ais %>%
  st_as_sf(coords=c("LON", "LAT"), crs=4269) %>%
  st_intersection(USA_crit_hab %>% dplyr::select(geometry))
Sys.time()



law_breakers = ships_RW_intersect %>% 
  filter(Length > 20, 
         SOG > 10)

head(law_breakers)
dim(law_breakers)
summary(law_breakers)

length(unique(law_breakers$CallSign)) # how many law breakers?
unique(law_breakers$VesselName) # what are their names?


# create ship tracks (lines)

illegal_paths = law_breakers %>%
  dplyr::mutate(date_time = lubridate::ymd_hms(BaseDateTime))
head(illegal_paths)
#now R can read the time


illegal_paths = law_breakers %>%
  dplyr::mutate(date_time = lubridate::ymd_hms(BaseDateTime)) %>%
  arrange(date_time) %>%
  group_by(CallSign) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_make_valid()


head(illegal_paths)

lon_bounds = c(-82, -76)
lat_bounds = c(25, 34)

law_breaking_map = ggplot()+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="black", color=NA)+
  geom_sf(data=USA_crit_hab, alpha=0.5, fill="yellow")+
  geom_sf(data=illegal_paths, aes(color=CallSign)) +
  coord_sf(1.3, xlim=lon_bounds, ylim=lat_bounds)+
  theme_classic()


ggsave(law_breaking_map, file="figures/law_breaking_map.pdf", height = 9, width = 5)  


illegal_path_lengths = illegal_paths %>%
  mutate(track_length_m = st_length(geometry))

class(illegal_path_lengths$track_length_m)
typeof(illegal_path_lengths$track_length_m) # keeps track of units could get into a bind use as.numeric
head(illegal_path_lengths)


sum(illegal_path_lengths$track_length_m)

