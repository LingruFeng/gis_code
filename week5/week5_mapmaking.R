##Load all our data
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)


## read in all the spatial data and 
## reproject it 
OSM <- st_read(here::here("greater-london-latest-free.shp", 
                          "gis_osm_pois_a_free_1.shp")) %>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')

Worldcities <- st_read(here::here("World_Cities", 
                                  "a4013257-88d6-4a68-b916-234180811b2d202034-1-1fw6kym.nqo.shp")) %>%
  st_transform(., 27700)

UK_outline <- st_read(here::here("gadm36_GBR_gpkg", "gadm36_GBR.gpkg"),layer = 'gadm36_GBR_0') %>%
  st_transform(., 27700)

#London Borough data is already in 277000
Londonborough <- st_read(here::here("statistical-gis-boundaries-london", 
                                    "statistical-gis-boundaries-london",
                                    "ESRI", 
                                    "London_Borough_Excluding_MHW.shp"))%>%
  st_transform(., 27700)


## read in the .csv
## and make it into spatial data
Airbnb <- read_csv("listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')


# make a function for the join
# functions are covered in practical 7
# but see if you can work out what is going on
# hint all you have to do is replace data1 and data2
# with the data you want to use

Joinfun <- function(data1, data2){
  output<- data1 %>%
    st_join(data2,.)%>%
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}

# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)

# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)

Worldcities2 <- Worldcities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='Birmingham'|
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Edinburgh')

newbb <- c(xmin=-296000, ymin=5408, xmax=655696, ymax=1000000)

UK_outlinecrop <- st_crop(UK_outline$geom, newbb)

Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

Airbnb <- Airbnb %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))




