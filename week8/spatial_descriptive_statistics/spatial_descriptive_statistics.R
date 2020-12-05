library(highcharter)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(raster)
library(downloader)
library(rgdal)

LondonWards <- st_read(here::here("prac8_data", 
                                  "New_ward_data",
                                  "NewLondonWard.shp"))
extradata <- read_csv(here::here("prac8_data", "LondonAdditionalDataFixed.csv"))
LondonWardsleftjoin <- LondonWards %>%
  left_join(.,extradata,
            by = c("WD11CD" = "Wardcode"))
#check which variables are numeric first

Datatypelist <- LondonWardsleftjoin %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

#make groups based on types of variables
Groups <- LondonWardsleftjoin %>% 
  st_drop_geometry()%>%
  dplyr::select(is.numeric)%>%
  pivot_longer(everything(),
               names_to="All_variables", 
               values_to="val")%>%
  mutate(All_variables = tolower(All_variables))%>%
  mutate(group = case_when(str_detect(All_variables, "age") ~ "Age",
                           str_detect(All_variables, "employ|income|job|jsa") ~ "Employment",
                           str_detect(All_variables,
                                      "house|rent|detatched|flat|terrace|owned|social|private|share|tax|mortgage") ~ "Housing", TRUE~"Other"))

Agehist <- Groups%>%
  filter(group=="Age")%>%
  ggplot(., aes(x=val)) + 
  geom_histogram(aes(x = val, y = ..density..))+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')

