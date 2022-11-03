# Map using tmap 
# Data: percentage of current smokers, Scottish Health Survey, 2019 (Scottish Government)

library(dplyr)
library(tidyverse)
library(sp)
library(tmap)
library(sf)

################################################################
# read in smoking prevalence file that has health board codes

smokers <- read_csv("scottish-health-survey-local-area-level-data.csv")

smokers <- smokers %>% 
  rename("hb2019name" = "Reference Area")
#################################################################
  
# get HB codes from a lookup file 

hb_lookup <- read_csv("Datazone2011lookup.csv") %>% 
  select(hb2019, hb2019name)

# keep one row of each

hb_lookup <- hb_lookup %>% 
  distinct(hb2019, hb2019name)

# join HB names to codes

smokers_hb <- smokers %>% 
  inner_join(hb_lookup, by = "hb2019name")

#rename 'All' to rate as this will show in the map

smokers_hb <- smokers_hb %>% 
  rename("Rate" = "All")

############################################################# 

#Read in shapefiles

#use sf package                                                           
shapeData <- st_read("SG_NHS_HealthBoards_2019.shp")
#No need to convert to a dataframe.                                                            

summary(shapeData) #now its a SPatial polygons dataframe

#############################################################
# Part 2 - Map smokers
############################################################

#join shapefile and hb file - use sp's merge

shp_hb <- sp::merge(shapeData, smokers_hb, by.x = "HBCode", by.y ="hb2019", duplicateGeoms = T) #this bc non-unqiue matches
names(shp_hb)

summary(shp_hb)

#Object of class SpatialPolygonsDataFrame so it has a dataframe
# lets see what it has

third <- shp_hb@data[[3]]  #access the third element of shp_hb, in slot "data"
print(third) #these are HB names.

#######################################################
# Map with tmap 

tm_shape(shp_hb) +
  tm_fill(col = "Rate") +
  tm_text("HBName", size = 0.5, col = "white", remove.overlap = TRUE) + #put text before fill so not under
  tm_borders() +
  tmap_style("cobalt") +
  tm_layout(legend.position = c("left", "top"), title= '% current smokers in Health Boards', 
            title.position = c("left", "top")) +
  tm_credits("Scottish Health Survey, 2019", position=c("left", "bottom"))

# Save map

tmap_save(filename = "SmokersHBs.jpg")

