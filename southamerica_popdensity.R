library(openxlsx)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(st)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)
library(tidyverse)

#Read in the basemap

geolevel <- sf::read_sf("world_geolev1_2021.shp")

#list all countries and filter
countries <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
geo1_sa <- geolevel %>%  filter(CNTRY_NAME %in% countries)

# Now we need to get the latest data year for each geolevel1
df <- geo1_sa %>% 
  distinct(GEOLEVEL1) %>% 
  group_by(CNTRY_NAME) %>%
  summarise(n())

# Read in pre-filtered population density
popdensity <- read.csv("popdensity_sa.csv")

# get the latest data year for each geolevel1 
popdensity2 <- popdensity %>% 
              group_by(GEOLEV1) %>% 
              slice(which.max(YEAR))

# merge into shapefile by GEOLEVEL1

mapdata <- popdensity2 %>% 
          left_join(southamerica, by = c('GEOLEV1'= 'GEOLEVEL1'))
          
 # convert to numeric
 
southamerica$CNTRY_CODE <- as.numeric(southamerica$CNTRY_CODE)
southamerica$GEOLEVEL1<- as.numeric(southamerica$GEOLEVEL1)

# check the values in popdensity -
#POPDENSGEO1 is an 8-digit string variable listing the population density in persons per square kilometer.

range(mapdata$POPDENSGEO1)
summary(mapdata$POPDENSGEO1)
# median density is 42.1 

hist(mapdata$POPDENSGEO1)
# most regions have density <2000 which means we need to group/bin values to make mapping work

# make a new var bins for density
#can find quantiles with:
classIntervals(popdensity2[["POPDENSGEO1"]], n= 5, style = "quantile")
#and then use those values to break the data into bins:
mapdata2 <- mapdata %>% 
  mutate(densitybins = cut(POPDENSGEO1, breaks = c(0, 8, 28, 61, 160, 15000)))
  
#### MAP USING GGPLOT ####

#some country names are missing so omit rows with NAs

mapdata3 <- na.omit(mapdata2)

#create column to show area name and density value in tooltip
mapdata3 <- mapdata3 %>% 
  mutate(text = paste0(ADMIN_NAME, ', ', CNTRY_NAME, ', ', POPDENSGEO1))

# define colours for bins
colours = c("#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#AE017E")
 
# static map
p <- ggplot() +                            
    geom_sf(data = mapdata3$geometry, 
            linewidth = 0.5,
            aes(fill = mapdata3$densitybins)) +
    scale_fill_manual(values = colours) +
    theme_void() +
    labs(fill = "Population/km2")
p 
ggsave("popdensity_southamerica_ggplot.png")

#### MAP USING LEAFLET ####

# leaflet complains about a grouped dataframe so convert it back to an sf object(?)

df <- sf::st_as_sf(mapdata3)
str(df)

# Create a palette that maps bins to colors
pal <- colorFactor("RdPu", domain = df2$densitybins) 

      
leaflet::leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%  
  setView(lng = -64.0, lat = -34.0, zoom = 3) %>%
  addPolygons(data = df, color = pal(df$densitybins) , stroke = 0.1, opacity = 0.7,
              popup = paste0("<b>Country: </b>" , df2$CNTRY_NAME, ', ',
              "<b>Region: </b>", df2$ADMIN_NAME,
              ', ', df2$POPDENSGEO1," people/km2")) %>% 
  addLegend("bottomright", pal = pal, values = df$densitybins,
            title = "Population/km2",
            opacity = 1)
