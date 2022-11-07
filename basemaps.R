#Playing with basemaps
setwd("C:/Users/heidij01/OneDrive - NHS Scotland/Documents/Mappy maps")

library(ggmap)
library(sf)

scotland <- c(lon = -3.188267 , lat = 55 )

scotland_map <- get_map(scotland, zoom= 13, maptype = 'watercolor',
                        scale = 1, source = 'stamen') 

#Error: Google now requires an API key.
#See ?register_google for details.

##########################################################
# https://www.r-bloggers.com/2018/10/getting-started-stamen-maps-with-ggmap/ 
# also explains ggmap google and stamen
 
# Get Scotland boundaries
# https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-gb-bfc/about

countries <- read.csv("uk_countries.csv")
countries_shp <- st_read("/UK Boundaries/CTRY_DEC_2021_GB_BFC.shp")
# this is a huge file and my R wants to explode.

colnames(countries_shp)
str(countries_shp)

# Subset the sf object
ukmap_sub <- countries_shp[countries_shp$CTRY21NM %in% "Scotland",]
plot(st_geometry(ukmap_sub)) # don't just do 'plot', as it will plot the whole dataframe,  each variable so it takes ages


# make something called a boundary box
borders <- st_bbox(ukmap_sub)

#stamenmap times out with UK map - probably too big
map <- get_stamenmap(borders, zoom = 5, maptype = "toner-lite")

######TEST

#uk <- c(left = -12, bottom = 55, right = 55, top = 60)
get_stamenmap(uk, zoom = 5,"toner-lite") %>% ggmap()
