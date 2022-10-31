---
title: Smoking with R's tmap
---
In my first post, I'll explain how to process public health data to generate a map on R. 

I've taken a csv file with the number of current smokers in Scotland's health boards from [here](https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data).

You get a file with just three columns, one with the health board code and the rate of smoking for males and females and the total rate.

Basically, I tried a few packages and got stuck with various dependency errors. I used *sf* (simple features) and *sp* to read in and join the files. For the map, I ended up using `tmap` which is flexible and the syntax is nice and concise.

I used `st_read` to read in a shapefile for health boards, and joined the smokers file to that using *sp's merge* through the health board codes.
```r
shapeData <- st_read("SG_NHS_HealthBoards_2019.shp")

shp_hb <- sp::merge(shapeData, smokers_hb, by.x = "HBCode", by.y ="hb2019", duplicateGeoms = T)
```
Note the "duplicateGeoms = TRUE" here, as the data files contain non-unique matches, ie. there are many rows for one health board code in the shapefile.

`st_read` is good because you don't have to transform the file afterwards like with *rgdal* for example - if you inspect it, e.g.
`r summary(shapeData) `, you see that it's a spatial polygons dataframe and doesn't need converting into a dataframe.
     
Finally, the code to create the map starts with `tm_shape` and the merged file. In `tm_fill(col = "Rate")`, you tell it to fill the polygons with colours defined by the variable you want, in this case, total rate of smoking. 

I tested a few colour palettes when I was using other packages (like *RColorBrewer*), and when including the palette you have to define whether the data is continuous or discrete. I was confused at many points with this, as the smoking data is... not continuous but not discrete? The csv just includes a number (the percentage) for each health board (it's discrete). But tmap did this automatically.

With other packages, I also had issues trying to centre the health board labels and not have them overlap, but tmap has a handy argument, `remove_overlap` which does just that. And I ended up using the *tmap* style *cobalt*.

```r
tm_shape(shp_hb) +
  tm_fill(col = "Rate") +
  tm_text("HBName", size = 0.5, col = "white", remove.overlap = TRUE) + 
  tm_borders() +
  tmap_style("cobalt") +
  tm_layout(legend.position = c("left", "top"), title= '% current smokers in Health Boards', 
            title.position = c("left", "top")) +
  tm_credits("Scottish Health Survey, 2019", position=c("left", "bottom"))
  ```

This is the resulting map:

![Map of smoking prevalence in Scotland](SmokersHBs.jpg)

There are things that could be improved, like some of the health board names could be showing better, but overall I'm pleased with how it turned out.