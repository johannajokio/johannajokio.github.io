---
title: Smoking prevalence in the Scottish Health Survey
---

I posted a map of smoking prevalence in Scotland's health boards, [here](https://github.dev/johannajokio/johannajokio.github.io/blob/ef75ec7117ce61f2fca4d16c31d13dc98ce3313e/posts/2022-10-31-smoking-prevalence.md). However, the *tmap* method binned (grouped) the values and you can´t see the exact percentage in each region. Furthermore, the (Scottish Government local area data)[http://statistics.gov.scot/data/scottish-health-survey-local-area-level-data] has been updated with aggregate figures from the years 2017-2021. Therefore I updated the map below using *ggplot*.

```r
ggplot(shp_hb) +
  geom_sf(color= 'white', aes(fill = as.factor(Rate))) +
  geom_sf_text(aes(label = HBName), color = 'black', size = 2) +
  scale_fill_brewer(palette = 'PuBuGn') +
  theme_void() +
  labs(fill = "% smokers") +
  ggtitle('Percentage current smokers by Health Board, 2017-2021',
          subtitle = 'Scottish Health Survey Indicator Smoking status: Current smoker') +
  theme(plot.title = element_text(family = "Arial",              # Font family
                                  face = "bold",                 # Font face
                                  color = 1,                     # Font color
                                  size = 12,                     # Font size
                                  margin = margin(2, 0, 0, 2)),  # Margins (t, r, b, l)
        plot.subtitle = element_text(size = 11, 
                                     hjust = 0))    # Subtitle customization
```
![Map of smoking prevalence in Scotland, 2017-2021](images/smokers_hb_updated.png)