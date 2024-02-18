#!/usr/bin/env python
# coding: utf-8

# ## Eurostat

import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# load Natural earth shapefile

land10 = gpd.read_file("ne_110m_admin_0_countries.shp")

# In[16]:


# Read data file
#Tertiary education (levels 5-8)

wb = pd.read_excel("eurostat_edat_lfs_9903.xlsx", sheet_name="Sheet 1")


# In[41]:


wb.head()


# In[40]:


wb = wb.rename(columns = {"GEO (Labels)": "Country"})


# In[35]:


land10 = land10.rename(columns = {"SOVEREIGNT" : "Country"})


# In[36]:


type(land10)


# In[42]:


## merge with datafile

df  = wb.merge(land10, on ='Country', how ='left')
gdf = gpd.GeoDataFrame(df)


# In[75]:


gdf["Percentage"].isna()


# In[88]:


#countries that have no value should be set to missing which we can do by 
gdf['Percentage'] = pd.to_numeric(gdf['Percentage'], errors='coerce')
gdf


# In[ ]:


# check 
from matplotlib import colormaps
list(colormaps)


# In[109]:


# create an Axes object and plot the map

fig, ax = plt.subplots(1,1)
ax.set_axis_off();

gdf.plot(ax=ax, 
        column="Percentage", 
        cmap= 'YlGn',
        legend=True,
        legend_kwds={"label": "Percentage Tertiary Educated", "orientation": "horizontal"},
        missing_kwds= dict(color= "lightgrey", label="Missing") #However, I couldn't make a label show up for missings
    )

# limit the map to europe
xmin, ymin, xmax, ymax = gdf[gdf.Country == 'Germany'].total_bounds
pad = 20    # add a padding around the geometry
ax.set_xlim(xmin-pad, xmax+pad)
ax.set_ylim(ymin-pad, ymax+pad)


# In[122]:


#now we can tweak the legend

from mpl_toolkits.axes_grid1 import make_axes_locatable

fig, ax = plt.subplots(1, 1) 

divider = make_axes_locatable(ax)

#axes on which to draw the legend in case of color map
cax = divider.append_axes("bottom", size="5%", pad=0.1)

ax.set_axis_off();
gdf.plot(ax=ax, 
        column="Percentage", 
        cmap= 'YlGn',
        legend=True,
        cax=cax,
        legend_kwds={"label": "Percentage Tertiary Educated", "orientation": "horizontal"},

    )

xmin, ymin, xmax, ymax = gdf[gdf.Country == 'Germany'].total_bounds
pad = 20 # add a padding around the geometry
ax.set_xlim(xmin-pad, xmax+ pad)
ax.set_ylim(ymin-pad, ymax+ pad)

# Figure footer title
fig.text(0.4, 0.9, "Source: Eurostat 2022",
         horizontalalignment="center", fontsize=8)

# Save plot to file 
plt.savefig("eurostat.png", dpi=600)



