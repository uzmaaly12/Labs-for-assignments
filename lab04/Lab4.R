library(tidyverse)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(GISTools)
library(grid)

countyboundries <- sf::read_sf("../data/County_Boundaries-_Census.shp")%>% sf::st_make_valid() 
NE.counties <- read.csv(file = '../data/ne_counties.csv')
CountyData <- sf::read_sf("../data/lancaster_county.shp")
MuncData <-  sf::read_sf( "../data/Municipal_Boundaries.shp")%>% sf::st_make_valid() 
StatePark <- sf::read_sf("../data/State_Park_Locations.shp")%>% sf::st_make_valid()
StreamData <-  sf::read_sf ("../data/Streams_303_d_.shp")%>% sf::st_make_valid()
Dem <- raster("../data/lc_dem.tif") 

#Task 1................................ Frankenmap

#State data frame:

NE.map <- tm_shape(countyboundries) + tm_fill(col = "ALAND10", alpha = 0.8, style="jenks", legend.show = F) + 
  tm_borders(col = "lightgreen", lwd = 1.5, lty = "solid")+
  tm_scale_bar(breaks = c(0, 50,100), text.size = 0.9, position = c("right","top") )
NE.map

#County data frame:

M.Lancaster <- st_intersection(MuncData,CountyData) #intersect municipal data with county data to get Lancaster county data
Stream.Lan <- st_intersection(StreamData,CountyData) # intersect stream data with county data to get stream data for Lancaster county

#making map for the Lancaster county

Lancaster.map <- tm_shape(M.Lancaster)+
  tm_fill("NAME",legend.show = FALSE, alpha = 0.8)+
  tm_text("NAME")+ 
  tm_shape(Stream.Lan)+
  tm_lines(col = "blue",lwd = 1,lty = "solid",legend.col.show=FALSE)+
  tm_shape(StatePark)+
  tm_symbols(shape = 22,col = "olivedrab3")+
  tm_layout(legend.position =c("right","center"))
Lancaster.map

# map <- base + layer_1 + layer_2 + aesthetic + function + layer_3

tm_shape(countyboundries) + tm_fill() + tm_borders()
Lancaster <- countyboundries[countyboundries$NAMELSAD10 == "Lancaster County",]

tm_shape(StreamData) + tm_lines()
tm_shape(StatePark) + tm_dots()
tm_shape(MuncData) + tm_polygons()

tm_shape(Dem) + tm_raster(alpha=0.7)
tm_shape(Dem) + tm_raster()

Lancaster.region = st_bbox(c(xmin = -96.91394, xmax = -96.46363,
                             ymin = 40.52302, ymax = 41.04612),
                           crs = st_crs(countyboundries)) %>% st_as_sfc()

Inset.map <- tm_shape(Lancaster.region) + tm_borders(lwd = 5, col="red")

# Putting the state map as an inset to the county map

NE.map + Inset.map 

# -Lancaster cOUNTY
Lancaster.Mun <- sf::st_intersection(MuncData, Lancaster)
Lancaster.parks <- sf::st_intersection(StatePark, Lancaster)
Lancaster.streams <- sf::st_intersection(StreamData, Lancaster)

raster::crs(Dem) <- crs(Lancaster)  

# plot(Dem)

lc.map <- tm_shape(Dem) + 
  tm_raster(alpha = 0.6, palette = colorRampPalette(c("darkolivegreen4","yellow", "brown"))(12),
            legend.show = F) + tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_layout(main.title="  Map of Lancaster County", title.size = 1.1)

lc.map

#adding Lancaster county map and DEM map
lc.map + Lancaster.map
print(NE.map + Inset.map, vp = viewport(0.7, 0.137, width = 0.3, height = 0.3))

#Task 2...........................................Static Map

#map of Gilgit Baltistan, Pakistan

GB.Dem <- raster("../data/Gb_dem.tif")
Gb_boundry <- sf::read_sf("../data/Gilgit_new_distric_boundry.shp")%>% sf::st_make_valid() 
River.GB <- sf::read_sf("../data/waterways.shp")%>% sf::st_make_valid() 

#intersect GB boundary data with the river data
Gb.rivers <- sf::st_intersection(Gb_boundry, River.GB)
Gb.rivers

#creating map for GB region 
Gb.map <- tm_shape(Gb_boundry)+
  tm_fill("DISTRICT",legend.show = FALSE, alpha = 0.9, lwd = 1.7)+ 
  tm_shape(Gb_boundry) + tm_borders(col = "black", lwd = 1.5, lty = "solid")+
  tm_text("DISTRICT")+ 
  tm_shape(Gb.rivers) +
  tm_lines(col = "blue",lwd = 1.8,lty = "solid",legend.col.show=TRUE)+
  tm_layout(legend.position =c("right","center"))
Gb.map

#plotting the DEM for the GB region
glt.map <- tm_shape(GB.Dem) + 
  tm_raster(alpha = 0.28, palette = colorRampPalette(c("darkolivegreen4","yellow", "brown"))(12),
            legend.show = F) + tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_layout(main.title="  Rivers in Gilgit Baltistan, Pakistan", title.size = 1.1) 
glt.map

#Adding vector and raster data (GB boundary map and DEM map)
Gb.map + glt.map

