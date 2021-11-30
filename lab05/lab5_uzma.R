library(tidyverse)
library(sf)
library(tmap)
library(stringr)
library(sp)
library(GISTools)
library(leaflet)
library(raster)
library(elevatr)
library(MAP)
library(spdep)


# spatial
counties <- sf::read_sf("../data/CBW/County_Boundaries.shp") %>% sf::st_make_valid()
#aspatial
bmps <- read_csv("../data/CBW/BMPreport2016_landbmps.csv")

#---------------------Task 1------------------
#2.3 Make a map of the counties, shading each county by the total cost of BMPs funded/implemented in that county.
#This will required you to join multiple datasets together

crs = st_crs(counties)  
bmpgroup <- bmps %>% group_by(GeographyName) %>% summarise(TotalBMPcost = sum(Cost, na.rm = T)) %>%
  mutate(GEOID.Fixed = str_sub(GeographyName, 1,5))
join.groups <- left_join(counties, bmpgroup, by = c ("GEOID10" = "GEOID.Fixed"))

#set colors and number of classes for total BMP cost
pal_cost <- pal <- colorBin("YlGnBu", domain = bmpgroup$TotalBMPcost, bins = 5)

#bins <- c(0, 32, 65, 97, 129, 162 )
#pal <- colorBin( "YlOrRd" , domain =  join.groups$TotalBMPcost, bins = bins)

#creating leaflet map for BMP total cost
cost.map <- leaflet(join.groups)  %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addPolygons(
    fillColor = ~pal(TotalBMPcost),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7, highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE), 
    label = paste0(join.groups$NAME10, " County \n", join.groups$TotalBMPcost))  %>% 
  addLegend(pal = pal, values = join.groups$TotalBMPcost, opacity = 0.7, title = "BMP Total cost",
  position = "bottomright") %>% 
  addMiniMap(
    tiles = providers$Esri.NatGeoWorldMap, 
    toggleDisplay = TRUE)
cost.map

#---------------------------------Task 3-----------------------------------

#map of Gilgit Baltistan, Pakistan

GB.Dem <- raster("../data/Gb_dem.tif")
Gb_boundry <- sf::read_sf("../data/Gilgit_new_distric_boundry.shp")%>% sf::st_make_valid() 
River.GB <- sf::read_sf("../data/waterways.shp")%>% sf::st_make_valid() 
GLOF.GB <- read_csv("../data/CBW/GLOF_gb.csv")

crs = st_crs(Gb_boundry)
#intersect GB boundary data with the river data
Gb.rivers <- sf::st_intersection(Gb_boundry, River.GB)
Gb.rivers

#merge the intersected data with the GLOF data 
glof.r <- merge(Gb.rivers, GLOF.GB, by.x= "D_NAME", by.y = "River", na.rm= T)
glof.r

#intersected GB data with GLOF data 
gb.data <- sf::st_intersection(Gb.rivers, glof.r)
gb.data

#set the coordinate reference system (CRS) of the Raster data.
crs(GB.Dem) <- CRS("+init=epsg:4326")
crs(GB.Dem) 
prj_dd <- "EPSG:4326"
prj_dd 

#calculating elevation values of G.B
elev_edge <- get_elev_raster(GB.Dem, prj = prj_dd, z = 5)
elev_edge 

#crop GB boundary from the DEM data
GB_elev <- crop(GB.Dem,Gb_boundry )
plot (GB_elev)

##creating interactive mapping
#bins contains elevation of the DEM
bins <- c(0, 2000, 4000, 6000, 8000)
pal <- colorBin("YlOrRd", domain = GB_elev, bins = bins)

#showing elevation values of G.B 
elevation_GB <- leaflet() %>% addTiles() %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>% 
  addRasterImage(GB.Dem, colors = "Spectral", opacity = 0.7) %>% 
  addLegend(pal = pal, values = values(elev_edge),
            title = "GB Elevation(m)")
elevation_GB

#GB district
u <- leaflet(Gb_boundry) %>% 
  addPolygons(fillColor = ~topo.colors(10, alpha = NULL), stroke = FALSE,
    weight = 2,
  opacity = 0.5,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.4, highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0,
    bringToFront = FALSE),
  popup = Gb_boundry$DISTRICT,
  label = sprintf(Gb_boundry$DISTRICT)) %>% 
  addPolylines( data = Gb.rivers,
                stroke = TRUE,
                weight = 2,
                opacity = 0.5,
                fillOpacity = 0.2,
                smoothFactor = 1,
                label =  paste0(Gb.rivers$D_NAME,"--" ,River.GB$type )) %>% 
  addTiles(group = "OSM") %>%
  addProviderTiles("Stamen.Toner", group = "Toner by Stamen") %>% 
  addLayersControl(baseGroups = c("OSM", "Toner by Stamen" )) %>%
  addMiniMap(toggleDisplay = TRUE,
             position = "bottomleft")  

u
  
# GB rivers 
u1 <- leaflet()  %>% 
  addPolylines( data = Gb.rivers,
    stroke = TRUE,
                 weight = 2,
                 opacity = 0.5,
                 fillOpacity = 0.2,
                 smoothFactor = 1,
                 popup = River.GB$name,
                 label =  paste0(Gb.rivers$D_NAME,"--" ,River.GB$type )) %>% 
    addTiles(group = "Esri") %>%
    addProviderTiles("Esri", group = "Esri.WorldPhysical") %>% 
    addLayersControl(baseGroups = c("OpenTopoMap", "Esri.WorldPhysical" ))
u1

#GB GLOF 
#bins1 <- c(1990, 1995,2000, 2005, 2010,2015)
#pal1 <- colorBin(domain = gb.data$name, bins = bins1)

u2 <- leaflet(Gb.rivers)%>%
  addPolylines( data = Gb.rivers,
                stroke = TRUE,
                weight = 0.8,
                opacity = 0.5,
                fillOpacity = 0.2,
                smoothFactor = 1,
                popup = River.GB$name,
                label =  paste0(Gb.rivers$D_NAME,"--" ,River.GB$type )) %>% 
  addPolygons(fillColor = ~topo.colors(8, alpha = NULL), stroke = FALSE,
               weight = 0.9,
              opacity = 0.3,
              color = ~pal(gb.data$Year),
              dashArray = "3",
              fillOpacity = 0.5, highlightOptions = highlightOptions(
                weight = 5,
                color = "#456",
                dashArray = "",
                fillOpacity = 0.8,
                bringToFront = TRUE),
 label = paste0(Gb.rivers$D_NAME, " GLOF\n ", gb.data$Year)) %>% 
   addProviderTiles("CartoDB.Positron", 
  options= providerTileOptions(opacity = 0.99)) %>%
    addProviderTiles("OpenStreetMap", group = "CartoDB.Positron") %>% 
    addLayersControl(overlayGroups = c("Gb.rivers","gb.data"),
                     baseGroups = c("Toner by Stamen", "CartoDB.Positron" ))
u2

  
#-----------------------#Task 2................................. 
#From lab 3, task Bonus #2: 
  
substates <- sf::read_sf("../data/substates.shp")
sf::st_crs(substates) 

#Choose a variable. 

Japanese <- substates %>% mutate(JapanPop = (DP0080010/DP0080001)*100 )

#cordinate reference system

sf::st_crs(Japanese)
Japanese.projected <- Japanese %>% sf::st_transform(., "EPSG:4326")
tmap::tm_shape(Japanese.projected) + tm_polygons()

# make the neighborhood

US_Japanese <- spdep::poly2nb(Japanese.projected, queen = TRUE)
US_Japanese

#Row-standardize the W

lw <- nb2listw(US_Japanese, style="W", zero.policy=TRUE)
lw

#Calculate the average number of neighbors

Japan.Pop <- lag.listw(lw, Japanese.projected$JapanPop)
Japan.Pop

MI <- moran.mc(Japanese.projected$JapanPop, lw, nsim=999, zero.policy=T)
MI

#9. Make a Moran Plot

lmoran <- localmoran(Japanese.projected$JapanPop,lw)
lmoran
summary(lmoran)

bind.moran<- cbind(Japanese.projected, lmoran )
bind.moran

#create bins and palette
bins <- c(0, 0.30000, 0.50000, 0.76617)
pal_mo <- colorBin("YlOrRd", domain =Japanese.projected$JapanPop, bins = bins)

summary(Japanese.projected$JapanPop)

mo <- leaflet(Japanese.projected)%>% addTiles() %>% 
  addPolygons( data = Japanese.projected,
                fillColor = ~pal_mo(JapanPop),
                stroke = TRUE,
                weight = 0.8,
                opacity = 0.5,
                fillOpacity = 0.2,
                smoothFactor = 1,
                popup = Japanese.projected$NAMELSA,
                label =  paste0(Japanese.projected$NAMELSA,"--" ,Japanese.projected$JapanPop )) 
mo 

#pal_bm <- pal <- colorBin("RdBu", domain = bind.moran$E.Ii, bins = lmoran)

bm <- leaflet()%>% addTiles() %>%
  addPolygons( data = bind.moran,
    fillColor = ~topo.colors(8, alpha = NULL), stroke = FALSE,
              weight = 0.9,
              opacity = 0.3,
              color = bind.moran$Ii,
              dashArray = "3",
              fillOpacity = 0.5, highlightOptions = highlightOptions(
                weight = 5,
                color = "#456",
                dashArray = "",
                fillOpacity = 0.8,
                bringToFront = TRUE),
              label = paste0(bind.moran$E.Ii) %>% 
  addLayersControl(overlayGroups = c("MI")))
  
bm







  







