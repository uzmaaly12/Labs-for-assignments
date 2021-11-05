library(spdep)
library(sf)
library(tidyverse)
library(tmap)

#1. Create a spatial subset of the US, with at AT MINIMUM 4 states, MAXIMUM 7 states. States must be contiguous. Save this subset as a shapefile such that it's sufficiently small in size that GitHub will accept the git-push

substates <- sf::read_sf("../data/substates.shp")
sf::st_crs(substates)

#2. Choose a variable. If it's a raw count, you should normalize the variable in an appropriate manner (e.g., by total population, percent, by area)

Asian <- substates %>% mutate(AsianPop = (DP0080006/DP0080001)*100 )

#3 Make a histogram of your chosen variable

hist(Asian$AsianPop)

#4Make a choropleth map of your chosen variable. Choose an appropriate data classification scheme

tm_shape(Asian) + tm_fill(col = "AsianPop", alpha = 0.9) +
  tm_borders(col = "brown", lwd = 1.5, lty = "solid")+ tm_scale_bar(breaks = c(0, 50,100), text.size = 0.5, position = c("right", "top"))

#5Develop a contiguity-based spatial weights matrix of your choosing (i.e., rook or queen)

sf::st_crs(Asian)
Asian.projected <- Asian %>% sf::st_transform(., "ESRI:102010")
tmap::tm_shape(Asian.projected) + tm_polygons()
US_Asian <- spdep::poly2nb(Asian.projected, queen = TRUE)
US_Asian[[1]]
Asian.projected$NAMELSA[1] # county in index 1
US_Asian[[1]] %>% Asian.projected$NAMELSA[.] # and it's neighbors.

#6Row-standardize the W

lw <- nb2listw(US_Asian, style="W", zero.policy=TRUE)
lw$weights[1]

#7. Plot a histogram of the number of neighbors

neighbors_Asian <- attr(lw$weights,"comp")$d
hist(neighbors_Asian)

#8.Calculate the average number of neighbors

USA.AsianPop <- lag.listw(lw, Asian.projected$AsianPop)
USA.AsianPop

#9. Make a Moran Plot

Moran.plot <- moran.mc(Asian.projected$AsianPop, lw, nsim=999)
# View results (including p-value)

Moran.plot
plot(Moran.plot, main="", las=1)

#10. Repeat #5 (and 5 - 9) above with a W developed using the IDW method. 
#You will need to investigate the spdep documentation to find the correct method/function.

#5Develop a contiguity-based spatial weights matrix of your choosing (i.e., rook or queen)
coords <- Asian.projected %>% as_Spatial() %>% coordinates()
A.dist <- dnearneigh(coords, 0, 50000)
centrioids <- st_centroid(Asian.projected)

#6Row-standardize the idW
Idw <- nb2listwdist(A.dist, centrioids, type="idw", style="W", zero.policy= T)

#7. Plot a histogram of the number of neighbors
neighbors.Asia <- attr(Idw$weights,"comp")$d
hist(neighbors.Asia)

#8.Calculate the average number of neighbors
Asian.lag <- lag.listw(Idw, centrioids$AsianPop)
Asian.lag

#9. Make a Moran Plot
MI <- moran.mc(Asian.projected$AsianPop, Idw, nsim=999, zero.policy=T)
plot(MI, main="", las=1)
moran.plot(Asian.projected$AsianPop, lw, zero.policy=TRUE, plot=TRUE)






