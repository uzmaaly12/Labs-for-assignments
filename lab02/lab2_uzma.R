#assignment-Lab02
#Uzma

library(tidyverse)
library(sf)
library(tmap)
library(stringr)
library(sp)
library(GISTools)

# spatial
counties <- sf::read_sf("../data/CBW/County_Boundaries.shp") %>% sf::st_make_valid()
dams <- sf::read_sf("../data/CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% sf::st_make_valid()
streams <- sf::read_sf("../data/CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% sf::st_make_valid ()

#aspatial
bmps <- read_csv("../data/CBW/BMPreport2016_landbmps.csv")

#Task 1: Aspatial operations
#1.1 Calculate summary statistics for the Cost of BMPs for each State (including DC)

CostofBMPs <- bmps %>% group_by(StateAbbreviation, BMP) %>% summarise(totalCost = sum(Cost, na.rm = T))
summary(CostofBMPs$totalCost)

#1.2 Make a scatterplot of Cost vs. TotalAmountCredited, ONLY FOR Units of type "Acres". You may need to apply a data transformation to one or more axes if the data are heavily skewed.

bmps %>% group_by(Unit = "Acres", TotalAmountCredited, Cost ) %>%
dplyr::filter(., TotalAmountCredited > 1 & TotalAmountCredited < 300 &  Cost > 1 & Cost < 400) %>% 
ggplot(., aes(x = TotalAmountCredited , y = Cost, colour= Unit))  +  labs(title = " Cost vs. TotalAmountCredited ")+
geom_point(aes(fill= Unit)) 


#1.3 Make a boxplot with "StateAbbreviation" on the x-axis and "TotalAmountCredited" on the y-axis. HOWEVER, the only data I want plotted are for cover crop BMPs. Note, there are many types of cover
#crops in this dataset, and I want you to include them ALL. There are handy functions within the stringr package that can help you here.

bmps  %>% 
dplyr::filter(., str_detect( BMP, "Cover"), TotalAmountCredited > 1 & TotalAmountCredited < 300) %>%
ggplot(., aes(x = StateAbbreviation, y = TotalAmountCredited)) +
geom_boxplot(aes(fill = StateAbbreviation)) + labs(title = "Total amount Credited for Cover Crops") 
 
  
#1.4 make a scatterplot of the dam dataset, this time with "YEAR" on the x-axis and "STATE" on y-axis (think of it like a timeline). Assume no dams were built in year 0, so you'll need to remove those data points.

dams %>% dplyr::filter(., YEAR > 1 )  %>% 
ggplot(., aes(x = YEAR, y = STATE, colour = 'YEAR' )) +  
labs(title = " Dams construction fro 1900 - 1960 ")+ 
geom_point(aes()) 

#1.5 make one last (aspatial) visualization. But this time, it's your choice what data and plots to use. The only requirement is that you link two of the datasets together in some manner. Be creative. Make it look
#nice (e.g., use proper labels, interesting colors/shading/size)
  
dams %>% ggplot(., aes(x = STATE, y = DamRemoval, colour= STATE)) + 
geom_point(aes(fill = STATE)) + 
labs(title = "Removal of dams from creeks, 2012-2017" , y="Dam removal year", x = "State") + 
facet_grid(STATE~.) 

#Task 2: Spatial operations
#2.1 Find the 5 longest streams in the 'streams opened by dam removal' dataset

longeststreams <- streams %>% filter(., !is.na(GNIS_Name)) %>% group_by(GNIS_Name) %>% 
summarise(Totallength = sum(LengthKM)) %>% slice_max(n=5, order_by = Totallength)
longeststreams  

#2.2 Find the three counties with the greatest TOTAL length of streams (opened by dam removal) in them

st.counties <- counties %>% dplyr::select(STATEFP10)

countystreams <- sf::st_intersection(streams, st.counties)%>%
group_by(STATEFP10) %>% summarise(Greatestlength = sum(LengthKM)) %>% slice_max(n=3, order_by = Greatestlength) 


#2.3 Make a map of the counties, shading each county by the total cost of BMPs funded/implemented in that county.
#This will required you to join multiple datasets together

bmpgroup <- bmps %>% group_by(GeographyName) %>% summarise(TotalBMPcost = sum(Cost, na.rm = T)) %>%
mutate(GEOID.Fixed = str_sub(GeographyName, 1,5))
  
join.groups <- left_join(counties, bmpgroup, by = c ("GEOID10" = "GEOID.Fixed"))

tm_shape(join.groups) + tm_polygons(col ="TotalBMPcost") 

#2.4 For each removed dam, find the closest stream segment

#mydist <- st_distance(dams, streams, by_element = F)

streamsIds <- sf::st_nearest_feature(dams, streams)

streams[streamsIds,]
lut <- data.frame(damId = seq(1, length(streamsIds)), streamId = streamsIds)

stream.joined <- streams %>% mutate(newId = seq(1:nrow(.))) %>% 
left_join(., lut, by = c("newId" =  "streamId"))
stream.dam <- dplyr::filter(stream.joined, !is.na(damId))
stream.dam

#2.5 Calculate how many removed dams are (or were) in each state

States.dams <- dams %>% group_by(STATE) %>% summarise(Total_dams = n())
States.dams
tibble(States.dams)






