# Labs-for-assignments
#Task 1: BASIC DATA MANIPULATION 
#1.1 For each county, calculate its land area as percentage of the total area (land + water) for that state.

CountyArea <- d.counties %>% group_by (STATEFP10)%>% mutate( StateTotalArea = sum(ALAND10 + AWATER10)) %>% mutate(LandAreaPercent=(ALAND10/StateTotalArea)*100)
 
glimpse (CountyArea)

#1.2 For each state, find the county that has the largest proportion of its land as water (water area / total area)

Statewater<- d.counties %>% group_by (STATEFP10) %>% mutate (totalarea = sum(ALAND10 + AWATER10)) %>% mutate(largeWaterArea = (AWATER10/totalarea))
glimpse(Statewater)

#1.3 Count the number of counties in each state

d.counties %>% as_tibble()%>% dplyr::select(-geometry)%>% group_by(STATEFP10) %>% summarise(totalcounties = COUNTYFP10 )

d.counties %>% as_tibble()%>% dplyr::select(-geometry)%>% group_by(STATEFP10) %>% summarise(totalcounties =n())

#1.4 Which station has the shortest name (STATION_NA) in the study area?
d.stations$STATION_NA  
statshortname<-d.stations %>% mutate (., namelength = nchar(d.stations$STATION_NA)==min(nchar(d.stations$STATION_NA)))
min(nchar(d.stations$STATION_NA))

#Task 2: Plotting attribute data
#2.1 Make a scatterplot showing the relationship between land area and water area for each county. Color each point using the state variable

ggplot(d.counties,aes(x=ALAND10,y= AWATER10,colour=STATEFP10))+geom_point()+ labs(title = "Relationship between land area and water area")

#2.2 Make a histogram of drainage area (Drainage_A) for all monitoring stations

d.stations %>% ggplot(., aes(x = Drainage_A)) + geom_histogram(aes(fill = MAP_ID)) + labs(title = "Drainage area for all monitoring stations") 

#2.3 Make a similar histogram, this time of drainage area (Drainage_A) for all monitoring stations. Color each point using the state variable

ggplot(d.stations,aes(x=Drainage_A,fill= MAP_ID,color= "purple"))+geom_histogram() + labs(title = "Relationship between land area and water area")

#Task 3: Write a function

#3.1 Write a function that does the following:
#A. accepts a vector of arbitrary numbers, calculates the mean, median, maximum, and minimum of the vector

A <- function(marks){mean(marks)
  median(marks)
  max(marks) 
  min(marks) 
  sort(marks) }

marks <- c(8,5,12,4,14,9,5,18)

A(marks)
sort(marks)

marks[1] >=14

Grades <-
  function
(marks){
  if
  (marks >=
   14
  ){
    print(
      "A"
    )
  }
  else
  {
    print(
      "B" ) }}
for
(i
 in
marks){
 Grades(i)}

#D.the function should only work with numeric values and print an error message if any other data type are found
#c(1, 0, -1)
#Test it with the following vectors

x <-c(1, 0, -1)
x[2]
Numeric <- function (x){if (x >=  1 ){ print(  "A" )} else { print("B")  }}
for (i in x){ Numeric(i) }


#c(10, 100, 1000)
y <- c(10, 100, 1000)
y[3]
Numeric <- function (y){if (y <= 100 ){ print(  "Yes" )} else { print("No")  }}
for (i in y){ Numeric(i) }


#c(.1, .001, 1e8)
z<-c(.1, .001, 1e8)
z[3]
Numeric <- function (z){if (z >=10 ){ print(  "Yes" )} else { print("No")  }}
for (i in z){ Numeric(i) }


#c("a", "b", "c")
e<- c("a", "b", "c")
e [1]
Numeric <- function (e){if (e >=10 ){ print(  "TRUE" )} else { print("FALSE")  }}
for (i in z){ Numeric(i)}


#Task 4: (slightly) more complex spatial analysis.
#4.1 Calculate the number of monitoring stations in each state

d.stations %>% group_by(STAID) %>% count(d.stations$MAP_ID)

#4.2 Calculate the average size of counties in New York (that are also in this study area)

NewYork <- d.counties %>% dplyr::filter(STATEFP10 == 51)
meanNewYork_county<-mean(NYW$ALAND10)
meanNewYork_county

#4.3 Calculate which state has monitoring stations with the greatest average drainage area (Drainage_A)

Max_DA<-max(d.stations$Drainage_A)
Max_DA

#Q1.In using the intersection functions, are the following two statements equivalent? If not, explain how.
#Be sure to think about BOTH the spatial data structures AND the attribute data. Would your answer be different if we were using different types of data?
sf::st_intersection(d.stations, del.counties)
sf::st_intersection(del.counties, d.stations)
#2.What did you find challenging in this lab? What was new?
The overall lab was a challenging for me but the Tasks 2 and 4 were more challenging.  
The plotting attribute data was new for me.
#3.What types of activities would you like to see in labs this semester?
Doing lab works in class 
