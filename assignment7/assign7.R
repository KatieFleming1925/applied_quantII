# PART 1: IN-CLASS (EXPLORING SPATIAL DATA WITH SF)
## Loading data
install.packages("sf")
install.packages("spData")
library(sf)
library(spData)
library(ggplot2)
library(dplyr)
library(tidyr)
data(world)

## 1.1.a: Load the world dataset and inspect its structure. 
class(world)
names(world)
nrow(world)
## In a comment, describe what makes an sf object different from a regular R df. What is the geometry column, and how is it stored?

## 1.1.b: Check the coordinate reference system (CRS) with st_crs(world). 
st_crs(world)
## What EPSG code does the dataset use?
### ANSWER/COMMENT: The dataset uses the 4326 CRS code. 
## In a comment, explain what WGS84 means and why it is the standard CRS for global geographic data.
### ANSWER/COMMENT: 

## 1.1.c: Use stgeometrytype and unique to inspect geometry type.
st_geometry_type(world)
unique(st_geometry_type(world))
## In a comment, explain what a MULTIPOLYGON is and given two concrete examples of countries that would require multiple polygons to represent their territory.
### ANSWER/COMMENT: 

## 1.1.d: Produce a quick map of GDP per capita using base R graphics:
#pdf("world_gdp_base.pdf")
##
plot(world["gdpPercap"])
##
plot(world["gdpPercap"], main = "GDP per capita by country")
## In a comment, describe what you see. Which regions appear wealthiest and which poorest?

## PROBLEM 2: ATTRIBUTE OPERATIONS
## 2.1.a: Using filter, create a subset of world containing only African countries. Call it africa.
africa=filter(world, continent=="Africa")
## How many African countries are in the dataset?
nrow(africa)
### ANSWER/COMMENT: There are 51 countries in the African dataset. 
## Plot africagdppercap using base graphics.
plot(africa["gdpPercap"], main="GDP per capita -- Africa")

## 2.1.b: Add pop
world=world%>%
  mutate(pop_millions=pop/1e6)

gdp_by_continent=world%>%
  group_by(continent)%>%
  summarise(mean_gdpPercap=mean(gdpPercap, na.rm=TRUE))

print(st_drop_geometry(gdp_by_continent))

## 2.1.c: Top 5 African countries by GDP per capita
africa_sorted=africa%>%
  arrange(desc(gdpPercap))%>%
  select(name_long, gdpPercap)

print(head(st_drop_geometry(africa_sorted), 5))


## PROBLEM 3: SIMPLE VISUALIZATION WITH GGPLOT2
## 3.1.a: 
ggplot(world)+
  geom_sf(aes(fill=gdpPercap))+
  scale_fill_viridis_c(option="plasma", na.value="grey80", name="GDP per capita")+
  theme_void()+
  labs(title="GDP per capita by country")

ggsave("world_gdp.pdf", width=10, height=5)

## Africa map with magma palette
ggplot(africa)+
  geom_sf(aes(fill=gdpPercap))+
  scale_fill_viridis_c(option="magma", na.value="grey80", name="GDP per capita")+
  theme_void()+
  labs(title="GDP per capita -- Africa")

ggsave("africa_gdp.pdf", width=7, height=6)


# PART 2: TAKE-H0ME (POINT DATA & SPATIAL JOINS)
## UPLOADING DATA
df=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/spatial/conflict_events.csv")
## PROBLEM 2.1 CONVERTING TABULAR DATA TO SF
## 2.1.a: Convert the events df to an sf object. Hint: use st_as_sf()
events=st_as_sf(df, coords=c("longitude", "latitude"), crs=4326)
## Run class() and st crs() on the result to verify it worked. 
class(events)
st_crs(events)
## In a comment, explain what st_as_sf() does: what does the coords argument specify, and what does crs = 4326 mean?
### ANSWER/COMMENT: This function converts the df into an sf object. We use the coords argument to tell the command which columns contain location data. The function then converts these to spatial points that R can plot and analyze. CRS=4326 tells R where these points are located on Earth's surface.

## 2.1.b: How many events are in the dataset? 
nrow(events)
### ANSWER/COMMENT: There are 68,354 events in the dataset.
## Count the event types. 
table(events$event_type)
## In a comment, which event type is most common? 
### ANSWER/COMMENT: The most common event type is state-based.

## 2.1.c: Make a map of conflict events overlaid on the world polygon. Use ggplot with two geom_sf layers: the first for the world polyons as a grey background and the second for events colored by event type. 
data(world)
cemap=ggplot()+
  geom_sf(data=world, fill="grey90", color="white")+
  geom_sf(data=events, aes(color=event_type), alpha=0.5)+
  theme_minimal()+
  labs(title="Global Conflict Events", color="Event Type")
## Save it with ggsave.
ggsave("assignment7/cemap.png")
## In a comment, describe the geographic pattern. In which regions are conflict events most concentrated?
### ANSWER/COMMENT: I can only see conflicts that unfolded in Africa reflected here. 

## PROBLEM 2.2 SPATIAL JOIN: EVENTS TO COUNTRIES
## 2.2.a: Use stjoin to assign country attributes from the world polygon to each conflict event.
## Before joining, verify that both objects share the same CRS. 
st_crs(world)==st_crs(events)
joined=st_join(events,world)
## Run nrow on the result and verify it equals nrows events.
nrow(joined)==nrow(events)
## In a comment, explain what stjoin is doing: how does it determine which country polygon each event point falls within? Why is checking the CRS before so important?
### ANSWER/COMMENT: stjoin merges two datasets based on their geographic relationship. Each point's geometric coordinates are matched against the boundary of the country polygon (in world data). When there is a match, stjoin takes the corresponding information from world and adds them to the row for each corresponding event. Checking the CRS is important because we need to make sure that both datasets are using the same mathematical instructions when it comes to mapping coordinates on the earth's surface.


## 2.2.b: Some events may not match any country polygon (e.g., events at sea, on islands, or exactly on a border).
sum(is.na(joined$name_long))
## What fraction of events has no matching country?
sum(is.na(joined$name_long))/nrow(joined)
### ANSWER/COMMENT: 0.02305644 or about 2.3% of conflict events have no matching country.
## In a comment, list two possible reasons why a point might not match any polygon. 
### ANSWER/COMMENT: A possible reason why a point might not match any polygon is in instances of conflicts right on the border between two polygons. Another reason could be slight border inaccuracies that place them outside a polygon.

## 2.2.c: Count the number of events and total fatalities per country. 
fatalities_country=joined%>%
  filter(!is.na(name_long))%>%
  st_drop_geometry()%>%
  group_by(name_long)%>%
  summarise(n_events=n(), total_fatalities=sum(fatalities, na.rm=TRUE))%>%
  arrange(desc(n_events))


head(fatalities_country,10)
## In a comment, are the results consistent with your knowledge of contemporary armed conflicts?
### ANSWER/COMMENT: Yes, these results are consistent with my knowledge of contemporary armed conflicts. I know that Ethiopia, for example, underwent major unrest within the past few years.

## PROBLEM 2.3 CHOROPLETH OF CONFLICT INTENSITY
## 2.3.a: Join the event counts back to the world polygon data.
event_counts=joined%>%
  st_drop_geometry()%>%
  filter(!is.na(name_long))%>%
  group_by(name_long)%>%
  summarise(n_events=n())

world_counts=world%>%
  left_join(event_counts, by="name_long")%>% ## merges country name
  mutate(n_events=replace_na(n_events,0)) ## Replace NA values with 0 for countries with no events.

## Verify that the row count matches nrow world.
nrow(world_counts)==nrow(world)

## 2.3.b: Make a choropleth map of conflict event counts by country.
choromap=ggplot(world_counts)+
  geom_sf(aes(fill=n_events))+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme_minimal()+
  labs(title="Conflict Event Counts by Country", fill="Number of Events")
## Save with ggsave.
ggsave("assignment7/choromap.png")
## In a comment, describe the map. Does the geographic pattern match the event-level map from 2.1.c?
### ANSWER/COMMENT: Yes, both maps share geographic patterns.

## 2.3.c: Make a second map using log-transformed counts.
conflict_log_map=ggplot(world_counts)+
  geom_sf(aes(fill=log1p(n_events)))+
  scale_fill_distiller(palette="Y10rRd", direction=1, name="Log(events+1)")+
  theme_minimal()+
  labs(title="Conflict Event Counts by Country", fill="Number of Events")
## Save as pdf.
ggsave("assignment7/conflict_log_map.pdf")
## In a comment, explain why the log transformation is useful and what it reveals that the raw count map did not reveal.
### ANSWER/COMMENT: The log transformation takes into account the countries with 0 events, or at one end of the extreme. This helps with data skewing.

## Problem 2.4: BONUS: ARE EVENTS FAR FROM THE CAPITAL CITY MORE DEADLY?
## 2.4.a: You want to know if events that take place away from the country capital have more fatalities (presumably bc of lower state capacity). Let's explore this in Nigeria.
## 2.4.b: Create a subset of the events in Nigeria.
nigeriabonus=joined%>%
  group_by(name_long)%>%
  filter(name_long=="Nigeria")
## 2.4.c: Create another dataframe with latitude and longitude for capitals.
## It's fine if it only has one row for Abuja. 
abuja=data.frame(
  name="Abuja",
  longitude=7.3986,
  latitude=9.0765
)
## Transform that object into a spatial one. 
capital=st_as_sf(abuja, coords=c("longitude", "latitude"), crs=4326)

## 2.4.d: Transform both spatial objects to UTM projection. 
nigeriabonus_utm=st_transform(nigeriabonus, crs=32632)
abuja_utm=st_transform(capital, crs=32632)

## 2.4.e: Calculate distance between the events df and the Abuja df using stdistance.
distances=st_distance(nigeriabonus_utm, abuja_utm)
## Add this to the events dataframe.
nigeriabonus_utm$distance_to_cap=as.numeric(distances)/1000

## 2.4.f: Run linear regression models where your outcome is fatalities and your main independent variable is distance from the national capital.
## Try using fatalities in logarithmic scale, and distance in log of kilometers.
## Also, try controlling for and interacting with event type.
m1=lm(log(fatalities+1)~log(distance_to_cap)+event_type+log(distance_to_cap)*event_type, data=nigeriabonus_utm)
m1

## 2.4.g: What are the results? Are events away from the capital more deadly?
### ANSWER/COMMENT: Yes, events away from the capital are more deadly. The coefficient on distance to capital is -0.09829.

## Problem 2.5: DISCUSSION:
## 2.5.a: Discuss one limitation of the spatial data approach used in this assignment. For example: what happens to events that fall exactly on the border between two countries?
## How might you handle events that fall just outside a polygon due to small coordinate imprecisions?
### ANSWER/COMMENT: One limitation of the spatial data approach that we used in this assignment is the uncertainty around mapping events that fall along a border. One way to resolve this is to assign points to the nearest polygon.
## 2.5.b: What is the difference between st join() and left join()? What information does each use to match rows, and when would you prefer one over the other?
### ANSWER/COMMENT: The st_join command combines rows on a spatial relationship. The left_join function in R combines rows from two datasets based on a variable both datasets share, with rows from the left dataset being kept. You use the former when your matching depends on the geometry whereas the latter can be used for tabular merges. 
