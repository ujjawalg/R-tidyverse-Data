library(tidyverse)
library(readr)
library(ggthemes)
library(sqldf)
library(leaflet)
library(jsonlite)
library(RColorBrewer)
library(sqldf)
library(geosphere)
library(sp)
library(rgeos)
library(plyr)
library(fossil)

severe_incident <- read.csv("severe_incidents.csv", header = TRUE, stringsAsFactors=FALSE)

severe_inc <- sqldf("Select * from severe_incident where Longitude < '-73.699215' and Longitude > '-74.257159' and Latitude > '40.495992' and Latitude < '40.915568'")

popup1 <- paste("Incident Type:",severe_inc$INCIDENT_TYPE_DESC,"<br/>",
                "When:",severe_inc$INCIDENT_DATE_TIME,"<br/>",
                "Property Type:",severe_inc$PROPERTY_USE_DESC,"<br/>",
                "Action Taken:",severe_inc$ACTION_TAKEN1_DESC,"<br/>")

Ans1 <- leaflet(severe_inc) %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(attribution = ""))%>%
        setView(lng = -74.0156491, lat = 40.7022541, zoom = 10)%>%
       addCircleMarkers(lng = ~Longitude, lat = ~Latitude, col = "red", radius = 0.1, popup = popup1, stroke = 0.1, opacity = 0.7) %>%
  addEasyButton(easyButton(
    icon="fa-globe", 
    title="New York City",
    onClick=JS("function(btn, map){ map.setZoom(10); }")))

Ans1


pal = colorFactor("Set1", domain = severe_inc$PROPERTY_USE_DESC)
color1_Property = pal(severe_inc$PROPERTY_USE_DESC)


Ans2a <- Ans1%>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, col = color1_Property, radius = 0.1, popup = popup1, stroke = 0.1, opacity = 0.7)%>%
addLegend(pal = pal, values = ~severe_inc$PROPERTY_USE_DESC, title = "Property Types",position="bottomright")

Ans2a

Ans2b <- leaflet(severe_inc) %>%
  addProviderTiles("Esri.NatGeoWorldMap",
                   options = providerTileOptions(attribution = ""))%>%
  setView(lng = -74.0156491, lat = 40.7022541, zoom = 10)%>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, col = color1_Property, radius = 5, popup = popup1, stroke = 2, opacity = 0.7,clusterOptions = markerClusterOptions())%>%
  addLegend(pal = pal, values = ~severe_inc$PROPERTY_USE_DESC, title = "Property Types",position="bottomright") %>%
  addEasyButton(easyButton(
    icon="fa-globe", 
    title="New York City",
    onClick=JS("function(btn, map){ map.setZoom(10); }")))

Ans2b

Firehouses1 <- read.csv("FDNY_Firehouse_Listing.csv", header = TRUE, stringsAsFactors=FALSE)

Firehouses <- sqldf("Select * from Firehouses1 where Longitude < '-73.699215' and Longitude > '-74.257159' and Latitude > '40.495992' and Latitude < '40.915568'")

FireIcons <- icons(
  iconUrl = "fire.png",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 7.5, iconAnchorY = 7.5)


Ans3 <- leaflet() %>%
  addProviderTiles("Esri.NatGeoWorldMap",options = providerTileOptions(attribution = "")) %>%
  addCircleMarkers(data=severe_inc,lng = ~Longitude, lat = ~Latitude, radius=~UNITS_ONSCENE/4, fillOpacity=0.7,
  popup=~paste("Incident on:",severe_inc$STREET_HIGHWAY,"<br>",
               "Units on Scene:",severe_inc$UNITS_ONSCENE,"<br/>"),group = "Incidents") %>%
  addMarkers(data=Firehouses,lng = ~Longitude, lat = ~Latitude,
             icon = FireIcons,
             popup=~paste("Facility Address:",Firehouses$FacilityAddress,"<br>",
                          "Borough:",Firehouses$Borough,"<br/>"),group ="Firehouses")%>%
  setView(lng = -74.0156491, lat = 40.7022541, zoom = 10)%>%
  addLayersControl(overlayGroups = c("Incidents","Firehouses"),
    options = layersControlOptions(collapsed = TRUE))

Ans3

#Ques4

severe_inc$Incident_time <- as.POSIXct(severe_inc$INCIDENT_DATE_TIME,format="%m/%d/%Y %H:%M")

severe_inc$Arrival_time <- as.POSIXct(severe_inc$ARRIVAL_DATE_TIME,format="%m/%d/%Y %H:%M")

severe_inc$Duration <- difftime(severe_inc$Arrival_time,severe_inc$Incident_time,units = "mins")

A <- matrix(c(severe_inc$Longitude,severe_inc$Latitude), ncol=2)
B <- matrix(c(Firehouses$Longitude,Firehouses$Latitude), ncol=2)


C <- matrix(c(distHaversine(B,A, r=6378137)),nrow=218)

UniqueCoordinates <- data.frame(severe_inc[,26:25])
UniqueCoordinates$Id <- formatC((1:nrow(UniqueCoordinates)), width=3,flag=0)


## Generate a function that looks for the closest Firestation for each id coordinates
NearestFS <- function(id){
  tmp <- UniqueCoordinates[UniqueCoordinates$Id==id, 1:2]
  Firetemp <- rbind(tmp,Firehouses[,6:5])
  BIN <- earth.dist(Firetemp, dist=TRUE)[1:(nrow(Firetemp)-1)]
  BIN <- which.min(BIN)
  BIN <- Firehouses[BIN,10]
  BIN <- data.frame(BIN, Firetemp=tmp)
  return(BIN)
}

#apply to each id and the merge
CoordinatesFS <- ldply(UniqueCoordinates$Id, NearestFS)


Distancetemp <- sqldf("Select * from CoordinatesFS sev join Firehouses fire on sev.BIN = fire.BIN")

A <- matrix(c(Distancetemp$Longitude,Distancetemp$Latitude), ncol=2)
B <- matrix(c(Distancetemp$Firetemp.Longitude,Distancetemp$Firetemp.Latitude), ncol=2)

C <- matrix(c(distHaversine(A,B, r=6378137)))

severe_inc$DistanceNearestFS <- C

Ans4a <- ggplot(data = severe_inc, aes(x=DistanceNearestFS*0.00062137, y=Duration, color=DistanceNearestFS*0.00062137, alpha = 0.7 ))+
  geom_point(shape = 16, size = severe_inc$UNITS_ONSCENE/5, show.legend = FALSE) +
  geom_smooth(method="loess", se=F,show.legend = FALSE) + 
  xlim(c(0, 3)) + 
  ylim(c(0, 75)) +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")+
  theme_bw() +
  xlab("Nearest Firestation Distance(in Miles)")+
  ylab("Response Time")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Fire Station Distance vs Response time")

Ans4a

Ans4b <- leaflet(severe_inc) %>%
  addProviderTiles("Esri.NatGeoWorldMap",options = providerTileOptions(attribution = "")) %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius= ~Duration/4, fillOpacity=0.7,col = color1_Property, popup=~paste("Distance Nearest Firestation(m):",severe_inc$DistanceNearestFS,"<br>","Response time(mins):",severe_inc$Duration,"<br/>"))%>%
  addLegend(pal = pal, values = ~severe_inc$PROPERTY_USE_DESC, title = "Property Types",position="bottomright")%>%
setView(lng = -74.0156491, lat = 40.7022541, zoom = 10)%>%
  addEasyButton(easyButton(
    icon="fa-globe", 
    title="New York City",
    onClick=JS("function(btn, map){ map.setZoom(10); }")))

Ans4b
