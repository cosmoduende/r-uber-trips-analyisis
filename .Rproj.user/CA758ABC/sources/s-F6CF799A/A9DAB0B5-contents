
# REQUIRED LIBRARIES
library(dygraphs)
library(tidyquant)
library(tidyverse)
library(dygraphs)
library(plyr)
library(quantmod)
library(ggthemes)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(ggmap)
library(lubridate)
library(leaflet)
library(plotly)
library(dplyr)
library(mgsub)
library(xts)


# DATA READING
myTrips <- read.csv("my_trips_uber_history.csv", stringsAsFactors = FALSE)

myTrips$Request.Time <- as.Date(myTrips$Request.Time, "%Y-%m-%d")
myTrips$Year <- as.Date(cut(myTrips$Request.Time, breaks="month"))


# TRAVEL TIMELINE BY CITY
timeline <- ggplot(myTrips, aes(Year, City))+
  geom_line(color = "#006790", size = 6) + 
  labs(x= "Year", y= "City") + 
  ggtitle("Cities where I have requested Uber services", "Full history timeline")
timeline
ggplotly()


# FINAL STATUS OF THE ORDER
orderStatus<-ggplot(myTrips, aes(City,fill = Trip.or.Order.Status)) + 
  labs(x = "City", y = "Number of requests") + 
  ggtitle("Final status of Uber service requests", "Order status by city") +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(name = "Final status", palette="Set1")
orderStatus
ggplotly()


# FINAL STATUS OF THE ORDER CONCERNING THE PRODUCT TYPE
productType = 
  myTrips %>%
# HOMOGENIZING INCONSISTENCIES IN PRODUCT NAMES
  filter(Product.Type != "All" & Product.Type != "" ) 

productType <- mgsub::mgsub(productType, c("uberX", "UberEATS Marketplace", "Pool"), c("UberX", "UberEATS - Marketplace", "uberPOOL"))
productType <- mgsub::mgsub(productType, c("uberPOOL: MATCHED"), c("uberPOOL"))

orderProductType <- qplot(Trip.or.Order.Status, data=productType, geom="bar", fill= Product.Type) +
  scale_fill_brewer(name = "Product type", palette="Set1")+
  labs(x = "Final order status", y = "Number of requests") + 
  ggtitle("Final status of Uber service requests", "Order status concerning the Product type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
orderProductType
ggplotly()


# MAP OF UBER REQUESTS IN MEXICO CITY
register_google(key = "YOUR_API_KEY")
MexicoCity <- subset(myTrips, City=="Mexico City")
ggmap(get_map(location = "Mexico City", zoom=11, maptype = "roadmap")) + 
  geom_point(aes(Begin.Trip.Lng, Begin.Trip.Lat), data=MexicoCity, color = I('Red'), size = I(2), zoom=11) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Locations where Uber was requested", "Mexico City")
ggplotly()


# DENSITY MAP OF UBER REQUESTS IN MEXICO CITY
qmplot(Begin.Trip.Lng, Begin.Trip.Lat, data = MexicoCity, geom = "blank", 
  zoom = 11, extent = "panel", maptype = "toner-background", darken = .7, legend = "right") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Level", low = "white", mid = "yellow", high = "red", midpoint = 100) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Locations where Uber was requested", "Mexico City Density Map")
ggplotly()


# HOW MUCH I PAID TO UBER FROM 2015 TO 2020
totalPaid <- ggplot(myTrips, aes(x=Dropoff.Time, y=Fare.Amount, Fare.Currency = Fare.Currency)) +
  geom_point(aes(col=Fare.Amount, size=Fare.Amount)) + 
  labs(col="Amount", size="size")+
  labs(x = "Timestamp", y = "Fare amount") + 
  ggtitle("How much have I paid for Uber services?", "From 2015 to 2020")+
  guides(size=FALSE)
totalPaid
ggplotly(totalPaid, tooltip = c("Dropoff.Time", "y", "Fare.Currency"))


# HOW MUCH I SPENT ON UBER IN 2020
myTrips2020 <- myTrips %>% 
  filter(Request.Time >= as.Date("2020-01-01") & Request.Time <= as.Date("2020-07-13"))

min(myTrips2020$Request.Time)
max(myTrips2020$Request.Time)

paid2020 <- ggplot(myTrips2020, aes(Request.Time, Fare.Amount))+ 
  geom_bar(stat = 'identity', fill = 'darkorange2', width=1) +
  labs(x = "Date", y = "Total spent per day") + 
  ggtitle("How much have I spent on Uber in 2020?", "From January to July")
paid2020
ggplotly(paid2020, tooltip = c("Request.Time", "Fare.Amount", "Fare.Currency", "City"))


# TRAVELED DISTANCE
distanceRides <- read.csv("my_trips_uber_history.csv")
distanceRides$Request.Time <- ymd_hms(distanceRides$Request.Time)
distanceRides$Dropoff.Time <- ymd_hms(distanceRides$Dropoff.Time)
rides = xts(x=distanceRides$Distance..miles., order.by = distanceRides$Request.Time)

dygraph(rides, main = "Distances traveled from 2015 to 2020") %>%
  dyOptions(drawPoints = TRUE, pointSize = 5, colors="#1a954d") %>%
  dyRangeSelector() %>%
  dyAxis("y", label= "Distance (km.)") %>%
  dyHighlight(highlightCircleSize = 0.5,
              highlightSeriesBackgroundAlpha = 1)


