library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(dismo)
library(plotly)
# this sets your google map for this session
# register_google(key = "AIzaSyATDKUI5pei71HRijEAPqW8MAoaQnDd-YI")

# this sets your google map permanently
# register_google(key = "AIzaSyATDKUI5pei71HRijEAPqW8MAoaQnDd-YI", write = TRUE)

# has_google_key()
# google_key()
# has_google_client()
# has_google_signature()
# 
# geocode("waco, texas", urlonly = TRUE)
# ggmap_show_api_key()
# geocode("waco, texas", urlonly = TRUE)
# ggmap_hide_api_key()
# geocode("waco, texas", urlonly = TRUE)
# 
# scrub_key("key=d_5iD")
# scrub_key("key=d_5iD", "[your \\1]")
# scrub_key("signature=d_5iD")
# scrub_key("client=a_5sS&signature=d_5iD")

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


# CREATING COORDINATES LINES FOR EACH WALL OF BUILDING 2
train <- read.csv("./datasets/trainingData.csv")
train <- train %>% filter(BUILDINGID==2)
plot_ly(data = train, x = ~LONGITUDE, y = ~LATITUDE)

# LEFT WALL OF THE RIGHT SIDE
lon_left_right <- c(-7304.484, -7347.108)
lat_left_right <- c(4864821, 4864748)
df_left_right <- as.data.frame(cbind(lon_left_right,lat_left_right))

# group rows by opposite pairs
df2_left_right <- df_left_right %>% group_by(group = lon_left_right %in% range(lon_left_right)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_left_right, x = ~lon_left_right, y = ~lat_left_right)


# RIGHT WALL OF THE RIGHT SIDE
lon_right_right <- c(-7300.819, -7343.871)
lat_right_right <- c(4864818, 4864746)
df_right_right <- as.data.frame(cbind(lon_right_right,lat_right_right))

# group rows by opposite pairs
df2_right_right <- df_right_right %>% group_by(group = lon_right_right %in% range(lon_right_right)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_right_right, x = ~lon_right_right, y = ~lat_right_right)


# SOUTH WALL OF THE SOUTH SIDE
lon_south_south <- c(-7343.871, -7413.921)
lat_south_south <- c(4864746, 4864785)
df_south_south <- as.data.frame(cbind(lon_south_south,lat_south_south))

# group rows by opposite pairs
df2_south_south <- df_south_south %>% group_by(group = lon_south_south %in% range(lon_south_south)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_south_south, x = ~lon_south_south, y = ~lat_south_south)


# NORTH WALL OF THE SOUTH SIDE
lon_north_south <- c(-7341.215, -7410.1)
lat_north_south <- c(4864756, 4864795)
df_north_south <- as.data.frame(cbind(lon_north_south,lat_north_south))

# group rows by opposite pairs
df2_north_south <- df_north_south %>% group_by(group = lon_north_south %in% range(lon_north_south)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_north_south, x = ~lon_north_south, y = ~lat_north_south)


# LEFT WALL OF THE LEFT SIDE
lon_left_left <- c(-7415.163, -7376.317)
lat_left_left <- c(4864796, 4864862)
df_left_left <- as.data.frame(cbind(lon_left_left,lat_left_left))

# group rows by opposite pairs
df2_left_left <- df_left_left %>% group_by(group = lon_left_left %in% range(lon_left_left)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_left_left, x = ~lon_left_left, y = ~lat_left_left)


# RIGHT WALL OF THE LEFT SIDE
lon_right_left <- c(-7410.1, -7380.907)
lat_right_left <- c(4864796, 4864847)
df_right_left <- as.data.frame(cbind(lon_right_left,lat_right_left))

# group rows by opposite pairs
df2_right_left <- df_right_left %>% group_by(group = lon_right_left %in% range(lon_right_left)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_right_left, x = ~lon_right_left, y = ~lat_right_left)


# SOUTH WALL OF THE NORTH SIDE
lon_south_north <- c(-7380.907, -7312.662)
lat_south_north <- c(4864847, 4864808)
df_south_north <- as.data.frame(cbind(lon_south_north,lat_south_north))

# group rows by opposite pairs
df2_south_north <- df_south_north %>% group_by(group = lon_south_north %in% range(lon_south_north)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_south_north, x = ~lon_south_north, y = ~lat_south_north)


# NORTH WALL OF THE NORTH SIDE
lon_north_north <- c(-7376.317, -7300.819)
lat_north_north <- c(4864862, 4864818)
df_north_north <- as.data.frame(cbind(lon_north_north,lat_north_north))

# group rows by opposite pairs
df2_north_north <- df_north_north %>% group_by(group = lon_north_north %in% range(lon_north_north)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 100)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

# plotting the intermediate coordinates line
plot_ly(data = df2_north_north, x = ~lon_north_north, y = ~lat_north_north)




# CONVERTING UTM to lat/long (for plotting in a real google map)

# install.packages("sp")
# install.packages("rgdal")
# install.packages("raster")
library(sp)
library(rgdal)
library(raster)

# loading my dataset and removing all unnecesary columns to make tests
train <- read.csv("./datasets/trainingData.csv")
train <- train %>% filter(BUILDINGID==2)
plot_ly(data = train, x = ~LONGITUDE, y = ~LATITUDE)


train[,1:520] <- NULL
train[,3:9] <- NULL
train

# Method 1
utmcoor<-SpatialPoints(cbind(train$LATITUDE,train$LONGITUDE), proj4string=CRS("+proj=utm +zone=30 +datum=WGS84 +units=m +ellps=WGS84"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))

train$lat <- coordinates(longlatcoor)[,1]
train$lon <- coordinates(longlatcoor)[,2]


plot_ly(data = train, x = ~lon, y = ~lat)


# real central coordinates of Jaume I university (not the same! ;( )
univ <- c(-0.067417, 39.992871)

# getting google map
map <- get_map(univ, zoom = 17, scale = 1)
ggmap(map)












#### TESTING - RESTRICTED AREA ----------------------------------------------------------------------------------------------------

# example of https://stackoverflow.com/questions/39580566/determine-which-points-lay-outside-an-irregularly-shaped-data-footprint-in-r

install.packages("manipulate")
library(manipulate)
library(sp)

set.seed(1)
par(pch = 19, cex=.5)
x <- runif(1000)
y <- runif(1000)


coords <- data.frame()
manipulate({
  plot(y~x)
  res <- manipulatorMouseClick()
  coords <<- rbind(coords, data.frame(x=res$userX, y=res$userY))
  if (length(coords)) lines(coords)
})

res <- point.in.polygon(x, y, coords$x, coords$y)!=0 

plot(y~x, col = res + 1L)
lines(coords)

# with my data

set.seed(1)
par(pch = 19, cex=.5)
x <- runif(1000)
y <- runif(1000)


coords <- data.frame()
manipulate({
  plot(y~x)
  res <- manipulatorMouseClick()
  coords <<- rbind(coords, data.frame(x=res$userX, y=res$userY))
  if (length(coords)) lines(coords)
})

res <- point.in.polygon(x, y, coords$x, coords$y)!=0 

plot(y~x, col = res + 1L)
lines(coords)





# so... real coordinates are dataset coordinates + 0.1216307 in longitude $ +0.5329294 in latitude, then....
# charging prediction data set
prediction <- read.csv("./datasets/Pre-processed datasets/Prediction.csv")

# reomoving all columns but the longitude and latitude
prediction[,1:521] <- NULL
prediction[,3:12] <- NULL


# trasforming raw UTM longitude and latitude into lat/lon
utmcoor<-SpatialPoints(cbind(prediction$LATITUDE,prediction$LONGITUDE), proj4string=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat +datum=WGS84 +no_defs"))

prediction$LAT <- coordinates(longlatcoor)[,1]
prediction$LON <- coordinates(longlatcoor)[,2]

# trasforming predicted raw longitude and latitude into lat/lon
# utmcoor_pred<-SpatialPoints(cbind(prediction$LATT_PRED,prediction$LONG_PRED), proj4string=CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +ellps=WGS84"))
utmcoor_pred<-SpatialPoints(cbind(prediction$LATT_PRED,prediction$LONG_PRED), proj4string=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
# longlatcoor_pred<-spTransform(utmcoor,CRS("+proj=longlat"))
longlatcoor_pred<-spTransform(utmcoor,CRS("+proj=longlat +datum=WGS84 +no_defs"))
prediction$lat_pred <- coordinates(longlatcoor_pred)[,1]
prediction$lon_pred <- coordinates(longlatcoor_pred)[,2]


#Add latitude and longitude EPSG4236 for plotting on leaflet/map
# cord.EPSG3857 <- SpatialPoints(cbind(WAPlocations$LNG, WAPlocations$LAT), proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
# cord.EPSG4326 <- spTransform(cord.EPSG3857, CRS = "+proj=longlat +datum=WGS84 +no_defs")
# WAPlocations$mapLAT <- cord.EPSG4326@coords[,2]
# WAPlocations$mapLNG <- cord.EPSG4326@coords[,1]
# remove(cord.EPSG3857, cord.EPSG4326)


# removing UTM coordinates columns
prediction[,1:4] <- NULL

prediction

plot_ly(data = prediction, x = ~LON, y = ~LAT)




# adjusting difference between raw and real lat/lon coordinates







univ_center <- c(-0.05421372, 39.45994160)
map4 <- get_map(univ_center, zoom = 17, scale = 1)
ggmap(map4)


train
plot_ly(data = train, x = ~lon, y = ~lat)




# train <- train[complete.cases(train),]
# train1 <- data.frame(x=train$LONGITUDE,y=train$LATITUDE)
# coordinates(train1) <- ~x+y
# class(train1)
# proj4string(train1) <- CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +ellps=WGS84") 
# train2 <- spTransform(train1,CRS("+proj=longlat +datum=WGS84"))
# head(train2)
# train$long <- coordinates(train2)[,1]
# train$lat <- coordinates(train2)[,2]
# ?CRS

# Method 1
# LONGITUDE = NORTHING, LATITUDE = EASTING
# utm <- train %>% filter(BUILDINGID==2)
# utm <- utm[complete.cases(utm),]
# utm1 <- data.frame(x=utm$LONGITUDE,y=utm$LATITUDE)
# coordinates(utm1) <- ~x+y 
# class(utm1)
# proj4string(utm1) <- CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +ellps=WGS84") 
# utm2 <- spTransform(utm1,CRS("+proj=longlat +datum=WGS84"))
# head(utm2)




lon <- c(-64.723946, -64.723754, -64.723808, -64.724004)
lat <- c(32.350843, 32.350783, 32.350618, 32.350675)
df <- as.data.frame(cbind(lon,lat))

# group rows by opposite pairs
df2 <- df %>% group_by(group = lon %in% range(lon)) %>% 
  # summarize lat and lon for each group into a list of a sequence from the first to the second
  summarise_each(funs(list(seq(.[1], .[2], length.out = 10)))) %>% 
  # expand list columns with tidyr::unnest
  unnest()

head(df2)

# Source: local data frame [6 x 3]
# 
#   group       lon      lat
#   (lgl)     (dbl)    (dbl)
# 1 FALSE -64.72395 32.35084
# 2 FALSE -64.72393 32.35082
# 3 FALSE -64.72392 32.35079
# 4 FALSE -64.72390 32.35077
# 5 FALSE -64.72388 32.35074
# 6 FALSE -64.72387 32.35072

# all I changed here is data = df2
mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 18, maptype = "satellite", scale = 2)
ggmap(mapgilbert) +
  geom_point(data = df2, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  scale_x_continuous(limits = c(-64.725, -64.723), expand = c(0,0)) +
  scale_y_continuous(limits = c(32.350, 32.351), expand = c(0,0))














mapjaumei_b2 <- get_map(location = c(lon = mean(df_left_right$lon_left_right), lat = mean(df_left_right$lat_left_right)), zoom = 18, maptype = "satellite", scale = 2)
ggmap(mapjaumei_b2) +
  geom_point(data = df_left_right, aes(x = lon_left_right, y = lat_left_right, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  scale_x_continuous(limits = c(-0.052, -0.054), expand = c(0,0)) +
  scale_y_continuous(limits = c(39.459, 39.460), expand = c(0,0))
