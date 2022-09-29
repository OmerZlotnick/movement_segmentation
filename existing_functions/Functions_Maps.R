
#All the libraries we need to work with mapping. Include "toolsForAtlas"

LibrariesForAtlas <- function() {
  # Load libraries
  library('adehabitatHR')
  library('maptools')
  library('mapview')
  library('leaflet')
  library(viridis)
  library(ggridges)
  library("SpatialTools")
  library(dplyr)
  library(tidyr)
  library(scales) 
  library(rgeos)
  library(sf)
  library(ptinpoly)
  library(reshape2)
  library(ggpubr)# package needed to the ATLAS package (for plotting)
  library(htmltools) # to add "pop-ups" to leaflet maps
  library(dbscan) # clustering algorithm
  #library(toolsForAtlas)
  library(RMySQL)
  library(ggplot2)
  library(leaflet)
  library(sp)
  library(rgdal)
  library(lubridate)
  library(plyr)
  library(RSQLite)
  library(shiny)
  library(dismo)
  library(readxl)
  library("gplots")
}

####Functions for metadata plotting:####

##(1) This function give a dataframe with a summary of each tag. It have
# an option to create a plot depending on your values. 

# Inputs:
# -(a) Data = The dataframe with the locations.
#Column names needed in Data: "TAG" (as factor),"dateTime" (as POSIXct), "LON", "LAT".
# - (b) ListOfStart - A dataframe with a list of tags, and when they were captured,
# Column names needed in ListOfStart: "TAG", "CaptureDate".
# - (c) plot = TRUE - by definition the function will create a plot.
# If only the table is needed write "plot = FALSE". 
# The plot have a time-table, with X axis is the time, while Y axis is the tag name.
# Each line represent the total time the tag was active
# numbers in the end of the line are the total days the tag was active.

TagsCondition <- function(Data,ListOfStart, plot = TRUE) {
  
  taillistDet <- list() #empty list
  taillistLoc <- list() #empty list
  Lisoftags <- unique(Data$TAG)
  #LastDetLoc
  
  # The following loop captures the last row of the locations from the Data for each tags
  
  for(i in 1:length(Lisoftags)) {
    d3 <- subset(Data, TAG==Lisoftags[i])
    d4 <- tail(d3, 1)
    d4 <- subset(d4,  select=c("TAG", "dateTime", "LON", "LAT"))
    taillistLoc[[i]] <- d4
    print(paste(i,"Tags out of", length(Lisoftags), "are done"))
  }
  
  Lastloc <- do.call(rbind.data.frame, taillistLoc)
  
  names(Lastloc)[names(Lastloc) == "dateTime"] <- "LastLocetionTime"

  # This will create tags as factors with three digits to combine the CSV file with the localizations and ditections

  ListOfStart$TAG  <- as.factor(ListOfStart$TAG)
  
  Lastloc2 <- merge(Lastloc, ListOfStart, by = 'TAG')
  names(Lastloc2)[names(Lastloc2) == "LON"] <- "LastLON"
  names(Lastloc2)[names(Lastloc2) == "LAT"] <- "LastLAT"
  names(Lastloc2)[names(Lastloc2) == "start_hour"] <- "TagStart"
  names(Lastloc2)[names(Lastloc2) == "date_capture"] <- "CaptureDate"
  
  Lastloc2<-Lastloc2[order(Lastloc2$TAG),] #make sure data is sorted chronologically (per tag)
  
  Lastloc2$FullLocStart <- paste(Lastloc2$CaptureDate, Lastloc2$TagStart, sep = " ")
  Lastloc2$FullLocStart <- as.POSIXct(Lastloc2$FullLocStart, format="%d/%m/%Y %H:%M:%S", tz="UTC")
  
  
  #Start_to_loc
  Lastloc2$Start_To_Loc <- as.numeric(difftime(Lastloc2$LastLocetionTime, Lastloc2$FullLocStart, units = "days"))
  Lastloc2$Start_To_Loc <- round(Lastloc2$Start_To_Loc, digits = 0)
  Lastloc2$Year <- substr(Lastloc2$CaptureDate, 7,10)
  Lastloc2$TAG <- as.factor(Lastloc2$TAG)
  
  return(Lastloc2)
  
  if (plot == TRUE) {
    
    for (i in unique(Lastloc2$Year)) {
      Condi <- ggplot(subset(Lastloc2, Year == i), aes(y = TAG)) +
        geom_point(aes(x = FullLocStart), 
                   color = "Black",
                   alpha = 1,
                   size = 2) +
        labs(x = "Date", 
             y = "Tags",
             title = "Tags life spane") +
        theme_minimal() + 
        geom_point(aes( x = LastLocetionTime)) +
        geom_segment(aes(x = FullLocStart,
                         y = TAG,
                         xend = LastLocetionTime,
                         yend = TAG),
                     color = "Black",
                     size = 1) +
        theme(legend.position = "bottom")  + 
        geom_text(aes( x = LastLocetionTime, label=Start_To_Loc), vjust=-0.2, size=2.8) + 
        facet_grid( ~ Year, scales='free')
      
      
      
      
      print(Condi)
      print(paste("In year", i ," Location summary:"))
      print(summary(subset(Lastloc2, Year == i)$Start_To_Loc))
     
    }
    
  
  }
}

#(2) This function give a dataframe a plot representing the total tags that were active in certain dates.
# Inputs:
# Data: Need to have the columns "dates" (as POSIXct), and "TAG" (as factor).
# The X axis show the time, while Y axis is the total of tags that work in those dates.
# color differ by years.

TimeTotalTagsPlot <- function(Data) {
  library(directlabels)
  
  Data$Year <- format(Data$date, format="%Y")

  years <- unique(Data$Year)
  years <- years[!is.na(years)]
  listofcolors <- topo.colors(length(years))
  
  aggplo <- ggplot()
  
  for (i in 1:length(unique(years))) {
    print(paste("aggregate locations in year", years[i], "..."))
    agg1 <- ddply(subset(Data, Year == years[i]),~date,summarise,number_of_distinct_orders=length(unique(TAG)))
    agg1$row_num <- seq.int(nrow(agg1))
    aggplo <- aggplo + geom_line(data = agg1, aes( x = date, y = number_of_distinct_orders),size=0.8, color = listofcolors[i])
  }
  
  aggplo <- aggplo + theme_minimal() + ylab("Tags working") + 
    xlab("Date")

  print(aggplo)
  return(aggplo)
}

####Functions for mapping:####

##(1) Show a chosen tag on the map separated by dates with polylines.

# (a) Data = The dataframe with the locations.
# Inputs: "TAG" (As factor), date (as POSIXct), "X","Y".
# (b) Tag = The tag name you want to test (example: Tag = "171")
# (c) Color = The color of the line. by defult it is red

ShowDateOnMap <- function(Data,Tag,Color = "Red") {
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  
  TagChoosen <-subset(Data, TAG == Tag)
  
  #TagChoosen <- AssignDayNumber(TagChoosen)
  
  DAYLists <- as.factor(unique(TagChoosen$date))
  m <- leaflet() %>% addProviderTiles("Esri.WorldImagery")
  DaysList <- list()
  for (i in DAYLists) {
    DayLo <- subset(TagChoosen, date == i)
    coordinates(DayLo)<-~X+Y
    proj4string(DayLo)<-CRS(itm)
    llpd1 <- spTransform(DayLo, wgs84)
    m <-  addPolylines(m, data=llpd1@coords, weight = 2, opacity = 2,group = i, color = Color)
  }
  
  daylistst <- as.factor(as.data.frame(DaysList))
  
  m <- addLayersControl(m,
                        baseGroups = DAYLists,
                        options = layersControlOptions(collapsed = FALSE) 
  )
  return(m)
}

##(2) The function created in the presentation. Show different aspect of
#movement behaviors, including separating time to night and day, flying as icon,
# Adding a legend, scale and measuring options.

# (a) Data = The dataframe with the locations.
# Inputs: "TAG" (As factor), date (as POSIXct), "X","Y".
# (b) TAGname = The tag name you want to test (example: Tag = "171")
# (c) FlyIcon = The icon of the flying locations.Flying is seperated by 6m/s.
MovementPresent <- function(data,TAGname,FlyIcon) {
  
  itm<-"+init=epsg:2039"
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  m <- leaflet() %>% addProviderTiles('OpenStreetMap')
  
  #Next add the data you want to see on the map. Can be dots, or line, or polygon
  data <- subset(data, TAG == TAGname)
  datesoflapwings <- unique(data$date)
  
  data$HourDay[hour(data$dateTime) >= 5 & hour(data$dateTime) < 18] <- "Day (05:00-18:00)"
  data$HourDay[hour(data$dateTime) >= 18] <- "Night (18:00-05:00)"
  data$HourDay[hour(data$dateTime) <= 5] <- "Night (18:00-05:00)"
  
  for (ii in 1:length(datesoflapwings)) {
    Dayformap <- subset(data, date == datesoflapwings[ii]) #I choose a day I want to see
    
    coordinates(Dayformap)<-~X+Y
    proj4string(Dayformap)<-CRS(itm)
    llpd1 <- spTransform(Dayformap, wgs84)
    m <-  addPolylines(m, data=llpd1@coords, weight = 2, opacity = 2, color = "red",group = datesoflapwings[ii])
    
    flyingseg <- subset(llpd1,spd > 8)
    GroundSeg <- subset(llpd1,spd <= 8)
    
    
    pal <- colorFactor(c("yellow","steelblue4"), domain = c("Day (05:00-18:00)","Night (18:00-05:00)"),
                       ordered = TRUE)
    
    
    l <- addMarkers(m,data=flyingseg, icon = ~FlyIcon,group = datesoflapwings[ii],
                    label = ~paste("Tag number:" ,TAG, "Time:", substr(dateTime, 12, 19)),
                    popup = ~paste("Tag number:" ,TAG, "<br>Time:", substr(dateTime, 12, 19),
                                   "<br>Detection time:", dT,
                                   "<br>Speed (m/s)", round(spd, 3)))  %>%  
      addCircleMarkers(data=GroundSeg, color =~pal(HourDay), radius = 3,stroke = FALSE, fillOpacity = 1,group = datesoflapwings[ii],
                       label = ~paste("Tag number:" ,TAG, "Time:", substr(dateTime, 12, 19)),
                       popup = ~paste(TAG, "<br>Time:", substr(dateTime, 12, 19),
                                      "<br>Detection time:", dT,
                                      "<br>Speed (m/s)", round(spd, 3)))
    
    m <- addLayersControl(l,
                          baseGroups = datesoflapwings,
                          options = layersControlOptions(collapsed = FALSE))
    
    
  }
  
  m <- addLegend(m,"topleft", data=data,pal = pal, values = ~HourDay, opacity = 1, labels = ~ sort(HourDay),
                 title = "Time of day") %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479") %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200,imperial = FALSE))
  return(m)
}

##(3) Show multiple tags in the same time
# (a) Data = The dataframe with the locations.

ShowIndOnMap <- function(Data) {
  
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  
  Taglist <- as.factor(unique(Data$TAG))
  m <- leaflet() %>% addProviderTiles("Esri.WorldImagery")
  
  listofcolors <- topo.colors(length(Taglist))
  
  DaysList <- list()
  for (i in 1:length(Taglist)) { #Creating a loop for the tags
    DayLo <- subset(Data, TAG == Taglist[i])
    coordinates(DayLo)<-~X+Y
    proj4string(DayLo)<-CRS(itm)
    llpd1 <- spTransform(DayLo, wgs84)
    m <-  addPolylines(m, data=llpd1@coords, weight = 2, opacity = 2,group = Taglist[i], color = listofcolors[i])
  }
  
  m <- addLayersControl(m,
                        overlayGroups = Taglist,
                        options = layersControlOptions(collapsed = FALSE)) %>% 
    addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 200,imperial = FALSE)) 
  return(m)
}

##(4) create and Map homerange of multiple tags.
# (a) Data = The dataframe with the locations.

HomeRangeMapping <- function(Data) {
  
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  MinuteFilter <- SubsetByTime(YourData = Data, SubsetTime = "60 mins")
  
  xyt<-subset(MinuteFilter, select = c(X,Y))
  id<-subset(MinuteFilter, select = TAG)
  locs1<-id
  coordinates(locs1)<-xyt
  
  tagLists <- as.factor(unique(MinuteFilter$TAG))
  
  m <- leaflet() %>% addProviderTiles('Esri.WorldImagery') 
  for (i in tagLists) {
    
    
    TagLo <- subset(locs1, TAG == i)
    
    UD3 <- kernelUD(TagLo[,1], h = "href", grid = 500,same4all = FALSE, hlim = c(0.1, 2), kern = "bivnorm", extent = 1,boundary = NULL)
    homerange1 <- getverticeshr(UD3, percent = 95)
    homerange1_Core <- getverticeshr(UD3, percent = 50)
    
    proj4string(homerange1)<-CRS(itm)
    proj4string(homerange1_Core)<-CRS(itm)
    
    llpd <- spTransform(homerange1, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    llpd2 <- spTransform(homerange1_Core, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    m <-  addPolygons(m, data=llpd, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8 ,group = i, color = "yellow") %>%
      addPolygons(data=llpd2, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8 ,group = i, color = "red") 
    print(paste("Tag",unique(homerange1_Core$id),"area:", unique(round(homerange1$area),2), "core:", unique(round(homerange1_Core$area),2)))
  }
  
  
  
  m <- addLayersControl(m,
                        overlayGroups = tagLists,
                        options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(
      position = "topleft",
      options = scaleBarOptions(maxWidth = 300,
                                metric = TRUE,
                                imperial = FALSE,
                                updateWhenIdle = TRUE))
  
  return(m)
}

##(5) This function is needed to run the 'HomeRangeMapping' function. It subset the data by 60 min.

SubsetByTime <- function(YourData,date, dateTime, TAG, X,  Y,TIME, SubsetTime) {
  library(dplyr)
  library(data.table)
  library(lubridate)
  Data <- YourData
  tags <- unique(Data$TAG)
  
  Listone <- list()
  for (i in tags) {
    LoopTag <- subset(Data, TAG == i)
    daysfilter <- as.factor(unique(LoopTag$date))
    listtwo <- list()
    for (ii in daysfilter) {
      LoopDay <- subset(LoopTag, date == ii)
      #LoopDay <- LoopDay[ , c("dateTime","date","TAG","X","Y")]  
      LoopDay <- setDT(LoopDay)[order(LoopDay)]
      
      output <- LoopDay[, .(DateTime = dateTime[1],date = date[1], TAG = TAG[1], X = X[1], Y = Y[1], TIME = TIME[1]) , 
                        by = .(Group = floor_date(dateTime, SubsetTime))]
      listtwo[[ii]] <- output
    }
    dataframetwo <- do.call(rbind.data.frame, listtwo)
    Listone[[i]] <- dataframetwo
  }
  NewLocations <- do.call(rbind.data.frame, Listone)
  NewLocations<-NewLocations[order(NewLocations$TAG,NewLocations$TIME),] #make sure data is sorted chronologically (per tag)
  
  return(NewLocations)
}

##(7) Combine the movement data with a polygon - showing what type of habitats are inside the homerange.

HR_With_Habitat <- function(Data, Raster) {
  
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  SubsetedData <- SubsetByTime(Data, SubsetTime = "60 mins") #Subseting the data to 60 minutes
  
  ### Creating home-ranges for the lapwings:
  xyt<-subset(SubsetedData, select = c(X,Y))
  id<-subset(SubsetedData, select = TAG)
  locs1<-id
  coordinates(locs1)<-xyt
  
  #Creating the home-range of kernel density for the choosen birds:
  
  UD3 <- kernelUD(locs1[,1], h = "href", grid = 500,same4all = FALSE, hlim = c(0.1, 2), kern = "bivnorm", extent = 1,boundary = NULL)
  
  homerange1 <- getverticeshr(UD3, percent = 95)
  homerange1_Core <- getverticeshr(UD3, percent = 50)
  
  proj4string(homerange1)<-CRS(itm)
  proj4string(homerange1_Core)<-CRS(itm)
  
  #Creating the home-range of kernel density:
  
  
  Raster2 <- projectRaster(Raster, crs=itm)
  Combined2 <- raster::intersect(Raster2,homerange1)
  r3 <- mask(Combined2, homerange1) #Removing unwanted locations:
  
  #Mapping the home-ranges and looking what type of habitats are in their home range
  Mapswithpol <- MapWithPolygon(homerange1,homerange1_Core,Raster2)
  
  return(Mapswithpol)
}

##(6) Function needed to run the 'HR_With_Habitat' function. 
#Can run separately. Add a home-range data (95%) and a core data (50%) and the wanter raster.

MapWithPolygon <- function(homerange,homerange_Core,Raster2) {
  
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  m <- leaflet() %>% addProviderTiles('Esri.WorldImagery')
  TAGId <- as.factor(unique(homerange@data$id))
  
  for (i in TAGId) {
    animalnum95 <- homerange[i,]
    animalnum50 <- homerange_Core[i,]
    
    #Adding area for later:
    size95 <- animalnum95$area
    size50 <- animalnum50$area
    #Combining the raster with the data:
    comination1 <- raster::intersect(Raster2,animalnum95)
    comination2 <- raster::intersect(Raster2,animalnum50)
    #Removing unwanted raster locations:
    r4 <- mask(comination1, animalnum95)
    r5 <- mask(comination2, animalnum50)
    palR <- colorNumeric(c("Blue","red","Green4", "Grey", "Brown"), values(Raster2),
                         na.color = "transparent") #Choose the colors of the raster    
    r4 <- projectRaster(r4, crs=itm)
    r5 <- projectRaster(r5, crs=itm)
    
    
    m <- addRasterImage(m, x = r4, opacity = 0.8,colors = palR, project=FALSE, group = i) %>% addRasterImage(x =r5, opacity = 0.9,colors = palR, project=FALSE, group = i)
  }
  
  m <- addLayersControl(m,
                        overlayGroups = TAGId,
                        options = layersControlOptions(collapsed = FALSE))
  
  return(m)
  #library("htmlwidgets")
  #saveWidget(m, file="HarodHRlapwings.html")
  
}
