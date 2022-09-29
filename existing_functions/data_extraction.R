
#Function to get locations:


# This function will require a VPN connections to server and
# will give a dataframe that contain all the locations for the tags listed in the ListOfStart dataframe:
#Required impute Parameters:
#1) Start_Time_Str: Starting date in the following format 'YYYY-MM-DD HH:MM:SS' (e.g.,'2020-03-31 06:00:00'), in UTC
#2) End_Time_Str: End date in the following format 'YYYY-MM-DD HH:MM:SS'(e.g.,'2020-11-18 06:00:00'), in UTC
#3) ListOfStart: dataframe with the following variables: 
# TAG as int with two or three digits (e.g., 46,47,..,147)
# 4) TagPrefix: The initial value for the atlas tag numbers, the defult is Harod (972006000000) 
# 5) quaryVar: The variable required  from the atlas server in a comma seperated chr format, the defult is 'TAG,TIME,X,Y,VARX,VARY,COVXY'.

# Output parameters:
# Dataframe with 17 variables:
# 1) X,Y: Locations as num
# 2) TIME: ATLAS detection time as number format
# 3) TAG: tag name as character with three digits (e.g., "047",..."194")
# 4) date: Date of detection (as Date format)
# 5) VARX,VARY,COVXY
# 6) LON,LAT: Locations as ITM
# 7) dateTime: Real time formate as POSIXct ("2020-07-28 06:00:05")
# 8)  dT: Detection time
# 9) distance
# 10) spd
# 11) stdVarXY
# 12) angl
# 13) traceNorm
# Notes: 
# 1) The function will search in the server all the tags specified in ListOfStart
# 2) This function work for the Harod system (in the connection to server)
# This will also give the important atributes for later (speed, angle ext..)

LocationRawData <- function(Start_Time_Str,End_Time_Str,ListOfStart,TagPrefix=972006000000,quaryVar='TAG,TIME,X,Y,VARX,VARY,COVXY') {
  
  source("../Functions-main/ConnectLib.R")
  
  # Functions to interact with databases
  # --- Database Connection 
  dbc <- dbConnect(RMySQL::MySQL(),
                   user = 'roatlasharod',            # username 
                   password = 'roatlasHarodOrr5678#',# password
                   host = '132.66.79.21',            # host ip address
                   port=5900,                        # port Number
                   dbname='harod')                   # name of data base
  
  
  # --- Set start & end time and convert to ATLAS time
  
  Start_Time_Str_Temp <- as.character.Date(Start_Time_Str) 
  ATLAS_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str_Temp,
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000
  
  End_Time_Str_Temp <- as.character.Date(End_Time_Str)
  ATLAS_End_Time<-as.numeric(as.POSIXct(End_Time_Str_Temp,
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000 
  
  #Make the tag list as full tag name:
  
  FullTag <- as.character(ListOfStart$TAG+TagPrefix)  #Create a list with only the tags
  
  AllTagsLoc <- list() #make an empty list for localizations
  
  for (i in 1:length(FullTag)) {
    query = paste('select', quaryVar, 'from LOCALIZATIONS WHERE TAG=',FullTag[i],
                  'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
    All_Data <- dbGetQuery(dbc,query)
    AllTagsLoc[[i]] <- All_Data
    print(paste(i,"Tags out of", length(FullTag), "are done"))
  }
  
  print(paste("Binding all tags, might take some time"))
  
  AllthetagsLoc <- do.call(rbind.data.frame, AllTagsLoc)
  
  dbDisconnect(dbc)
  
  # Make the locations as ITM
  AllthetagsLoc <-convertSpatial.ITM2WGS84(AllthetagsLoc, xyColNames=c("X","Y"))
  AllthetagsLoc <- as.data.frame(AllthetagsLoc)
  
  # Substruct the tag names
  AllthetagsLoc$TAG <- substr(AllthetagsLoc$TAG, 10, 13)
  
  AllthetagsLoc<-AllthetagsLoc[order(AllthetagsLoc$TAG,AllthetagsLoc$TIME),] #make sure data is sorted chronologically (per tag)
  AddTrubite <- list()
  print(paste("Adding atributes"))
  FullTag2 <- unique(AllthetagsLoc$TAG)
  for (i in FullTag2){
    tag <- subset(AllthetagsLoc, TAG == i)
    DF <-addLocAttribute(tag, locAttributs=c( "locQuality"))
    AddTrubite[[i]] <- DF
  }
  
  RawData <- do.call(rbind.data.frame, AddTrubite)
  print(paste("Done!"))
  
  return(RawData)
}

library(dplyr)

#Working with the data:

best_tags <- c("444", "610", "476", "422", "411", "458", "416", "455", "474",
               "421", "415", "414", "409", "615", "406", "612", "472", "479",
               "608", "483", "407", "616", "405")

ListOfStart <- read.csv("../inputs/TAG_dates.csv") %>%
  filter(TAG %in% best_tags)

LocRaw2022 <- LocationRawData('2019-04-12 06:00:00','2022-04-25 07:00:00',ListOfStart) # Download the data I want
