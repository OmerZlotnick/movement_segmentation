# a function to read data directly from the ATLAS server
# requires a VPN connection to TAU servers or any other relevant server
# input variables:
    # Start_Time_Str - a time  sting in the format '2020-10-10 12:00:00' in UTC
    # End_Time_Str   - a time  sting in the format '2020-10-10 12:00:00' in UTC
    # FullTag        - a vector of Tags in the format 972006000223
    # SYS            - the name of the system (currently only Harod is implemented)
# return value:
    # returns a list of two data.frames, "DET", includes the detection with the period and "LOC" includes the localizations

Data_from_ATLAS_server <- function(Start_Time_Str,End_Time_Str,FullTag, SYS="Harod",includeDet=TRUE,includeLoc=TRUE)
{
  if (SYS=="Harod") #connects to Harod server
  {dbc <- dbConnect(RMySQL::MySQL(),
                   user = 'roatlasharod',            # username 
                   password = 'roatlasHarodOrr5678#',# password
                   host = '132.66.79.21',            # host ip address
                   port=5900,                        # port Number
                   dbname='harod')                   # name of data base
  }
  else
  {stop("system not defined")}
  
  # --- Examine the tables contained in the database 
  # dbListTables(dbc)           
  
  # --- Examine the names of the columns in a table
  # dbListFields(dbc, 'DETECTIONS')
  # dbListFields(dbc, 'LOCALIZATIONS')
  
  # --- Set start & end time and convert to ATLAS time

  Start_Time_Str_Temp <- as.character.Date(Start_Time_Str) 
  ATLAS_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str_Temp,
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000
  End_Time_Str_Temp <- as.character.Date(End_Time_Str)
  ATLAS_End_Time<-as.numeric(as.POSIXct(End_Time_Str_Temp,
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000 

  AllTagsDet <- list() #make an empty list for detections
if(includeDet)  
  {  
  for (i in 1:length(FullTag)) 
    { # build a  DETECTIONS query for the system, the results include the variables listed below
    query = paste('select TAG,TIME,BS,RSSI,GAIN,SNR,SAMPLES_CLK from DETECTIONS WHERE TAG=',FullTag[i],
                  'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
    All_Data <- dbGetQuery(dbc,query)
    AllTagsDet[[i]] <- All_Data
    }
  }
  
  
  AllTagsLoc <- list() #make an empty list for localizations
if(includeLoc)    
  {
  for (i in 1:length(FullTag)) 
    { # build a  LOCALIZATIONS query for the system, the results include the variables listed below # NCONSTRAINTS
    query = paste('select TAG,TIME,X,Y,Z,VARX,VARY,COVXY,NBS,PENALTY from LOCALIZATIONS WHERE TAG=',FullTag[i], 
                  'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
    All_Data <- dbGetQuery(dbc,query)
    AllTagsLoc[[i]] <- All_Data
    }
  }
  dbDisconnect(dbc)
  RawDet0 <- do.call(rbind.data.frame, AllTagsDet)
  RawLoc0 <- do.call(rbind.data.frame, AllTagsLoc)
  A <- list("DET"=RawDet0,"LOC"=RawLoc0)
  return(A)
}