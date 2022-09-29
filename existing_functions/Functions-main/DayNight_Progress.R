# The function calculates time progress in order to compare the activity along days / night with different starting time / length
#output:
#   A vector of progress percentiles:
#   the night is valued 0   to 100 from sunset  to sunrise
#   The day   is valued 100 to 200 from sunrise to sunset 
#input parameters:
#   a vector Time_POSI - with time in POSIXct format
#   a single value or a vector containing the corresponding coordinates Lat, Lon in geographic / wgs format

require("suncalc")
require("dplyr")
require("lubridate")
require("ggplot2")
DayNight_Progress <- function(Time_POSI, Lat, Lon) {
  if (!is.POSIXct(Time_POSI))
    {er <- errorCondition("the Time_POSI vector must be in POSIXct format")
    stop(er)}
  if (length(Lat)!=length(Lon))
    {er <- errorCondition("the Lat and Lon vectors must be of the same length")
    stop(er)}
  if (length(Lat)==1)
   { Lat <- rep(Lat,length(Time_POSI))
    Lon <- rep(Lon,length(Time_POSI))}
  if (length(Time_POSI)!=length(Lon))
    {er <-errorCondition("the coordinates and time vectors must be of the same length")
    stop(er)}
  tz_loc <- tz(Time_POSI)
  data <- data.frame(Time_POSI=Time_POSI,date=Time_POSI, lat=  Lat,lon= Lon)
  Sun_Alt = getSunlightPosition(data = data, keep = "altitude") # finding the angle of the sun above horizon
  data$date <- date(data$Time_POSI)
  data$Sun_Alt <- Sun_Alt$altitude
  data$Part = ifelse(data$Sun_Alt<=0, "Night", "Day") # defining each point as night or day
  OrderedData <- data[order(data$Time_POSI),]
  
  # the next three lines identifyes night/ date/date changes, 
  # the length of day and beginning of each segment (day/night in a date) is calculated once for each segment  for computational efficiency)
  
  OrderedData$diff <- OrderedData$Part!=lag(OrderedData$Part)|(date(OrderedData$Time_POSI)!=lag(date(OrderedData$Time_POSI))) 
  OrderedData <- OrderedData %>% mutate(partNum=cumsum(ifelse(is.na(diff), 0, diff)))
  Days <- OrderedData %>% group_by(partNum) %>%  slice(1) # takes only the first of each segment
  
  # calculates the segment length :  
  Days$length <- (
    ifelse(Days$Part=="Night", (ifelse(hour(Days$Time_POSI)>12, 
      
      {
        sunset1 <- getSunlightTimes(data = Days, keep = "sunset", tz = tz_loc)
        sunrise1 <- getSunlightTimes(data = data.frame(date = date(Days$Time_POSI)+1, lat = Days$lat, lon = Days$lon), keep = "sunrise", tz = tz_loc)
        as.numeric(difftime(sunrise1$sunrise, sunset1$sunset, units = "secs"))
      }, 
      {
        sunset2 <- getSunlightTimes(data = data.frame(date = date(Days$Time_POSI)-1, lat = Days$lat, lon = Days$lon), keep = "sunset", tz = tz_loc)
        sunrise2 <- getSunlightTimes(data = Days, keep = "sunrise", tz = tz_loc)
        as.numeric(difftime(sunrise2$sunrise, sunset2$sunset, units = "secs"))
      })), 
      {
        sunset3 <- getSunlightTimes(data = Days, keep = "sunset", tz = tz_loc)
        sunrise3 <- getSunlightTimes(data = Days, keep = "sunrise", tz = tz_loc)
        abs(as.numeric(difftime(sunrise3$sunrise, sunset3$sunset, units = "secs")))
      })
  )
  # calculates the segment  beginning time:   
  Days$start <- as.POSIXct(
    ifelse(Days$Part=="Night", (ifelse(hour(Days$Time_POSI)>12, 
                                       
                                       {
                                         sunset1 <- getSunlightTimes(data = Days, keep = "sunset", tz = tz_loc)
                                         sunset1$sunset
                                       }, 
                                       {
                                         sunset2 <- getSunlightTimes(data = data.frame(date = date(Days$Time_POSI)-1, lat = Days$lat, lon = Days$lon), keep = "sunset", tz = tz_loc)
                                         sunset2$sunset
          
                                       })), 
                                       {
                                         sunrise3 <- getSunlightTimes(data = Days, keep = "sunrise", tz = tz_loc)
                                         sunrise3$sunrise
                                       }),
    tz="UTC", origin="1970-01-01")

data <- left_join(data,distinct(dplyr::select(OrderedData,partNum,Time_POSI,lat,lon)),by=c("Time_POSI","lat","lon")) # joining the length and beginning time for all data 
data <- left_join(data,dplyr::select(Days,partNum,length,start),by="partNum") # joining the length and beginning time for all data 
data$progress <- as.numeric((data$Time_POS-data$start)/data$length)
data$progress[which(data$Part=="Day")] <- data$progress[which(data$Part=="Day")]+1
return (abs(data$progress*100)) # the absolute value is taken to make sure no negative values are taken stemming from the fact that
#the sunrise is defined slightly before the the sun angle become positive (insgnificant number of samples)
}


plotDailyActivity <- function(MoveData,activeSpeed=50/1200,TimevarName='dateTime',IdentifierName='TAG',simplePlot=T)
{
  if (!all(c(TimevarName,IdentifierName,'Latitude','Longitude','X','Y') %in% names(MoveData)) )
    simpleError(sprintf( 'MoveData doesnt contain the required variable:Latitude,Longitude,X,Y, %s, %s' , TimevarName,IdentifierName)  )
  colnames(MoveData)[(colnames(MoveData)==TimevarName)] <- 'dateTime'
  colnames(MoveData)[(colnames(MoveData)==IdentifierName)] <- 'TAG'
  MoveData <- MoveData %>% arrange(TAG,dateTime) %>% 
    group_by(TAG) %>% 
    mutate(dist=sqrt((X-lag(X))^2+(Y-lag(Y))^2),
           spd=dist/as.numeric(dateTime-lag(dateTime),units='secs'),
           H=hour(dateTime)) %>%
    ungroup()
  MoveData$dayprogress <- DayNight_Progress(MoveData$dateTime,MoveData$Latitude,MoveData$Longitude)
  
  activeRatio <- MoveData %>% arrange(TAG,dateTime) %>% 
    filter(dayprogress<201) %>% 
    mutate(dayProgCategory = (round(dayprogress/10)*10/200)) %>%
    group_by(dayProgCategory) %>% 
    mutate(totalPointsCount=n(),
           activePointsCount=length(which(spd>activeSpeed)),
           activeRatio=activePointsCount/totalPointsCount) %>% 
    slice(1)
  if(simplePlot)
  {
    plot((activeRatio$dayProgCategory),activeRatio$activeRatio,ylim=c(0,1))
    x <- seq(0,1,0.01)
    lines(x,(1+cos(pi/2+x*2*pi))/3,col='red')
    lines(x,x*0+1/3,col='red')
  }
  else{
    
    p <-  ggplot(MoveData %>% filter(dayprogress<201), aes(x=as.factor(round(dayprogress/10)*10/200),y=spd)) + 
      geom_violin(trim=F) + 
      stat_summary(fun=mean, geom="point", size=2, color="red")+
      ylim(0, 5*activeSpeed)
    p
  }
}

