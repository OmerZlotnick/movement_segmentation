# the function accepts date in %d/%m/%Y format and time in %H:%M:%S format in, UTC and returns a numeric format (time from 1/1/1970)
# if ATLAS=TRUE the time is returned in milliseconds, else it is returned in seconds

humanTime2Unix <- function(DATE,TIME,ATLAS=FALSE)
{
  # convert times to UNIX time (ATLAS format)
  if (ATLAS) milisecond <- 1000
  else       milisecond <- 1
  DateTime<-paste(DATE,TIME)
  DateTime<-as.double(as.numeric(as.POSIXct(DateTime, "%d/%m/%Y %H:%M:%S", tz="UTC",origin="1970-01-01"))*milisecond) 
  return(DateTime)
}

# The function produces a data.table containing general Tag data
# the data is formed from a data.frame containing Localization ATLAS data 
# input variables:
      # a data.frame containing ATLAS localization with column names "TAG", the animal idetifier and "Time" (unix time)
# output
      # a data frame with the start and end times for every tag in unix format   
Tagtime <- function(AA)
  {
  TagList <- unique(AA$TAG)
  capture_unix <- numeric()
  off_unix <- numeric()
  for (tg in TagList)
    {
    capture_unix <- c(capture_unix,min(AA$TIME[which(AA$TAG==tg)]))
    off_unix <- c(max(AA$TIME[which(AA$TAG==tg)]))
    }
  times <- data.frame("TAG"=TagList,"capture_unix"=capture_unix,"off_unix"=off_unix)
  return(times)
  }