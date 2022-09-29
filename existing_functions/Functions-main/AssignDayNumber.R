# A function to assign day number
# Output:
    # returns the same data.frame with additional column "DAY"
    # "DAY" columns contain either Julian day or sequential day (counted from the first date of the TAG)
    # "DAY" is defined so it begins on DayStartTime (a HH:MM:SS character, in UTC, just like the ATLAS times)
# input:
  # The input variable "data" is a data.frame containing a POSIXct time columns (tz="UTC",origin="1970-01-01") 
  # The input time variable can be defined using the input variable "TimeColName"
  # The input TAG variable can be defined using the input variable "GroupIdentifier"
  # The day are counted from either as Julian days (Julian=TRUE) or from the first day in the array (default, Julian=FALSE) 


AssignDayNumber <- function(data,DayStartTime="00:00:00",TimeColName = "dateTime",GroupIdentifier = "TAG",Julian=FALSE)
{
  data <- as.data.frame(data)
  
  # setting the names of variables to agree with user choice
  dateTimeCol <- which(colnames(data)==TimeColName)
  IntialColname <- colnames(data)[dateTimeCol]
  colnames(data)[dateTimeCol] <-  "dateTime"
  GroupIdentifierCol <- which(colnames(data)==GroupIdentifier)
  IntialGroupname <- colnames(data)[GroupIdentifierCol]
  colnames(data)[GroupIdentifierCol] <-  "TAG"
  
  if(!is.POSIXct(data[,dateTimeCol]))
  {er <- errorCondition(sprintf("AssignDayNumber: the specified variable (%s) in the provided data.frame must be in POSIXct format",TimeColName))
    stop(er)
  }
  
  timeshift <- as.numeric(as.POSIXct(paste("1970-01-01",DayStartTime),tz="UTC",origin="1970-01-01"))
  if (Julian)
  {
    data<- data %>% mutate(DAY=as.numeric(yday(dateTime-timeshift)))
    
  }
  else
  {
    data<- data %>% group_by(TAG) %>% 
           mutate(DAY=as.numeric(date(dateTime-timeshift)-min(date(dateTime-timeshift))+1)) %>% 
           ungroup()
  }
  # converting back the variables names to the original ones!
  colnames(data)[dateTimeCol] <-  IntialColname
  colnames(data)[GroupIdentifierCol] <-  IntialGroupname
  return(data)
}


