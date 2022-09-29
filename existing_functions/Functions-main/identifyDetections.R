# specify detections to each localization
# input variables:
  # LOC= a data.frame of defections (containing "TIME", "TAG",  and other observable)
  # DET= a data.frame of defections (containing "TIME", "TAG",  and "BS" observable, and possibly other)
  # the algorithm merge the and detection file according to the second  it was receipted
  # returns a a data.frame equal to LOC but with another observable: the antennas detected at that time)
  # note that it possible, due to an ATLAS bag that some of the detecting antennas didn't contribute to the localization
  # if unloclalized==TRUE these un localized detection will be specified in a second data.frame 
require(tidyverse)
identifyDetections <- function(Loc,Det,unloclalized=FALSE)
{

  Loc$roundTIME <- round(Loc$TIME/1000,0)*1000  # round time overide small detection time differences
  Det$roundTIME <- round(Det$TIME/1000,0)*1000 
  
  Det$BS <- as.character((Det$BS))              # it is easier to gruop character than integers
  Det$BS<-gsub("9720060000", '', Det$BS) 
  Det$BS<-gsub("972006000", '', Det$BS)
  Det$BS<-gsub("9720060", '', Det$BS)


  # a merge of all data where all localized detections also have coordinates:
  # Data <- left_join(Det,Loc,by=c("roundTIME","TAG"),suffixes = c(".DET",".LOC")) % dplyr version that does not work
  Data <- merge(x = Loc, y = Det, by = c("TAG","roundTIME"),all = TRUE, suffixes = c(".LOC",".DET"))
  print(sprintf("there are %i unlocalized detections",sum(is.na(Data$TIME.LOC)))) # detections that were not converted to loactions ( mainly due to only 2 or one antenna) 
  
  if (unloclalized)
  {
    # Detections that should have been added to a loclaization ut have not
    unLocDet <- Data[which(is.na(Data$TIME.LOC)),]
    unLocDet <- with(data = unLocDet,aggregate(BS,by = list(TIME=TIME.DET,roundTIME=roundTIME,TAG=TAG), unique))
    colnames(unLocDet)[which(colnames(unLocDet)=="x")] <- "AllBS"
    unLocDet<-unLocDet[order(unLocDet$TAG,unLocDet$TIME),] #make sure data is sorted chronologically (per tag)
  }

  Data <- Data %>%  # dyplyr syntax 
          filter(!is.na(X)) %>%
          group_by(TIME.LOC,TAG) %>%
          mutate(allBS = paste(BS, collapse = "")) %>%
          filter(row_number()==1) %>%
          ungroup()
  
  colnames(Data)[which(colnames(Data)=="TIME.LOC")] <- "TIME"
  redundant_columns<-c( "roundTIME","BS","TIME.LOC","TIME.DET","SNR","SAMPLES_CLK") # columns we won't use in this example.
  Data<-Data[,-which(colnames(Data) %in% redundant_columns)]
  Data$nAllBS <- nchar(Data$allBS)/2
  Data<-Data[order(Data$TAG,Data$TIME),] #make sure data is sorted chronologically (per tag)
  if (!unloclalized)
    return(as.data.frame(Data))
  else
    return(list(Data=as.data.frame(Data),unLocDet=unLocDet))
}
          