library (dplyr)


stop_move_analysis <- function(data,stopparams="default",DayColName=c("DAY"),minSampperDay=500,rmMargins=F)
{
  # A wrapper function which defines stops/flights segments and filters the data based on these definations
  # The wrapper activates additional functions including AdpFixedPoint & MarkStopLoc (from toolsForAtlas) which
  # Defines stop points and clears the data based on the segmentation
  # Input data - 
  # data - a data.frame containing a trajectory with columns $TAG,$TIME,$dT,$DAY,$X,$Y
  # the "day" variable name can be defined according to the DayColName parameter
  # stopparams - a data.frame defining the stop identification parameters
  # minSampperDay - number of minimum samples (rows) per day, a day with less than the minimum will discard
  # rmMargins - an activator to the "RemoveMargins" function: F-not activate, T- will activate the function 
  # Output - a data.frame with additional columns presenting the stops-flying segments:
  # ADP : a set of variables of each stop:
  # "ID" a set of ID, for each stop starting at the beginning of the data.set
  #  "X","Y", the median for each stop
  #  "start", "end", "duration" times for each stop ( in "TIME" units, usually miliseconds)
  #  "qlt"  some unknown quality parameter,
  # seg.ID : a set of ID, for each movement segment starting at the beginning of the data.set
  # indices to match tag frequency:
  if (stopparams=="default")
  ind_rts<-data.frame("smp_rte"=c(8000,4000), # sample rate
                      "obs_min"=c(4,8 ), # minimum of within range localizations
                      "p_lim"=c(5,10), #Tolerance of n localizations outside the buffer
                      "adp_rng"=rep(35,2)) # adaptive Fixed point buffer radius

  else
    ind_rts <- stopparams
  
  #changing the "DAY" column name
  names(data)[which(colnames(data)==DayColName)] <- "DAY"
  
  #removing days with less then 100 rows 
  tt <- table(data$DAY)
  data1 <- subset(data, DAY %in% names(tt[tt > minSampperDay]))
  
  #Performing track segmentation - adding properties to each of the points
  stop_data<-defineStopPoints(data1,ind_rts)
  
  #clear the data using RemoveMargins: Abandons previous data for the first exit from the previous 
  #day's accommodation point, and after entering the next day's accommodation point
  if(rmMargins)
  {stop_data<-RemoveMargins(stop_data)}
  stop_data<-as.data.frame(stop_data)
  
  stop_data<-addLocAttribute(stop_data, locAttributs=c("distanceSpeed", "locQuality")) # function to add attributes for each pair of conseucutive points. 
  names(stop_data)[which(colnames(stop_data)=="DAY")] <- DayColName
  return(stop_data)
}

defineStopPoints<-function(data,
                           ind_rts,
                           time_gap=3600*1000){
  #Modified from a script of Ingo Schiffner, Emmanuel Lourie & Sivan Margalit
  # A wrapper for AdpFixedPoint & MarkStopLoc functions from toolsForAtlas
  # Main tasks:
  # Loop over several tags with several days each, applying the following functions:
  # AdpFixedPoint - returns a list of track segmentation with stops properties including start/end time, duration, and median location (X/Y)
  # Removing dummy points created by AdpFixedPoint (stops with only a single point)
  # MarkStopLoc - Marking localizations as stop or move segment returning a final data.frame where each point is characterized as either stop (ADP) or movement (seg)
  # Input variables:
  # data - data.frame containing a trajectory with columns $TAG,$TIME,$dT,$DAY,$X,$Y
  # ind_rts - a data.frame with ADP parameters: $smp_rte,$obs_min,$p_lim,$adp_rng 
  # output - a data.frame with additional columns presenting the stops-flying segments, and their properties
  
  owl_adp<-NULL
  data<-data%>%
    arrange(TAG, TIME)
  
  TagList <- unique(data$TAG)
  
  for (tg in TagList){
    # what is this tag;s sampling frequency?
    freq<-round(min(data$dT[which(data$TAG==tg)],na.rm=TRUE),0)*1000
    # Get AdpFixedPoint criterias from the table according to sampling rate:
    smp_rte <- ind_rts$smp_rte[which(ind_rts$smp_rte==freq)] # sample rate
    adp_rng <- ind_rts$adp_rng[which(ind_rts$smp_rte==freq)] # adaptive Fixed point buffer radius
    obs_min <- ind_rts$obs_min[which(ind_rts$smp_rte==freq)] # minimum of within range localizations
    p_lim <- ind_rts$p_lim[which(ind_rts$smp_rte==freq)] #Tolerance of n localizations outside the buffer 

    
    # created a list of DAYs tracked per tag for looping:
    nDAYs<-unique(data$DAY[which(data$TAG==tg)])
    
    # Start loop per tag-DAY. Notice that data MUST be sorted chrnologically first.
    for (nn in nDAYs){
      AFP_time <- Sys.time()# for the Prints the work duration
      DAY_dat<-as.data.frame(data%>%
                                 filter((TAG==tg) & (DAY==nn))%>%
                                 arrange(TIME))
      
      
      if(nrow(DAY_dat)>20) # only run if DAY data (point for TAG per DAY) is over 20 locations:
      {
        #function to perform track segmentation (from toolsForAtlas)
        AFPList <- AdpFixedPoint (time.vec = DAY_dat$TIME,
                                  x=DAY_dat$X,
                                  y=DAY_dat$Y,                             
                                  adp_rng=adp_rng,
                                  smp_rte=smp_rte,
                                  obs_min=obs_min,
                                  p_lim=p_lim,
                                  time_gap=time_gap) 
        
        #remove dummy points created by AdpFixedPoint
        kdx <- which(AFPList$duration!="1")
        AFPList <- AFPList[kdx,]
        
        
        if (nrow(AFPList)>1){
          AFPList$DAY_number<-nn
          AFPList$TAG<-tg
          owl_adp<-rbind(owl_adp, AFPList)
        }
      }
    }   
  }
  print(sprintf("AdpFixedPoint is done by time= %s",Sys.time()- AFP_time))# Prints the work duration  
  # Mark localizations as perch or move segment (from toolsForAtlas)
  MarkStop_time <- Sys.time()# for the prints the work duration
  filtered_with_ADP<- MarkStopLoc(data, owl_adp)
  print(sprintf("MarkStopLoc is done by time= %s",Sys.time()- MarkStop_time))# Prints the work duration
  return(filtered_with_ADP)
}

RemoveMargins<-function(stop_DAYs){
  # The function crops the data based on the segmentation done by the previous functions
  # keeps a single location from the first stop (discard the rest of the roosting locations) and only a single point form the last stop (discard the rest of the roosting locations)
  # Input data - data.frame containing the columns $TAG,$TIME,$DAY,$ADP.ID
  # output - a trimmed data.frame 
  RemoveMargin_time <- Sys.time()# for the prints the work duration
  #ifelse(is.na(min(N12$ADP.ID)),0,min(N12$ADP.ID, na.rm = T))==ifelse(is.na(max(N12$ADP.ID)),0,max(N12$ADP.ID, na.rm = T)) #the line doesn't work, because if there is NA in the Column (even a single one) the result is 'true'
  clear_DAYs <- stop_DAYs %>%  
    group_by(TAG,DAY)  %>%
    filter(TIME>=TIME[max(which(ADP.ID == min(ADP.ID,na.rm=T)))] & TIME<=TIME[which.max(ADP.ID)]) %>%  
    ungroup()
  clear_DAYs <- clear_DAYs[order(clear_DAYs$TAG,clear_DAYs$TIME),]
  clear_DAYs <- as.data.frame(clear_DAYs)
  print(sprintf("RemoveMargins is done by time= %s",Sys.time()- RemoveMargin_time))# Prints the work duration
 return(clear_DAYs)
} 


PlotSegStop <- function(stopdat,data2,DAY,DayColName=c("DAY"))
{
  # plots the data before and after Stop analysis
  # input parameters:
    # stopdat - a "stop_move_analysis" analyzed data.frame ( contaning the variables "LAT", "LON", "ADP.ID", "ADP.X", "ADP.Y", "ADP.qlt","DAY")
    # data2 - an original data that corresponds to stopdat, ( containing the variables "LAT", "LON", "dateTime","TIME")
    # DAY - a single day to plot, must be contained in the values of days in the original data
    # the name of the day column name
  

  names(stopdat)[which(colnames(stopdat)==DayColName)] <- "DAY"
  names(data2)[which(colnames(data2)==DayColName)] <- "DAY"
  stopdat <- stopdat[which(stopdat$DAY==DAY),]
  data2 <- data2[which(data2$DAY==DAY),]
  
  
  stopdat<-stopdat%>%
    mutate("segOrADP"=ifelse(is.na(ADP.ID),"SEG","ADP"))
  AtmpStop<-stopdat%>%
    group_by(ADP.ID, ADP.X, ADP.Y, ADP.duration, ADP.qlt,DAY)%>%
    dplyr::summarise("cnt"=n(),
              "start_time"=min(dateTime),"end_time"=max(dateTime))%>%
    ungroup()
  # AtmpStop <- AtmpStop[-which(is.na(AtmpStop$ADP.X)),] #this is the original line, but I changed it due to ERRORS
  AtmpStop <- AtmpStop[which(!is.na(AtmpStop$ADP.X)),]
  AtmpStop<-as.data.frame(convertSpatial.ITM2WGS84(AtmpStop, xyColNames =c("ADP.X", "ADP.Y")))
  
  ll<-leaflet() %>% #addProviderTiles('Esri.WorldImagery') %>%
    addTiles() %>% 
    addCircles(data=stopdat, lng = ~LON, lat = ~LAT, weight = 5, fillOpacity = 1,color = 'red', group="track",
               popup=~sprintf("time= %s",dateTime)) %>% 
    addCircles(data=stopdat %>%subset(segOrADP=="SEG") , lng = ~LON, lat = ~LAT, weight = 5, fillOpacity = 1,color = 'blue', group="seg",
               popup=~sprintf("SEG.ID= %i,time= %s,TIME=%0.0f",seg.ID,dateTime,TIME)) %>% 
    addCircles(data=AtmpStop, lng = ~LON, lat = ~LAT, weight = 5, fillOpacity = 1,radius = 5,color = 'green', group="ADP",
               popup=~sprintf("ADP.ID= %i,time= %s, count=%i",ADP.ID,start_time,cnt)) %>%
    addCircles(data=stopdat %>%subset(segOrADP=="ADP"), lng = ~LON, lat = ~LAT, weight = 5, fillOpacity = 1,radius = 5,color = 'yellow', group="stop",
               popup=~sprintf("ADP.ID= %i,time= %s,TIME=%0.0f",ADP.ID,dateTime,TIME)) %>% 
    addPolylines(data=stopdat,
                 lng = ~LON,
                 lat = ~LAT,
                 weight = 1,
                 fillOpacity = 0.7,
                 color = 'red') %>% 
    addCircles(data=data2, lng = ~LON, lat = ~LAT, weight = 5, fillOpacity = 1,radius = 5,color = 'black', group="original",
               popup=~sprintf("time= %s,TIME=%0.0f",dateTime,TIME)) %>% 
    addPolylines(data=data2,
                 lng = ~LON,
                 lat = ~LAT,
                 weight = 1,
                 fillOpacity = 0.7,
                 color = 'black', group="original_line") %>%
    addLayersControl(
      overlayGroups = c("track","seg","ADP","stop","original","original_line"),
      options = layersControlOptions(collapsed = FALSE, autoZIndex=TRUE)) 
  ll <- ll%>% 
    addScaleBar(position = c("bottomleft"), 
                options = scaleBarOptions(imperial=FALSE,maxWidth=200))
  ll
}

#' Adaptive fixed point
#'
#' AdpFixedPoint is a function to perform track segmentation utilizing a
#' first-passage algorithm to determine fixed points where the agent/animal
#' has spent a minimum number of observations (obs_min) within a a certain
#' range (adp_rng) of a fixed point that is continuously reavaluated.
#'
#' @param time.vec absolute time e.g. in ms (ATLAS timestamp)
#' @param x projected longitude in meters
#' @param y projected latitude in meters
#' @param adp_rng adaptive range defining fixed points
#' @param smp_rte sampling rate e.g. in ms (ATLAS)
#' @param obs_min minimum nummber of observations defining a fixed position
#' @param p_lim point limit for leaving current fixed point
#' @param time_gap minimal time gap of missing values refere as exit of adp area (default 5 minutes)
#'
#'
#' @return The function returns an array containing information about each fixed
#'         point including (in order) start time, end time, duration, number of locations,
#'         position quality, median x, median-y, lower x quantile, upper x quantile, lower y quantile, upper y quantile
#
#'
#'
#' Original Code by Ingo Schiffner 2017 (Matlab version)
#' Converted to R by Emanuel Lourie 2018
#'
#' @export

AdpFixedPoint <- function(time.vec,x,y,
                          adp_rng,
                          smp_rte,
                          obs_min,
                          p_lim,
                          time_gap=5*60*1000)
{
  n_x <- which(!is.na(x))
  max_pos <- trunc(length(n_x)/(obs_min+p_lim))
  time.vec <- as.double(time.vec[n_x]) # fix potential problems with integer overlow
  
  AFPList<-data.frame(start=rep(NA,max_pos),
                      end=rep(NA,max_pos),
                      duration=rep(NA,max_pos),
                      num_loc=rep(NA,max_pos),
                      position_qlt=rep(NA,max_pos),
                      medX=rep(NA,max_pos),
                      medY=rep(NA,max_pos),
                      dummy=rep(NA,max_pos))
  
  si <- n_x[1]                    # start index
  ei <- n_x[length(n_x)]          # end index
  cfp <- si                       # set initial fixed point
  
  cfp_i <- 1                      # counter to check if we have a valid fixed point (i.e. exceeds obs_min)
  l_cnt <- 0                      # counter to check if agent is leaving current fixed point (i.e. exceeds p_lim)
  fp_cnt <- 0                     # fixed point counter
  
  #set startpoint as first point (we do this for those cases were the animal is imidiatley moving)
  # AFPList$start[fp_cnt] <- time.vec[si] # start time
  # AFPList$end[fp_cnt] <- time.vec[si+1] # end time
  # AFPList$duration[fp_cnt] <- 1         # duration (ms)
  # AFPList$num_loc[fp_cnt] <- 2          # number of localizations defining fixed point
  # AFPList$position_qlt[fp_cnt] <- 0     # estimate of position quality
  # AFPList$medX[fp_cnt] <- x[si]         # median x position
  # AFPList$medY[fp_cnt] <- y[si]         # median-y position
  
  cfp_x <- NULL # current fixed point x
  cfp_y <- NULL # current fixed point y
  cfp_t <- NULL # current fixed point time
  
  #move through the data
  i <- 1
  while (i<length(n_x))
  {
    # for (i in n_x[-1])
    i <- i+1
    #make sure its a valid position
    if (is.na(x[i]) | is.na(y[i])){
      print(sprintf("AdpFixedPoint: invalid value in x or y at line %d",i))
      next
    }
    
    #get distance from current fixed point
    dx <- x[i]-x[cfp]
    dy <- y[i]-y[cfp]
    e_dst <- (dx^2 + dy^2)^0.5
    dt<-time.vec[i]-time.vec[i-1]
    
    if ((e_dst > adp_rng) | (dt>time_gap))# check if agent is out of range
    {
      # increase leaving counter
      l_cnt <- l_cnt + 1
      if ((l_cnt >= p_lim) | (dt>time_gap)) # check if agent has left current fixed point
      {
        if (cfp_i >= obs_min) # check if fixed point has sufficient observations
        {
          #evaluate fixed point
          fp_cnt <- fp_cnt + 1
          AFPList$start[fp_cnt] <-   time.vec[cfp]      #cfp_t[1]                         # start time
          AFPList$end[fp_cnt] <- cfp_t[cfp_i-1]                                # end time
          AFPList$duration[fp_cnt] <- (cfp_t[cfp_i-1]- time.vec[cfp]) #cfp_t[1])                     # duration
          AFPList$num_loc[fp_cnt] <- cfp_i;                                      # number of localizations
          AFPList$position_qlt[fp_cnt] <- round((AFPList$num_loc[fp_cnt]/(1+AFPList$duration[fp_cnt]/smp_rte)),3) # position quality
          AFPList$medX[fp_cnt] <- median(c(x[cfp],cfp_x))                                 # median x position
          AFPList$medY[fp_cnt] <- median(c(y[cfp],cfp_y))                                # median y position
          cfp <- i-l_cnt
        }
        
        # set new fixed point
        cfp <- cfp+1
        i <- cfp
        cfp_i <- 1
        #reset leaving counter
        l_cnt <- 0
        
        # reset temp data
        cfp_x <- NULL
        cfp_y <- NULL
        cfp_t <- NULL
      }
    }
    else
    {
      
      #add data to tmp fixed point list
      cfp_x[cfp_i] <- x[i]
      cfp_y[cfp_i] <- y[i]
      cfp_t[cfp_i] <- time.vec[i]
      cfp_i <- cfp_i +1
      
      #reset leaving counter
      l_cnt <- 0
    }
  }
  #in case that the last point is part of a stop point, add it to AFPList as the final stop
  if (cfp_i >= obs_min) # check if fixed point has sufficient observations
  {
    #evaluate fixed point
    fp_cnt <- fp_cnt + 1
    AFPList$start[fp_cnt] <-   time.vec[cfp]      #cfp_t[1]                         # start time
    AFPList$end[fp_cnt] <- cfp_t[cfp_i-1]                                # end time
    AFPList$duration[fp_cnt] <- (cfp_t[cfp_i-1]- time.vec[cfp]) #cfp_t[1])                     # duration
    AFPList$num_loc[fp_cnt] <- cfp_i;                                      # number of localizations
    AFPList$position_qlt[fp_cnt] <- round((AFPList$num_loc[fp_cnt]/(1+AFPList$duration[fp_cnt]/smp_rte)),3) # position quality
    AFPList$medX[fp_cnt] <- median(c(x[cfp],cfp_x))                                 # median x position
    AFPList$medY[fp_cnt] <- median(c(y[cfp],cfp_y))                                # median y position
    cfp <- i-l_cnt
  } else if ((fp_cnt+1)<=max_pos){  #set end point to last position (in case track stops mid flight)  
    fp_cnt <- fp_cnt + 1
    AFPList$start[fp_cnt] <- time.vec[ei-1]  # start time
    AFPList$end[fp_cnt] <- time.vec[ei]    # end time
    AFPList$duration[fp_cnt] <- 1           # duration
    AFPList$num_loc[fp_cnt] <- 2           # number of localization
    AFPList$position_qlt[fp_cnt] <- 0           # position quality
    AFPList$medX[fp_cnt] <- x[ei]       # median x position
    AFPList$medY[fp_cnt] <- y[ei]       # median y position
  }
  #trunctate output matrix
  AFPList<-AFPList[1:fp_cnt,]
  return(AFPList)
}


#' Mark localizations as perch or move segment
#'
#' MarkStopLoc can be used to combine the perching data (from AdpFixedPoint function)
#' into the localization data. each localization within a perching period mark as
#' assigned to ADP point. and all the othr localizations are movements
#'
#' @param locs.df localizatins data frame (after filtering) with m rows
#' @param ADP.df ADP dataframe - retreived by AdpFixedPoint with n rows
#'
#' @return The function returns a data frame with m rows (as in locs.df),
#'        but numbrt of columns enhaced with information from ADP.df
#'
#' @import dplyr
#' @export
MarkStopLoc<-function(locs.df, ADP.df){
  TagList<-unique(ADP.df$TAG)
  
  # verify loc.df does not have yet ADP/ Seg.ID fields
  if (!('ADP.ID' %in% colnames(locs.df))){
    locs.df$ADP.ID<-NA
    locs.df$ADP.X<-NA
    locs.df$ADP.Y<-NA
    locs.df$ADP.start<-NA
    locs.df$ADP.end<-NA
    locs.df$ADP.duration<-NA
    locs.df$ADP.qlt<-NA
    
    locs.df$seg.ID<-NA
  }
  
  ADP.df<-ADP.df%>%arrange(TAG, start)
  
  for (aTag in TagList){
    
    segID<-1
    ADP.of.tag.ix<-which(ADP.df$TAG==aTag)
    print(sprintf("MarkStopLoc(): TAG %s has %d ADP",
                  as.character(aTag), length(ADP.of.tag.ix)))
    ADP.ix.1<-min(ADP.of.tag.ix)
    # mark localizations in movement segment befor first AOP
    seg.ix<-which((locs.df$TAG==aTag) & locs.df$TIME<ADP.df$start[ADP.ix.1])
    if (length(seg.ix)>5){
      locs.df$seg.ID[seg.ix]<-segID
    }
    
    for (i in ADP.of.tag.ix){
      
      startTime<-ADP.df$start[i]
      endTime<-ADP.df$end[i]
      
      loc.in.ADP<-which((locs.df$TAG==aTag) & (between(locs.df$TIME, startTime, endTime)))
      #if (length(loc.in.ADP)!=ADP.df$num_loc[i]){
      #print(sprintf("markStopLoc() ADP %d : nloc=%d =?= %d ",
      #      i, length(loc.in.ADP), ADP.df$num_loc[i]))
      #}
      locs.df$ADP.ID[loc.in.ADP]<-segID
      locs.df$ADP.X[loc.in.ADP]<-ADP.df$medX[i]
      locs.df$ADP.Y[loc.in.ADP]<-ADP.df$medY[i]
      locs.df$ADP.start[loc.in.ADP]<-ADP.df$start[i]
      locs.df$ADP.end[loc.in.ADP]<-ADP.df$end[i]
      locs.df$ADP.duration[loc.in.ADP]<-ADP.df$duration[i]
      locs.df$ADP.qlt[loc.in.ADP]<-ADP.df$position_qlt[i]
      
      # mark next movement segments after ADP
      segID<-segID+1
      if (i<nrow(ADP.df)){
        seg.ix<-which(between(locs.df$TIME,ADP.df$end[i]+1,ADP.df$start[i+1]-1))
      }
      else{
        seg.ix<-which(locs.df$TIME>ADP.df$end[i])
      }
      
      if (length(seg.ix)>0){
        locs.df$seg.ID[seg.ix]<-segID
      }
    }
  }
  return (locs.df)
}
