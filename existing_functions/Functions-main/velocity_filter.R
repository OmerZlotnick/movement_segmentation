require(data.table)
# velocity filter removes a location according to velocity
# details:
      # it removes a location whenever both the velocity from the previous point to it (v_in) and the velocity from it to the next point (v_out) are greater than  spdThreshold
      # it repeats the filtering with different step size:
            # step=0 means that the velocity is calculated between nearest neighbors (in time)
            # step=1 means that the velocity is calculated between second nearest neighbors
            # the input variable "steps" determines up to which neighbor to check velocity (default=1)
            # thus it can filter locations in case that a set of points up to size "steps" was drifted away
# input variable "data" is a data.frame with locations saved with column names x,y, and time
# returns the data.frame without the filtered locations
velocity_filter <- function (data,spdThreshold=15, x = "X", y = "Y", time = "TIME", steps=1, printfilt=T) 
{
for(i in 1:steps){
  spd <- matl_get_speed(data,x=x,y=y,time=time,type = "in",step = i)*1000
  KEEP <- (spd<spdThreshold)|(data.table::shift(spd,-i)<spdThreshold)
  KEEP[is.na(KEEP)] <- TRUE  
  data<-data[which(KEEP),]
  if(printfilt)
  print(sprintf("step %i removed %i locations",i,sum(!KEEP)))
}
return(data)  
}

# velocity filter removes a location  according to distance
# details:
      # it removes a location whenever both the distance from the previous point to it and the distance from it to the next point are greater than distThreshold
      # it repeats the filtering with different step size:
          # step=0 means that the distance is calculated between nearest neighbors (in time)
          # step=1 means that the distance is calculated between second nearest neighbors
          # the input variable "steps" determines up to which neighbor to check distance (default=1)
          # thus it can filter locations in case that a set of points up to size "steps" was drifted away
# input variable "data" is a data.frame with locations saved with column names x,y, and time
# returns the data.frame without the filtered locations
distance_filter <- function (data,distThreshold=15*8, x = "X", y = "Y", steps=1,printfilt=T) 
{
  for(i in 1:steps){
    dst <- matl_simple_dist(data,x=x,y=y,step = i)
    KEEP <- c((dst<distThreshold)|(data.table::shift(dst,i)<distThreshold),rep(T,i))
    KEEP[is.na(KEEP)] <- TRUE  
    data<-data[which(KEEP),]
    if (printfilt)
    print(sprintf("step %i removed %i locations",i,sum(!KEEP)))
  }
  return(data)  
}

# calculates a distance between subsequent points (or the next "step" point)
# used within the filters
matl_simple_dist <- function (data, x = "x", y = "y",step=1) 
{
  assertthat::assert_that(is.data.frame(data), is.character(x), 
                          is.character(y), msg = "simpleDist: some data assumptions are not met")
  if (nrow(data) > 1) {
    x1 <- data[[x]][seq_len(nrow(data) - step)]
    x2 <- data[[x]][(1+step):nrow(data)]
    y1 <- data[[y]][seq_len(nrow(data) - step)]
    y2 <- data[[y]][(1+step):nrow(data)]
    dist <- c(sqrt((x1 - x2)^2 + (y1 - y2)^2))
  }
  else {
    dist <- NA_real_
  }
  return(dist)
}
# calculates a speed between subsequent points (or the next "step" point)
# used within the filters
matl_get_speed <- function (data, x = "x", y = "y", time = "time", type = "in", step=1) 
{
  # atlastools::atl_check_data(data, names_expected = c(x, y, time))
  data.table::setorderv(data, time)
  distance <- matl_simple_dist(data, x, y,step)
  # distance <- distance[(step+1):length(distance)]
  dtime <- data[[time]][(step+1):nrow(data)]-data[[time]][1:(nrow(data)-step)]
  # time <- c(NA, diff(data[[time]]))
  speed <- distance/dtime
  if (type == "in") {
    speed <- c(rep(NA,step),speed)
  }
  else if (type == "out") {
    speed <-c(speed,rep(NA,step))
  }
  return(speed)
}

atl_unifiedFilter <- function(data,stdLimit=80,spdLimit=20,spdSteps=3,distLimit=300,distSteps=3,ellipsMovangle=15,ellipsMovRadius=15,ellipsMovDist=30)
{
  # the unified filter wraps a set of basic filters:
    # stdVarXY filter that filter-out any point with value greater than stdLimit
    # basic filter that discards any location which is identical to the previous
    # ellipse-Move filter that  discard points the move in correlation with their uncertainty ellipse with parameters: 
    #          ellipsMovangle ( max angle between ellipse and movement),
    #          ellipsMovRadius (max distance from previous location)
    #          ellipsMovDist   (max value for large axis of the uncertainty ellipse))
    # velocity filter (maximum speed of spdLimit over spdSteps steps )
    # distance filter (maximum distance of distLimit over distSteps steps )
  
  if (length(which(colnames(data) %in% c("TIME","X","Y","VARX","VARY","COVXY")))<6){
    Er <- simpleError(paste("atl_unifiedFilter: missing one of the columns TIME,X,Y,VARX,VARY,COVXY\n",
                            "  TIME expected to be 13 length integer for epoch time in millisecond",
                            "  X,Y coordinats in local mercator projection (metric units)"))
    stop(Er)
  }
  if (length(unique(data$TAG))>1) #the function is written to work on a single TAG (this line verify this requirement)
  {
    Er <- simpleError(paste("The atl_unifiedFilter function deals with single tags data"))
    stop(Er)
  }
  ellipsMovangle <- abs(cos(ellipsMovangle*pi/180)) # convert the angle to its cosine value
  data <- addLocAttribute(data, locAttributs=c("distanceSpeed","locQuality")) # function to add attributes for each pair of conseucutive points. 
  data <- data %>% filter(stdVarXY<stdLimit) %>%  # calculate ellipse parameters from VAR and COV values ( require the ellipsDir function from this file)
                  mutate(val1=sqrt(abs((VARX+VARY)-sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2),
                         val2=sqrt(((VARX+VARY)+sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2),
                         ellipsDir=ellipsDir(VARX,VARY,COVXY),
                         dX=abs(X-lag(X))) %>%
    filter(dX>0) %>%  # discards any location which is identical to the previous
    group_by(TAG) %>% 
    mutate(dX=X-medlag5(X), # lag(X), #caclculates movement direction!
           dY=Y-medlag5(Y), # lag(Y), #
           moveAngle= atan2(dX,dY)*180/pi,
           moveAngle= ifelse(moveAngle>0,moveAngle,moveAngle+360),
           projMovStdaxis=abs(cos((moveAngle-ellipsDir)*pi/180)),
           Dist = sqrt(dX^2+dY^2)
           #filter options: val2*projMovStdaxis>12, distance*projMovStdaxis>50 or different combinations of them
    ) %>% 
    # filter(projMovStdaxis<0.95|val2<15) %>%
    filter(projMovStdaxis<ellipsMovangle|val2<ellipsMovRadius|Dist<ellipsMovDist) %>%
    # dplyr::select(-c(dX,dY,Dist)) %>% 
    ungroup()
  data <- velocity_filter (data,spdLimit, x = "X", y = "Y", time = "TIME", steps=spdSteps) # velocity filter
  data <- distance_filter (data,distLimit, x = "X", y = "Y", steps=distSteps) # distance filter
  data <- addLocAttribute(data, locAttributs=c("speed")) # function to add attributes for each pair of conseucutive points. 
  
  return(data)
  
  
}

eDir <-function(VecVal)
{
  ellipDir <- ifelse(VecVal$values[1]>VecVal$values[2],
                     atan2( VecVal$vectors[1,1],VecVal$vectors[2,1])*180/pi,
                     atan2( VecVal$vectors[1,2],VecVal$vectors[2,2])*180/pi)
  ellipDir <- ifelse(ellipDir>0,ellipDir,ellipDir+180)
  return(ellipDir)
}

ellipsDir <- function(VARX,VARY,COVXY)
{
  x <- cbind(VARX, COVXY, COVXY,VARY)
  listOfMat <- lapply(seq_len(nrow(x)), function(i) x[i,])
  listOfMat <- lapply(listOfMat,matrix,ncol=2)
  listOfVecVal <- lapply(listOfMat,eigen)
  return(unlist(lapply(listOfVecVal,eDir)))
  
}

medlag5 <- function(x)
{
  xx <- cbind(lag(x,1),lag(x,2),lag(x,3),lag(x,4),lag(x,5))
  listOfLags <- lapply(seq_len(nrow(xx)), function(i) xx[i,])
  return(unlist(lapply(listOfLags,median,na.rm=T)))
}
