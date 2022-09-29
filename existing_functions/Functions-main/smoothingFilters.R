# Smoothing filters include two smoothing tools (outlayerSmooth, movMean)
# and a functions that applies this two tools on each burst in a dataframe (AvgSmooth)

outlayerSmooth <- function(dataBurst,DistDiffThreshold=0,rmGeoCoords=T)
{  
  # This function smooths a track by identifying outlayers: (for any point, it calculates the distance to its two subsequent points, e.g. for points indexed 1,2, and 3, it calculates the d_12,
  # and d_13. If d_13 is smaller than d_12, the point 2 is set as an average of points 1 and 3)
  # it is advised to operate the function on separated time-bursts of the data (as wrapped in the function "AvgSmooth"
  # The function requires the following input variable:
  # dataBurst: a data.frame containing the variables: "X","Y" (coordinates in UTM/ITM format).
  # The output is the same data.frame with smoothed coordinate 
  # by deafalut the LAT and LON coordinates will be deleted has they are no longer updated
  track <- dataBurst
for (locIdx in (nrow(track)-2)) {
  dist1_2 = sqrt((track$X[locIdx] - track$X[locIdx+1])^2 + (track$Y[locIdx] - track$Y[locIdx+1])^2)
  dist1_3 = sqrt((track$X[locIdx] - track$X[locIdx+2])^2 + (track$Y[locIdx] - track$Y[locIdx+2])^2)
  
  if (dist1_2-DistDiffThreshold > dist1_3) {
    track$X[locIdx+1] = mean(c(track$X[locIdx],track$X[locIdx+2]))
    track$Y[locIdx+1] = mean(c(track$Y[locIdx],track$Y[locIdx+2]))
  }
}
  if(rmGeoCoords)
  {track <- subset(track, select = -c(LAT,LON) )
   print("removing LON and LAT coordinates")
   warning(sprintf('note that LAT and LON variables might need to be updated' ))}
  warning(sprintf('note that some variables (including dT, dst,spd,angl) might need to be updated' ))
return(track)}

movMean <- function(dataBurst,Weight = c(0.25,0.5,0.25),replace=T,rmGeoCoords=T) 
{
  # This function smooths a track by setting any point as weighted average of its neighboring points, according to the Weight variable!
  # it is advised to operate the function on separated time-bursts of the data (as wrapped in the function "AvgSmooth"
  # The function requires the following input variables:
  # dataBurst: a data.frame containing the variables: "X","Y" (coordinates in UTM/ITM format).
  # Weight is the weights used for the weighted average , it can have any number of values (recommended to be odd number of values in a symmetric pattern)  
  # The output is the same data.frame with smoothed coordinates. In the case that replace=False is specified, only the "x", "Y" coloumns will be returned!
  Weight <- Weight/sum(Weight)
  X <- as.numeric(stats::filter(dataBurst$X, Weight,method = "convolution",sides = 2))
  Y <- as.numeric(stats::filter(dataBurst$Y, Weight,method = "convolution",sides = 2))
  X[which(is.na(X))] <- dataBurst$X[which(is.na(X))]
  Y[which(is.na(Y))] <- dataBurst$Y[which(is.na(Y))]
  warning(sprintf('note that some variables (including dT, dst,spd,angl) might need to be updated' ))
  
  if(replace){
    dataBurst$X <- X
    dataBurst$Y <- Y
    if(rmGeoCoords)
      {dataBurst <- subset(dataBurst, select = -c(LAT,LON) )
      print("removing LON and LAT coordinates")}
    return(dataBurst)}
  else
    warning(sprintf('note that LAT and LON variables might need to be updated' ))
    return(data.frame(X=X,Y=Y))
}

AvgSmooth <- function(dataBurst,Weight = c(0.25,0.5,0.25),UpdateGeoCoords=T,DistDiffThreshold=0)
{ 
  # This function operates the two smoothing functions () on separated groups defined by its timeGapBurst value!
  # The function requires the following input variables:
  # dataBurst: a data.frame containing the variables: "X","Y" (coordinates in UTM/ITM format), and "timeGapBurst" a value that separates the track into time-continuous bursts
  # Weight is the weights used for the weighted average in the "movMean" function (see this function for details)
  # The output is the same data.frame with smoothed coordinates. 
    dataBurst <- dataBurst %>% 
  group_by(timeGapBurst) %>%
    group_modify(~ outlayerSmooth(.,rmGeoCoords=F,DistDiffThreshold=DistDiffThreshold    )) %>% 
    group_modify(~ movMean(.,Weight,rmGeoCoords=F ))
    
    if(UpdateGeoCoords)
    {
      print("Udating LON and LAT coordinates")
      itm.spdf<-data.frame(LON=dataBurst$X,LAT=dataBurst$Y)
      #generate SpatialPointsDataFrame for ITM coordinates
      coordinates(itm.spdf)<-c("LON","LAT")
      
      #projection string for Israeli New Grid (ITM) format
      # from: http://spatialreference.org/ref/epsg/2039/proj4/
      itm<-"+init=epsg:2039" 
      # itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
      proj4string(itm.spdf) <- CRS(itm)
      
      
      # CRS = Coordinates Reference Sysytem
      WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      
      # spTransform() - function that convert from one CRS to another
      #generate SpatialPointsDataFrame for WGS84 coordinates
      WGS84.spdf <- spTransform(itm.spdf, WGS84)
      
    dataBurst$LON <-WGS84.spdf$LON
    dataBurst$LAT <-WGS84.spdf$LAT
    }
    warning(sprintf('note that some variables (including dT, dst,spd,angl) might need to be updated' ))
  return(dataBurst)
}