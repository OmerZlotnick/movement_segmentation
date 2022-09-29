## amt Paper: https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.4823
## amt: https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html

# This function divide the track into bursts based of continuous detection (only time-wise )
# it is based on time gaps between two point and requires the following variables:
    # data: a data.frame containing the variables: "X","Y" (coordinates in any format),"dateTime" (POSIXct fromat)
    # secTol # maximum time-length (seconds) for gap within a burst (above this value will define a new burst )
    # minBurstFix # Min points in burst (discards all locations in burst shorter than this value)
    # sampRate : the nominal sampling rate of the tag (number of seconds)
    # keepadditionalvars: should the return function include all source columns ( default is True)
#The output is a data.frame with all initial variables and the addition of:
    #"timeGapBurst" (a unique burst integer ID ), "pointInBurst" (number of points in the specific burst)
# Packages required: amt, lubridate

timeGapBurst <- function(data,secTol,minBurstFix,sampRate=8,keepadditionalvars=T){
  
  require(amt)
  data$TIME <- as.numeric(data$TIME)
  dataBurst_1 <- data %>%
    make_track(.x=X, .y=Y, .t=dateTime) %>%
    track_resample(rate = seconds(sampRate), tolerance = seconds(secTol)) %>%     # steps_by_burst() %>%   group_by(burst_) %>%    ungroup() %>%
    select(x_,y_,t_,burst_) %>% #select(x1_,y1_,t1_,burst_) %>%
    rename(X = x_,Y = y_,dateTime = t_, timeGapBurst = burst_) %>% #rename(X = x1_,Y = y1_,dateTime = t1_, timeGapBurst = burst_) %>%
    group_by(timeGapBurst) %>% # remove burst with pointInBurst lower than "minBurstFix"
    add_count()%>%
    filter(n > minBurstFix) %>%
    rename(pointInBurst=n) %>% 
    mutate(TIME = as.numeric(as.POSIXct(dateTime,"%Y-%m-%d %H:%M:%S", tz="UTC"))*1000 ) # Atlas time 
    colOrder <- c("X", "Y", "TIME","dateTime","timeGapBurst", "pointInBurst") #Change the order of columns
    dataBurst_1 <- dataBurst_1[, colOrder]
    if(keepadditionalvars)
      dataBurst_1 <- left_join(dataBurst_1,data[,-which(colnames(data) %in% c("TIME","X","Y")),],by="dateTime")
   return(ungroup(dataBurst_1))
}
