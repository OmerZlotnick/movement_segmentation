addDistanceSpeed<-function(input.loc.df){
  
  if (length(which(colnames(input.loc.df) %in% c("TIME","X","Y")))<3){
    Er <- simpleError(paste("addDistanceSpeed: missing one of the columns TIME,X,Y\n",
                            "  TIME expected to be 13 length integer for epoch time in millisecond",
                            "  X,Y coordinats in local mercator projection (metric units)"))
    stop(Er)
  }
  
  output.loc.df <- input.loc.df %>% group_by(TAG) %>% 
    arrange(TIME) %>% select(! c("distance","dT","spd"))
    mutate(distance=c(NA,(diff(X,1)^2 + diff(Y,1)^2) ^ 0.5),
           dT=c(NA,as.integer(diff(TIME,1))/1000),
           spd=de/dt)
  return(output.loc.df)
}