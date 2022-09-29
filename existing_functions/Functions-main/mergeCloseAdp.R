library(dplyr)
library(data.table)
# a function presented in the Jerusalem ATLAS tutorial
# merge close ADP locations 
#input:
  # adp.df - a data.frame output of ADP
  # adp_rng - maximal range to merge ADP locations,in meters (default=20)
  # time_gap - a maximal time-gap to merge ADP locations in minutes (default=5)
  # smp_rte - the tag sampling frequency in seconds (default=8)
#output:
  # an ADP data.frame

mergeCloseAdp<-function(adp.df,adp_rng=20,smp_rte=8,time_gap=5)

  {
  smp_rte <- smp_rte*1000
  time_gap <- time_gap*60*1000
  adp.df<-adp.df%>%arrange(TAG, start)
  tags.lst<-unique(adp.df$TAG)
  results.adp.lst<-list()
  for (tg.ix in 1:length(tags.lst)){
    tg<-tags.lst[tg.ix]
    tag.adp.df<-subset(adp.df, TAG==tg)
    nn<-nrow(tag.adp.df)
    tag.adp.df$dT<-NA
    tag.adp.df$dT[1:(nn-1)]<-(tag.adp.df$start[2:nn]-tag.adp.df$end[1:(nn-1)])
    tag.adp.df$distance<-NA
    tag.adp.df$distance[1:(nn-1)]<-with(tag.adp.df,
                                      expr={
                                        sqrt((medX[2:nn]-medX[1:(nn-1)])^2+(medY[2:nn]-medY[1:(nn-1)])^2)
                                      })
    
    # gaps big nough between stopping locations
    gap.ix<-which((tag.adp.df$dT[1:(nn-1)]>time_gap) | (tag.adp.df$distance[1:(nn-1)]>adp_rng))
    if ((nn-1) %in% gap.ix){
      gap.ix<-c(gap.ix, nn)
    }
    gap.adp.df<-tag.adp.df[gap.ix,]
  
    # trying to merge close consequent stopping locations
    prev_i<--1
    if (nrow(gap.adp.df)<nn){
      close.adps<-which((tag.adp.df$dT<=time_gap) & (tag.adp.df$distance<=adp_rng))
      merged.adp<-tag.adp.df[0, ]
      isSeq=FALSE
      j<-1
      for (i in close.adps) {
        if (isSeq==TRUE){
          adp.row<-merged.adp[j,]
        }else{
          adp.row<-tag.adp.df[i,]
        }
      
        adp.row$end <- tag.adp.df$end[i+1]
        adp.row$duration <- adp.row$end-adp.row$start
        adp.row$num_loc <- adp.row$num_loc+tag.adp.df$num_loc[i+1]
        adp.row$position_qlt <- round((adp.row$num_loc/(1+adp.row$duration/smp_rte)),3) # position quality
        adp.row$medX=mean(adp.row$medX, tag.adp.df$medX[i+1])
        adp.row$medY=mean(adp.row$medY, tag.adp.df$medY[i+1])
        
        if (i+2<=nn){
          dT<-tag.adp.df$start[i+2]- adp.row$end
          distance<- sqrt((tag.adp.df$medX[i+2]-adp.row$medX)^2+(tag.adp.df$medY[i+2]-adp.row$medY)^2)
        }else{
          dT<-NA
          distance<-NA
        }
        merged.adp[j,] <- adp.row
        
       
        
        if (!is.na(dT)){
          if ((dT>time_gap) | (distance>adp_rng)){
            j<-j+1  
            isSeq=FALSE
          }
          else{
            isSeq=TRUE
          }
        }
        
        prev_i=i
        
      }
      ret.adp.df<-rbind(gap.adp.df, merged.adp)
      ret.adp.df<-ret.adp.df%>%arrange(start)
      results.adp.lst[[tg.ix]]<-ret.adp.df
    }
      
  }
  results.adp.all<-rbindlist(results.adp.lst)
  return(results.adp.all)
}


