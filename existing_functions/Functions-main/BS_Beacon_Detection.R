# require(ggplot)
require(ggplot2)
require(lubridate)
require(dplyr)
require(RSQLite)
require(gridExtra)


Det_Data_from_ATLAS_server <- function(Query_Time_df,FullTag,BS=NA, SYS="Harod",Fields='TAG, TIME, BS, RSSI, GAIN, SNR, SAMPLES_CLK')
      {
   if (SYS=="Harod") #connects to Harod server
   {
      dbc <- dbConnect(RMySQL::MySQL(),
                     user = 'roatlasharod',            # username
                     password = 'roatlasHarodOrr5678#',# password
                     host = '132.66.79.21',            # host ip address
                     port=5900,                        # port Number
                     dbname='harod')                   # name of data base
   }
   else
   {stop("system not defined")}
   Ntag <- length(FullTag)
   AllTagsDet <- list() #make an empty list for detections
   Q <- data.frame(query=rep("c",nrow(Query_Time_df)*length(FullTag)))
   for (j in 1:nrow(Query_Time_df))
   {
      for (i in 1:length(FullTag)) 
      { # build a  DETECTIONS query for the system, the results include the variables listed below
         if (is.na(BS)) {
            Q$query[(j-1)*Ntag+i] <- paste('select',Fields,'from DETECTIONS WHERE TAG=',FullTag[i],
                                           'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000)
         #    query = paste('select TAG,TIME,BS,RSSI,GAIN,SNR from DETECTIONS WHERE TAG=',FullTag[i],
         #                               'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000)
         } else
         Q$query[(j-1)*Ntag+i] <-    paste('select',Fields,'from DETECTIONS WHERE TAG=',FullTag[i],
                                        'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000,'AND BS =', BS)
         # query = paste('select TAG,TIME,BS,RSSI,GAIN,SNR from DETECTIONS WHERE TAG=',FullTag[i],
         #               'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000, 'AND BS =', BS)
         # All_Data <- dbGetQuery(dbc,query)
         # AllTagsDet[[(j-1)*Ntag+i]] <- All_Data
      }
   }
   All_Data <- apply(X = Q,MARGIN = 1,FUN = dbGetQuery,conn = dbc)
   dbDisconnect(dbc)
   RawDet0 <- do.call(rbind.data.frame, All_Data)
   return(RawDet0)
}
Query_time_df <- function(Start_Time_Str,End_Time_Str,sample_timesPerDay=24,sample_lengthMinute=1)
      {
      num_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str,
                                              "%Y-%m-%d %H:%M:%S", tz="UTC"))
      num_End_Time<-as.numeric(as.POSIXct(End_Time_Str,
                                            "%Y-%m-%d %H:%M:%S", tz="UTC"))
      times <- seq(from = num_Start_Time,to = num_End_Time,by = 60*60*24/sample_timesPerDay)
      Query_time=data.frame(Start=times,End  =times+(sample_lengthMinute*60))
      Query_time$posStart <- as.POSIXct(Query_time$Start, tz="UTC", origin="1970-01-01")
      Query_time$posEnd <- as.POSIXct(Query_time$End, tz="UTC", origin="1970-01-01")
      return(Query_time) 
      }
Get_ATLAS_Det_DATA <- function(Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute,FullTag,BS=NA,slice=T,Fields='TAG,TIME,BS,RSSI,GAIN,SNR,SAMPLES_CLK')
      {
      STime <- Sys.time()
      Query_Time <- Query_time_df(Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute)
      RawDet0 <- Det_Data_from_ATLAS_server(Query_Time,FullTag,BS,Fields = Fields)
      RawDet0$BS<-gsub("9720060", '', RawDet0$BS)
      RawDet0$TAG<-gsub("972006000", '', RawDet0$TAG)
      
      RawDet0$dateTime<-as.POSIXct((RawDet0$TIME)/1000, tz="UTC", origin="1970-01-01")
    if (slice)  
       RawDet1 <- RawDet0 %>% mutate(M=minute(RawDet0$dateTime),H=hour(RawDet0$dateTime),D=date(RawDet0$dateTime)) %>% 
         group_by(TAG,BS) %>%
         mutate(Tag_BS=factor(paste(BS,"-",TAG ))) %>% 
         add_count()%>%
         ungroup() %>% 
         group_by(M,H,D,TAG,BS) %>% 
         slice_head() %>% 
         ungroup()
      else
         RawDet1 <- RawDet0
      
      print('The time to download and reshpe data was')
      print( Sys.time()-STime)
      return(RawDet1)
      }
plotCountDet <- function(RawDet1,xAxisRes='3 hours')
      {start_time <- RawDet1$dateTime[1]
RawDet1 %>% ggplot(aes(dateTime, factor(paste(BS,"-",TAG ))))+
   geom_point(aes(x=dateTime,y= factor(paste(BS,"-",TAG )),group=TAG, color=TAG) )+
   # geom_line(aes(x=dateTime,y=BS,group=TAG, color=TAG) ) +
   geom_text(data=RawDet1 %>% distinct(Tag_BS,.keep_all = TRUE) %>% dplyr::select(Tag_BS,n),
             aes(x=start_time, y=Tag_BS, label = n, hjust = +1.1))+
   scale_x_datetime(date_breaks = xAxisRes)+
   theme(axis.text.x=element_text(angle=60, hjust=1)) +
   labs(title= paste(' Beacons detection by Base station'),
        subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
        x = "", 
        y = "Base Stations- Beacons")}
plotBSperformance <- function(RawDet1,BS2plot,TAG2plot=NA ,xAxisRes='12 hours')
      {
   if(is.na(TAG2plot))
   {TAG2plot <- unique(RawDet1$TAG)}
   
   p1 <- RawDet1 %>% filter(BS %in% BS2plot ) %>% filter(TAG %in% TAG2plot ) %>%
      ggplot(aes(dateTime, RSSI))+
      geom_point(aes(x=dateTime,y= RSSI,group=interaction(TAG,BS), color=interaction(TAG,BS),shape =BS) )+
      geom_line(aes(x=dateTime,y= RSSI,group=interaction(TAG,BS), color=interaction(TAG,BS)) ) +
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_blank(),
            legend.position=c(0.97, 0.5),
            legend.background = element_rect(color="black",fill ="grey",#transparent",
                                             size=0.5, linetype="solid")) +
      labs(color="TAG.BS",
           # title= paste(' RSSI'),
           title = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "RSSI")
   p2 <- RawDet1 %>% filter(BS %in% BS2plot ) %>% filter(TAG %in% TAG2plot ) %>%
      ggplot(aes(dateTime, SNR))+
      geom_point(aes(x=dateTime,y= SNR,group=interaction(TAG,BS), color=interaction(TAG,BS),shape =BS) )+
      geom_line(aes(x=dateTime,y= SNR,group=interaction(TAG,BS), color=interaction(TAG,BS)) ) +
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_blank(),
            legend.position='none') +
      labs(
           # title= paste(' SNR'),
           # subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "SNR")
   p3 <- RawDet1 %>% filter(BS %in% BS2plot )  %>%
      ggplot(aes(dateTime, GAIN))+
      geom_point(aes(x=dateTime,y= GAIN,group=BS, color=BS) )+
      geom_line(aes(x=dateTime,y= GAIN,group=BS, color=BS) ) +
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_text(angle=60, hjust=1),
            legend.position=c(0.97, 0.4),
            legend.background = element_rect(color="black",fill ="grey",
                                             size=0.5, linetype="solid")) +
      labs(
           # title= paste(' GAIN'),
           # subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "GAIN")
   grid.arrange(p1,p2,p3, nrow = 3)
   
}
plotTagsCount <- function (RawDet1,xAxisRes='3 hours',BS2plot=NA) #(RawDet1,BS2plot,TAG2plot=NA ,xAxisRes='12 hours')
      {
   A <- RawDet1 %>% 
      group_by(H,M,D,BS) %>%
      summarise(uniqueTagsDetected=length(unique(TAG)),dateTime=min(dateTime),
                meanSNR=mean(SNR),meanRSSI=mean(RSSI)) %>%
      arrange(dateTime) %>% 
      ungroup()
   if(any(!is.na(BS2plot)))
      A <- A %>% filter(BS %in% BS2plot)
   
   p1 <-   A %>% ggplot(aes(dateTime, uniqueTagsDetected))+
      geom_point(aes(x=dateTime,y= uniqueTagsDetected,group=BS, color=BS) )+
      geom_line(aes(x=dateTime,y= uniqueTagsDetected,group=BS, color=BS) )+
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      theme(axis.text.x=element_blank()) +
      labs(title= paste('number unique Tags '),
           subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "number of unique tags")
   
   p2   <-   A %>% ggplot(aes(dateTime, meanSNR))+
      geom_point(aes(x=dateTime,y= meanSNR,group=BS, color=BS) )+
      geom_line(aes(x=dateTime,y= meanSNR,group=BS, color=BS) )+
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      labs(title= paste('mean SNR of detections'),
           subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "mean SNR")
   grid.arrange(p1,p2, nrow =2)
   
}
plotDriftCases <- function(RawDet2,xAxisRes= '3 hours')
      {
   A <-  RawDet2 %>% group_by(TAG,BS) %>% 
      arrange(TIME) %>% 
      mutate(TimeDiff=SAMPLES_CLK-lag(SAMPLES_CLK)) %>% 
      ungroup()%>% 
      filter(TimeDiff>8e6/2&TimeDiff<8e6*1.5)
   
   A <- A %>%     pivot_wider(id_cols= c('TAG','TIME','dateTime'),names_from = BS, values_from = TimeDiff,names_prefix='Diff') %>% 
      mutate(meanDiff=rowMeans(select(.,starts_with('Diff')),na.rm=T),N=rowSums(select(.,starts_with('Diff')),na.rm=T)/meanDiff)
   A <- A %>% pivot_longer(
      cols = starts_with("Diff"),
      names_to = "BS",
      names_prefix = "Diff",
      values_to='sampDiff',
      values_drop_na=T
   ) 
   A$samp_div <- abs(A$meanDiff-A$sampDiff)
   A <- A %>% filter(samp_div>10 | N<2)
   
   D <- as.data.frame(table(A$BS,date(A$dateTime)))
   colnames(D) <- c('BS','Date','Count')
   D <- D %>% filter(Count>10)
   
   start_time <- A$dateTime[1]
   p1 <- A %>%  filter(samp_div>10) %>%
      ggplot(aes(dateTime, samp_div))+
      geom_point(aes(x=dateTime,y= samp_div,group=BS, color=BS) )+
      # geom_line(aes(x=dateTime,y= samp_div,group=BS, color=BS) ) +
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_text(angle=60, hjust=1),
            legend.position=c(0.97, 0.5),
            legend.background = element_rect(color="black",fill ="grey",#transparent",
                                             size=0.5, linetype="solid")) +
      labs(title= paste('Beacon detection Base station Sample clock drift '),
           subtitle = paste('period= ',RawDet2$dateTime[1],' - ',max(RawDet2$dateTime)),
           x = "", 
           y = "sample clock drift")

   
   p2 <- A %>% filter(N<2 ) %>%mutate(Tag_BS=paste(BS,"-",TAG ), ) %>% 
      group_by(Tag_BS) %>% mutate(count_TAG_BS=n()) %>% ungroup() %>% 
      ggplot(aes(dateTime, factor(paste(BS,"-",TAG ))))+
      geom_point(aes(x=dateTime,y= factor(paste(BS,"-",TAG )),group=TAG, color=TAG) )+
      # geom_line(aes(x=dateTime,y= factor(paste(BS,"-",TAG )),group=TAG, color=TAG)) +
      geom_text(aes(x=start_time, y=Tag_BS, label = count_TAG_BS, hjust = +1.1))+
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      labs(title= paste('unsynced detections'),
           subtitle = paste('period= ',RawDet2$dateTime[1],' - ',max(RawDet2$dateTime)),
           x = "", 
           y = "Base Stations- Beacons")
   
   grid.arrange(p1,p2, nrow = 2)
   return(D)
   
}

Start_Time_Str     <- c('2021-05-31 07:00:00')
End_Time_Str       <- c('2021-05-31 09:00:00')
sample_timesPerDay <- 24*60
sample_lengthMinute<- 1
FullTag <- c(972006000003,972006000004,972006000006)
# 
RawDet1 <- Get_ATLAS_Det_DATA (Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute,FullTag )
# 
plotCountDet(RawDet1,xAxisRes='1 min')
plotBSperformance(RawDet1,BS2plot=c( "03") ,xAxisRes='1 hours')
# 
# 
RawDet2 <- Get_ATLAS_Det_DATA (Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute,FullTag,slice = F,Fields='TAG,TIME,BS,SAMPLES_CLK' )
plotDriftCases(RawDet2,xAxisRes='1 min')
# 
# FullTag2 <- (seq(from = 972006000007, to =972006001000, by = 1))
# RawDet2 <- Get_ATLAS_Det_DATA (Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute,FullTag2 )
# plotTagsCount(RawDet2,xAxisRes='3 hours') # ,BS2plot=c("01","02","03","04","07")





