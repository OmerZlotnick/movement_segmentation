# accepts a movement data of one tag plots every day separately and allows on-plot filtering and choosing
# input variables:
    # data - a positions data (coordinates are in columns labeled "X" and "Y")
    # printoptions - sholud the function print the options at each step (default=TRUE,
    # DefalutN2filter - see n option (throw  specific points, default=FALSE)
# at every iteration the function waits for user command:
      # c = show next day,
      # i = zoom-in (according to two points on diagonal), 
      # o = zoom-out(back to a scope containin the entire day)
      # s = throw two points square (according to two points on diagonal), 
      # n = throw  specific points (these points should be pointed specifically on the map)
      #     if DefalutN2filter is FALSE, you will be requred to specify the number of point to filter-out
      #     you can stop choosing by clicking stop on the map
      #     if DefalutN2filter was choosen as another value, this is the number of points to filter-out
      # l = collect/mark a line between two points (point two points on the track, all points in b
      #     between will be colored red, then you can either collect them or not)
      # p = collect points in a squre,
      #     specify a square by designating two diagonal points, all points will be colored, yopu can either collect them or not!
      # b = discard current day, break loop return the data collected and filter so far.
      # B = discard current day, move a day backwards 
      # D = discard current day, choose specific day
      # t = set a default number of filtered to points(n),
      # d = toggle option not presentation")
# output
      # the function return a data.frame with all the points left after filtration.
      # if specific points were collected, the function returns a list of two data.frames, 
          # the first contains the points left after filtration
          # the second is the collected points (each point has another variable "segment" that specifies its sample)

visual_filter <- function(data,printoptions=TRUE,DefalutN2filter=FALSE)
{

  DAY_list <- unique(data$DAY)
  if(is.null(DAY_list)) 
      {Er <- simpleError("No DAYs found in the provided data")
      stop(Er)}
  TAG_list <- as.numeric(unique(data$TAG))
  if(is.null(TAG_list)) 
      {Er <- simpleError("No TAGs found in the provided data")
      stop(Er)}
  if(length(TAG_list)>1) 
      {Er <- simpleWarning("you are strongly advised to enter a single tag data, bad things might happen otherwise")
      stop(Er)}
  filtData <- NULL
  Collected_points <- NULL
  xlims <- NULL
  ylims <- NULL
  segment_index <- 1
  for (TAG_ex in TAG_list)
  {
    DAY_list <- unique(data$DAY[which(data$TAG==TAG_ex)])
  print(sprintf("Tag=%3.0f, there are %i available days:",TAG_ex,length(DAY_list)))
  print(DAY_list)
  day_Index <- 1 
  while (day_Index <=length(DAY_list))
  {
    day <-DAY_list[day_Index]
    daydata<-data[which( data$DAY==day),]
    # Tag_ex <- 
    xlims[1] <- min(daydata$X,na.rm =TRUE) # the plot limits are defined for each day
    xlims[2] <- max(daydata$X,na.rm =TRUE)
    ylims[1] <- min(daydata$Y,na.rm =TRUE)
    ylims[2] <- max(daydata$Y,na.rm =TRUE)
   
    
    if(day %in% unique(filtData$DAY)){
      print(sprintf("Day %i was proccessed before.choose: 1 - Work with original data or 2 - work with filtered data", day))
      print(" Choosing (1) will discard previous filtering and collecting")
      data2use <- readline(" :  ")
        if (as.integer(data2use)==1)
              {daydata <- data[which(data$DAY==DAY_list[day_Index]),]
              xlims[1] <- min(daydata$X,na.rm =TRUE) # the plot limits are defined for each day
              xlims[2] <- max(daydata$X,na.rm =TRUE)
              ylims[1] <- min(daydata$Y,na.rm =TRUE)
              ylims[2] <- max(daydata$Y,na.rm =TRUE)
              Xlims <- xlims
              Ylims <- ylims
              dayInd <- which(Collected_points$DAY==DAY_list[day_Index]&Collected_points$TAG==TAG_ex)
              Collected_points <- Collected_points[-dayInd,]
        }else if (as.integer(data2use)==2){
              daydata <- filtData[which(filtData$DAY==DAY_list[day_Index]),]
              xlims[1] <- min(daydata$X,na.rm =TRUE) # the plot limits are defined for each day
              xlims[2] <- max(daydata$X,na.rm =TRUE)
              ylims[1] <- min(daydata$Y,na.rm =TRUE)
              ylims[2] <- max(daydata$Y,na.rm =TRUE)
              Xlims <- xlims
              Ylims <- ylims
        }
        dayInd <- which(filtData$DAY==DAY_list[day_Index]&filtData$TAG==TAG_ex)
        filtData <- filtData[-dayInd,]
        }
    # if(day %in% unique(Collected_points$DAY))

    userinput <- "i"
    Xlims <- xlims
    Ylims <- ylims
    while (userinput!="c")
    {
      plot(daydata$X,daydata$Y,asp=1,col="black",pch = 3,
           main=sprintf("Tag=%3.0f, DAY = %i, time range = %s -> %s ",TAG_ex,day,daydata$dateTime[1],daydata$dateTime[nrow(daydata)]),xlim=Xlims,ylim=Ylims)
      lines(daydata$X,daydata$Y,col="black")
      if(printoptions)
      {
      writeLines("options:  c = show next day,
          i = two points zoom-in, 
          o = zoom-out,
          s = throw two points square, 
          n = throw  specific points,
          l = collect/mark a line between two points 
          p = collect points in a squre,
          b = discard current day, break loop return the data collected and filtered so far,
          B = discard current day, move a day backwards 
          D = discard current day, choose specific day
          t = set a default number of filtered to points(n),
          d = toggle option view  (present/ not present)") #D = choose a spcific day,
      }
      userinput <- readline(sprintf("DAY = %i, what to do? ",day)) # the system askes the user what to do
         
           if (userinput=="i") # zoom-in
             {
             limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot
             Xlims <- round(limits$x[order(limits$x)])
             Ylims <- round(limits$y[order(limits$y)])
             }
            else if (userinput=="o") #zoom-out
            {
            Xlims <- xlims
            Ylims <- ylims
            }
            else if (userinput=="s") #filter-out square
            {
            limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot
            Indeces2Rm <- which(!(daydata$X>min(limits$x,na.rm =TRUE)&(daydata$X<max(limits$x,na.rm =TRUE))&
                                    daydata$Y>min(limits$y,na.rm =TRUE)&(daydata$Y<max(limits$y,na.rm =TRUE))))
            print(sprintf("Removed %i points",nrow(daydata)-length(Indeces2Rm)))
            daydata<-daydata[Indeces2Rm,]  
            }
            else if (userinput=="n") # filter-out n points
            {
              if(DefalutN2filter)
                {points2throw <- identify(daydata$X,daydata$Y,n=DefalutN2filter,labels="x",plot=TRUE)}
              else
                {
                points2throw <- readline("how many points to throw ?") 
                points2throw <- identify(daydata$X,daydata$Y,n=as.numeric(points2throw),labels="x",plot=TRUE)
                }
            daydata <- daydata[-points2throw,]
            }
            else if (userinput=="l") # collect points between to points
            {
              points2collect   <- identify(daydata$X,daydata$Y,n=2,labels="o",plot=TRUE,offset=0)
              few_Collected_points <- daydata[seq(min(points2collect,na.rm =TRUE),max(points2collect,na.rm =TRUE)),]
              points(few_Collected_points$X,few_Collected_points$Y,col="red")
              collect <- readline(sprintf("marked %i points? do you want to save them as segment %i?  (n/y)",nrow(few_Collected_points),segment_index))
              if (collect=="y")
                {
                few_Collected_points$segment <- rep(segment_index,nrow(few_Collected_points))
                segment_index <- segment_index+1
                Collected_points <- rbind(Collected_points,few_Collected_points)
                }
              
            }
            else if (userinput=="p") # collect points in a squre
            {
              limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot
              few_Collected_points<-daydata[which((daydata$X>min(limits$x)&(daydata$X<max(limits$x))&
                                         daydata$Y>min(limits$y)&(daydata$Y<max(limits$y)))),] 
              points(few_Collected_points$X,few_Collected_points$Y,col="red")
              collect <- readline(sprintf("marked %i points? do you want to save them as segment %i?  (n/y)",nrow(few_Collected_points),segment_index))
              if (collect=="y")
              {
                few_Collected_points$segment <- rep(segment_index,nrow(few_Collected_points))
                segment_index <- segment_index+1
                Collected_points <- rbind(Collected_points,few_Collected_points)
              }
            }
            else if (userinput=="D") # choose a specific day
            {
             print(sprintf("TAG=%3.0f, there are %i available days:",TAG_ex,length((DAY_list))))
             print(DAY_list)
              a_day <- readline("choose a day:")
              if (is_empty(which(DAY_list==as.integer(a_day))))
                  {print("invalid day choice, reloading the same day")
                   day_Index <- day_Index
                   } else
                  {day_Index <- which(DAY_list==as.integer(a_day))
                   break
                  }
            }
            else if (userinput=="B") # move a day backwards
              {
              day_Index <- day_Index-1
              day <-DAY_list[day_Index]
              data2use <- readline("present original data (1) or filtered data (2)?. Choosing (1) will discard previous filtering and collecting    ")
              if (as.integer(data2use)==1)
              {daydata <- data[which(data$DAY==DAY_list[day_Index]),]
              xlims[1] <- min(daydata$X,na.rm =TRUE) # the plot limits are defined for each day
              xlims[2] <- max(daydata$X,na.rm =TRUE)
              ylims[1] <- min(daydata$Y,na.rm =TRUE)
              ylims[2] <- max(daydata$Y,na.rm =TRUE)
              Xlims <- xlims
              Ylims <- ylims
              dayInd <- which(Collected_points$DAY==DAY_list[day_Index+1]&Collected_points$TAG==TAG_ex)
              Collected_points <- Collected_points[-dayInd,]
              }else {
              daydata <- filtData[which(filtData$DAY==DAY_list[day_Index]),]
              xlims[1] <- min(daydata$X,na.rm =TRUE) # the plot limits are defined for each day
              xlims[2] <- max(daydata$X,na.rm =TRUE)
              ylims[1] <- min(daydata$Y,na.rm =TRUE)
              ylims[2] <- max(daydata$Y,na.rm =TRUE)
              Xlims <- xlims
              Ylims <- ylims
              }
              dayInd <- which(filtData$DAY==DAY_list[day_Index+1]&filtData$TAG==TAG_ex)
              filtData <- filtData[-dayInd,]
              }
            else if (userinput=="b") # break the while loop and close the function
            {break}
            else if (userinput=="d") #toggle presentation mode (present/ not present)
            {
              if(printoptions) {printoptions <- FALSE}
              else {printoptions <- TRUE}
            }
            else if (userinput=="t") #set a default number of points to choose
            {
              DefalutN2filter <- as.numeric(readline("enter default number for samples to filter (0 = no default) "))
            }  
    }
    if(userinput=="b")
      {break}
    if(userinput!="D")
      {print(sprintf("DAY %i, initial number of points= %i final number of points=%i",day,length(which(data$DAY==day)), nrow(daydata)))
      filtData <- rbind(filtData, daydata)
      day_Index <- day_Index+1}
      }
  }
  if (is.null(Collected_points))
  {return(filtData[order(filtData$TAG,filtData$TIME),])}
  else
  {return(list(filterd=filtData[order(filtData$TAG,filtData$TIME),],
               collected=Collected_points[order(filtData$TAG,filtData$TIME),]))}
}