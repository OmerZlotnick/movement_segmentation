
# required packages: tidyverse, dplyr, lubridate,  ggplot2

# The function takes movement data, with one line per date and plot the 
# deployment dates of all individuals in the data set.
# (In case you have multiple lines per date you have to first slice it, 
# or talk to me :) )

# Arguments: 
    # movement_df -  data containing date and id columns
    # name        -  the species name 
    # date        -  the date column 
    # id          -  the id column (ring / tag / ...)
# for example: plot_deployment_dates(daily_movement, 'Lapwings', daily_movement$date, daily_movement$ring)

# Return plot


plot_deployment_dates <- function(movement_df, name, date, id){
  last_date <- max(unique(date))
  
  plot <- movement_df %>% 
    dplyr::mutate(month = month(date)) %>% 
    ggplot(aes(x = date, 
               y = as.factor(id),
               colour = as.factor(month)))+
    geom_point()+
    theme_bw() +
    scale_x_date(date_breaks = '2 months')+
    labs(title= paste(name, ' - deployment dates'),
         subtitle = paste('N = ', n_distinct(id), ', last date:', last_date),
         x = "Date", 
         y = "Individuals", 
         colour='month')+
    theme(axis.title = element_text(size = 25),
          axis.text=element_text(size=15),
          plot.title = element_text(face = 'bold', size = 30),
          plot.subtitle = element_text(face = 'bold', size = 25),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 12))
  
  return(plot)
}