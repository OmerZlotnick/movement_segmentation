library(dplyr)
library(leaflet)

source("../existing_functions/Functions_maps.R")
source("../existing_functions/Functions-main/atl_plots.R")

LibrariesForAtlas()

itm<-"+init=epsg:2039" #ITM coordinations
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

tw <- 9
k <- 4

# to_plot <- "567_2021-06-30"
# to_plot <- "567_2021-07-30"
# to_plot <- "567_2021-08-15"
# to_plot <- "568_2021-06-30"
# to_plot <- "568_2021-07-30"
# to_plot <- "568_2022-08-15"
# to_plot <- "680_2022-03-30"
# to_plot <- "680_2022-04-30"
# to_plot <- "680_2022-05-30"
# to_plot <- "682_2022-03-10"
# to_plot <- "682_2022-04-30"
to_plot <- "682_2022-05-30"

data_path <- paste0("../outputs/9 examples/tw_",tw,"/k_",k,"/",to_plot,".csv")

data <- read.csv(data_path)

data$group <- as.factor(data$group)

dayformap <- data
#dayformap <- subset(data, date == "2021-10-20") 

#Convert the data to a spatialPointDataFrame:
coordinates(dayformap) <- ~X+Y
proj4string(dayformap) <- CRS(itm)
llpd1 <- spTransform(dayformap, wgs84)

m <- leaflet() %>% 
  addProviderTiles('Esri.WorldImagery')

m <-  addPolylines(m, data=llpd1@coords, weight = 0.5, opacity = 2, color = "black")
m

jet.colors <- #taken from http://senin-seblog.blogspot.com/2008/09/some-r-color-palettes.html
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

colors=jet.colors(k+1)

pal <- colorFactor(colors, domain = as.factor(-1:k), ordered = TRUE)

dayformap <- as.data.frame(dayformap)

# ggplot(dayformap,aes(x= X, y= Y, color= group))+
#   geom_line(color = "black")+
#   geom_point() +
#   theme_bw() +
#   theme(panel.grid = element_blank())

n <- m %>%
  addCircleMarkers(data=dayformap, color =~pal(group),stroke = FALSE, radius = 2, fillOpacity = 1,
                   label = ~paste("Tag number:" ,TAG, "Group:", group),
                   popup = ~paste(TAG, "<br>Time:", substr(dateTime, 12, 19),
                                  "<br>Detection time:", dT,
                                  "<br>Group:", group,
                                  "<br>Speed (m/s)", round(spd, 3)))
n

# ggplot(data, aes(x = dT, y = group)) +
#   geom_point()



# limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot
# 
# xlims <- round(limits$x)
# ylims <- round(limits$y)
# 
# plot(dayformap$X,dayformap$Y,asp=1)
# plotsqure(dayformap$X,dayformap$Y)
# plotdays(data, 405)  
# atl_mapleaf(dayformap)
# 
# atl_mapgg(dayformap)
