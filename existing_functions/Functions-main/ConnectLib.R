# Sourcing packages and local functions
# packages are assumed to be installed (Installing packages is commented and can be used before first use)
# if no input is given, looks for a "functions" directory in current directory   
ConnectLib <- function (path2func="./") {

# Installing packages is commented (sometimes needed before first use)
# if no input is given, looks for a "functions" directory in current directory   
#  ----------------------------- 

# install.packages("mapview")
# install.packages("rgdal")
# install.packages("sf")
library(data.table)
library(dplyr) # summary and re-arrangement of data
library(lubridate) # For date & time  manipulations
# install.packages("dbscan")
library(dbscan) # clustering algorithm
# install.packages("sp")
library(sp) # library to work with spatial objects (for mapping)
# install.packages("leaflet")
library(leaflet) # function to visualise tracks on a map
library(htmltools) # to add "pop-ups" to leaflet maps
# install.packages("RSQLite")
library(RSQLite) # package needed to the ATLAS package for loading sqlite files
# install.packages("ggpubr")
library(ggpubr)# package needed to the ATLAS package (for plotting)
# install.packages("ggmap")
# install.packages("ggplot2")
library(ggmap)
library(ggplot2)
library(mapview)
# install.packages("suncalc")
library(suncalc)
library(DBI)

# ------- packages from Git_hub or local source: 

# install.packages(paste0(general_path,"/Workshop/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# install.packages(paste0(path2func,"/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# devtools::install_github("pratikunterwegs/atlastools")
library(toolsForAtlas) # the ATLAS package (Sivan Margalit) 
  # https://rdrr.io/github/sivanMargalit/toolsForAtlas/
# install.packages("devtools")  # for atlastiools by pratikunter
# devtools::install_github("pratikunterwegs/atlastools") # for atlastiools by pratikunter
# library(atlastools)
  

# source(paste0(path2func,"Movement_Metrics_Functions.R"))   # my enhancements for ADP

# source(paste0(path2func,"points_to_line.R"))
  
  source(paste0(path2func,'ellips_param.R'))
  source(paste0(path2func,"Data_from_ATLAS_server.R"))    # my functions
source(paste0(path2func,"identifyDetections.R"))    # my function
source(paste0(path2func,"visual_filter.R"))       # my functions
source(paste0(path2func,"wrap_ADP.R"))            # my functions
# source(paste0(path2func,"mergeCloseAdp.R"))       # enhancements for ADP (Jerusalem group)
  # A <- mergeCloseAdp(adp.df,adp_rng=20,smp_rte=8,time_gap=5) -       merges close ADP locations from  the Jerusalem group  (closeness is defined according to parameters adp_rng, and time_gap), the function returns 
source(paste0(path2func,"AssignDayNumber.R"))     # my functions
source(paste0(path2func,"atl_plots.R"))           # my functions
source(paste0(path2func,"Tagtime.R"))             # my functions 
source(paste0(path2func,"velocity_filter.R"))     # my functions  
source(paste0(path2func,"DayNight_Progress.R"))     # my functions   
source(paste0(path2func,"smoothingFilters.R"))     # my functions 
source(paste0(path2func,"timeGapBurst.R"))     # my functions 
source(paste0(path2func,"defineMoveStopSegments.R"))     # my functions 
}
