# a general template for running ATLAS codes
# Includes examples for most functions in the repository

#---------- cleaning and setting preferences ----------------------------- 
{
  rm(list=ls()) # clean history 
  options(digits = 14) # Makes sure long numbers are not abbreviated.
  rm(list = setdiff(ls(), lsf.str())) # removes data, not
  # Sys.setenv(TZ = 'UTC')
}
# --------- Setting paths  ------------
{
  general_path <- "../" # my dell computer directory
  # general_path <- "C:/Users/eitam.arnon/OneDrive - GTIIT/POST/ATLAS/" # my GT computer
  setwd       (paste0(general_path,"my_codes"))
  path2func <- paste0(general_path,"existing_functions/Functions-main/")
  path2data <- paste0(general_path,"outputs/1 locations_data/")
  #install.packages(paste0(general_path,"existing_functions/Functions-main/toolsForAtlas_0.0.0.9001.tar.gz"), repos = NULL, type = "source")
  #install.packages("remotes")
  #remotes::install_github("sivanMargalit/toolsForAtlas")
  source(paste0(path2func,"ConnectLib.R"))
  ConnectLib(path2func)
  source("../existing_functions/Functions-main/Track_cpp.R")
  #source(paste0(path2func,"addLocAttribute.R"))
  #source(paste0(path2func,"addDistanceSpeed.R"))
}

calculate_angle  <- function(locations_df) {
  
  in_work_df <- locations_df %>%
    mutate(angle = 0.0)
  
  for(i in 1:nrow(in_work_df)){
    
    View(in_work_df)
    
    
  }
}

files_lst <- list.files(path2data)
dir.create("../outputs", showWarnings = FALSE)
dir.create("../outputs/2 filtered_data", showWarnings = FALSE)

#for(file in files_lst[1:length(files_lst)]){
for(file in files_lst){
  
  path_to_file <- paste0(general_path,"outputs/1 locations_data/",file)
  
  RawLoc1 <- read.csv(path_to_file) 
  
  RawLoc1 <- RawLoc1 %>%
    addLocAttribute(locAttributs=c("distanceSpeed", "locQuality","angle"))
  
  ux <- unique(round(RawLoc1$dT))
  mode_value <- ux[which.max(tabulate(match(round(RawLoc1$dT), ux)))]
  print(mode_value)
  
  RawLoc2 <- RawLoc1
  
  # if (mode_value < 7){
  #   RawLoc2 <- RawLoc1[seq(1,length(RawLoc1$dT),2),]
  # } 
  
  rm(RawLoc1)
  
  FiltLoc1 <- TrackConfidanceLevelcpp(RawLoc2) %>%
    filter(Conf == 2) %>%
    addLocAttribute(locAttributs=c("distanceSpeed", "locQuality","angle"))
  
  rm(RawLoc2)
  
  TempLoc1 <- FiltLoc1
  rm(FiltLoc1)
  
  for(i in 1:1){
    TempLoc1 <- atl_unifiedFilter(TempLoc1,stdLimit = 40,spdLimit = 25,spdSteps = 10,distLimit = 300,distSteps = 5,ellipsMovangle = 15,ellipsMovRadius = 15,ellipsMovDist = 30)
    TempLoc1 <- addLocAttribute(TempLoc1, locAttributs = c("distanceSpeed","angle"))
  }
  
  FiltLoc2 <- TempLoc1
  rm(TempLoc1)
  
  FiltLoc2 <- addLocAttribute(FiltLoc2, locAttributs=c("distanceSpeed","angle"))
  
  FiltLoc3 <- FiltLoc2[,c("TAG","X","Y", "LON","LAT","dateTime", "date", "distance","dT","spd")]
  
  output_path <- paste0("../outputs/2 filtered_data/", file)
  write.csv(FiltLoc3, output_path, row.names = FALSE)
  
}

