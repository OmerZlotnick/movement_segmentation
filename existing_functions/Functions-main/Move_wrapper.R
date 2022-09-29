pcks=list('dplyr',"move","lubridate") 
sapply(pcks, require, char=TRUE)

loginmove=movebankLogin(username='XXX',password="XXX");

PegionStudy=897179972 # Pigeons
VolturesOhadStudy=6071688 

studyDetails <- getMovebankStudy(study=PegionStudy,login=loginmove) 
refTable <- getMovebankReferenceTable (study=PegionStudy,login=loginmove) 
allAnimals <- getMovebankAnimals(study=PegionStudy,login=loginmove)

allAnimals <- allAnimals %>% mutate(endYear=year(as.POSIXct(timestamp_end, format="%Y-%m-%d %H:%M:%OS", tz="UTC")))
table(allAnimals$endYear)

Animals2012 <- allAnimals %>% filter(endYear<2013)
names2012 <- unique(Animals2012$local_identifier)
# names2012 <- gsub("\\_.*","",unique(Animals2012$animalName)) # another option

MoveStackDatasetOhad=getMovebankData(study=6071688, login=loginmove,animalName= names2012, #
                                           includeExtraSensors=FALSE, deploymentAsIndividuals=FALSE,removeDuplicatedTimestamps=TRUE)#animalName only certain individuals

MoveStackPigeon=getMovebankData(study=897179972, login=loginmove,
                                     includeExtraSensors=FALSE, deploymentAsIndividuals=FALSE,removeDuplicatedTimestamps=TRUE)#animalName only certain individuals

# MoveStackDatasetOhad=getMovebankData(study=6071688, login=loginmove,animalName= c("Y00","A64","A74>T02 white"),
#                                            includeExtraSensors=FALSE, deploymentAsIndividuals=FALSE,removeDuplicatedTimestamps=TRUE)#animalName only certain individuals

limitEndYears <- function(Move,startY,endY)
{
  DF_Move <- as.data.frame(Move)
  DF_Move <- DF_Move %>% 
             mutate(endYear=year(as.POSIXct(timestamp_end, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))) %>% 
             filter(endYear>=startY&endYear<endY)
  Move <- move(x=DF_Move$location_long, y=DF_Move$location_lat, 
                  time=as.POSIXct(DF_Move$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                  proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                  data=DF_Move, animal=DF_Move$trackId, sensor=DF_Move$sensor)
  return(Move)
  
}

MoveStackDatasetOhad2021 <- limitEndYears(MoveStackDatasetOhad_names,startY=2018,endY = 2022 )

CombineMove <- function(Move1,Move2)
{  
DF_Move <- merge(as.data.frame(Move1),as.data.frame(Move2),all=T)
DF_Move <- DF_Move %>% arrange(trackId,timestamp)
MoveAll <- move(x=DF_Move$location_long, y=DF_Move$location_lat, 
                time=as.POSIXct(DF_Move$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                data=DF_Move, animal=DF_Move$trackId, sensor=DF_Move$sensor)
return(MoveAll)}

BasicFilterMove <- function(Move,speedThreshold=120)
{
  DF_Move <- as.data.frame(Move)
  VarsToRemove <- which(names(DF_Move) %in% c("sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
                                              "sensor_type","mw_activity_count","eobs_accelerations_raw","eobs_acceleration_sampling_frequency_per_axis",
                                               "eobs_acceleration_axes","argos_valid_location_algorithm","argos_sensor_4","argos_sensor_3","argos_sensor_2",
                                               "argos_sensor_1","argos_semi_minor","argos_semi_major","argos_pass_duration","argos_orientation","argos_nopc",
                                               "argos_lat1","argos_lat2","1084088","argos_lon1","argos_lon2","argos_nb_mes","argos_nb_mes_120",
                                               "eobs_key_bin_checksum","eobs_fix_battery_voltage","eobs_battery_voltage","eobs_status",
                                               "eobs_start_timestamp","eobs_type_of_fix","eobs_used_time_to_get_fix","eobs_temperature",
                                               "gps_dop","magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z","ornitela_transmission_protocol",
                                               "tag_voltage","algorithm_marked_outlier","argos_altitude","argos_best_level","argos_lc","argos_iq",
                                               "argos_gdop","argos_error_radius","argos_calcul_freq","location_lat.1","location_long.1","timestamps","height_raw",
                                               "barometric_pressure","barometric_height","battery_charging_current","eobs_activity","manually_marked_outlier",
                                               "eobs_activity_samples", "acceleration_raw_y", "battery_charge_percent", "data_decoding_software","gps_vdop","height_above_ellipsoid",
                                               'acceleration_raw_x','acceleration_raw_z',"acceleration_raw_z","eobs_horizontal_accuracy_estimate","eobs_speed_accuracy_estimate",
                                               "import_marked_outlier","orientation_quaternion_raw_w","orientation_quaternion_raw_x","orientation_quaternion_raw_y","orientation_quaternion_raw_z",
                                               "visible","deployment_id","event_id","tag_local_identifier","comments","death_comments","individual_id","latest_date_born",          
                                               "ring_id","timestamp_start","timestamp_end","number_of_events","number_of_deployments","taxon_detail" ))
 
  DF_Move <- DF_Move[,-VarsToRemove]
  DF_Move <- DF_Move[which(DF_Move$ground_speed<speedThreshold),]
  MoveAll <- move(x=DF_Move$location_long, y=DF_Move$location_lat, 
                  time=as.POSIXct(DF_Move$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                  proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                  data=DF_Move, animal=DF_Move$trackId, sensor=DF_Move$sensor)
  return(MoveAll)
  
  
}


Move_df <- as.data.frame(MoveStackPigeon)

