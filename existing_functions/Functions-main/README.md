# Functions
a set of function for working with movement data

This repository is an updating library of functions for working with movement data

Here we only specify the general descriptipon of function, its dependency on other functions and, sometimes, its source

Users are encourged to download the functions, save them in their "functions" directory, use, debug and change them freely
Please share your improvments and new functions

## List of functions:
	ATALS_main                                                          - R script, wrapping most of the other functions, usefull for starting a project and for examples!

	ConnectLib(path2func="\functions")                                  - a functions that imports needed libraries and functions (used in ATLAS_main) does not retrun a value
	A <- Data_from_ATLAS_server(Start_Time_Str,End_Time_Str,FullTag)    - a function that import data from the ATLAS server (requires a VPN connection to server), returns a list, of two data frames
	A <- identifyDetections(Loc=RawLoc0,Det=RawDet0,unloclalized =F) - a function that assignes the antennas number used to calculate each localizaion, when specifing unloclalized =F, also return a list of detections not used for localization
	A<- AssignDayNumber(data,DayStartTime="12:00:00",TimeColName="LocTime",Julian=FALSE) - a function that assigns a day number to each location ( a day is defined as starting at DayStartTime, returns the same data with additional column (DAY)
	A <-Tagtime("tagtimes.csv")       - a function to create a data.frame of tags, each with is relevant time limits. accepts an ATLAS location data.frame , returns a data frame
	A <-humanTime2Unix(DATE,TIME,ATLAS=FALSE)       - he function accepts date in d/m/Y format and time in H:M:S format in, UTC and returns a numeric format (time from 1/1/1970), if ATLAS=TRUE the time is returned in milliseconds, else it is returned in seconds

	file: velocity filter includes 4 functions: (All currently works for a single tag)
		A <- velocity_filter (data,spdThreshold, x = "X", y = "Y", time = "TIME", steps=20) - filter according to velocity. Discards a set of drifted points up to length "steps" when  this drifted section required speed over some spdThreshold to connect , returns a data.frame that does not include the discarded points 
		A <- distance_filter (data,distThreshold=500, x = "X", y = "Y", steps=2)            - filter according to distance. Discards a set of thrown-away points up to length "steps", when this drifted section required was overdistThreshold from its surrounding. Returns a  data.frame that does not include the discarded points. 
		A <- matl_simple_dist <- function (data, x = "x", y = "y",step=1) 		    - accepts the data.farme and the names of the itm coordintes and returns a vector of distances between locations seperated by "step" steps
		A <- matl_get_speed <- function (data, x = "x", y = "y", time = "time", type = "in", step=1) - accepts the data.farme and the names of the "ITM" coordintes and returns a vector of velocities between locations seperated by "step" steps
		
	A <-filterByVelocity (data,dataLegend=c("X","Y","TIME"), options=optionsArg)        - a toolsForAtlas function ! not available with ORRSlab github "function" repository. It filters according to velocity. Identefies a reliable point and throws any point which is not connected (with reasonable speed to this reliabl point. might be relativly slow and inefficient. Returns a data.frame that does not include the discarded points

	file: atl_plots include 3 functions
		plotsqure(x,y,a_col="red",override=FALSE) - given two diagonal points of a squre, plots a squre
		plotdays(data,TAG_ex,xlims,ylims) - ploting function,  plot each day on a simple plot and stops (no background)
		atl_mapleaf(data)                 - ploting function,  plot the entire data on a leaflet map, each point is assigned some data
		atl_mapleaf_withstops(list(FiltLoc1,bat_adp),Tags=TAG_ex,Days=dispDAY) - ploting function,  plot two data.frames, the first is a usuall track the second a stop data.frame each point is assigned some data, 
		atl_mapgg(data)                   - ploting function,  plot the entire data on a ggmap map, uses geographic coordinates!
		
	A <- visual_filter(data,printoptions=TRUE,DefalutN2filter=FALSE)  - plots data on a simple plot and allows you to graphicaly discard or collect points, return a dat.frame with data after the filter. If points were collected it return a list, with two data.frames A$filterd and $collected.
	A <- wrap_ADP(FiltLoc1,freq=8)             -wraps the function AdpFixedPoint that calculates stops and their parameters (duration mean position etc) it includes loop over days and tags and post-processing it returns a data.frame with all parameters
	A <- DayNight_Progress(Time_POSI, Lat, Lon)  - calculates time progress in order to compare the activity along days / night with different starting time / length,  the night is valued 0 to 100 from sunset  to sunrise and the day is valued 100 to 200 from sunrise to sunset, the input arguments are  a vector Time_POSI - with time in POSIXct format and a vector (or single value) containing the corresponding coordinates Lat, Lon in geographic / wgs format
	A <- timeGapBurst(data,secTol,minBurstFix,sampRate=8) - Divides the track into bursts of continuous detection (only time-wise), based on amt package. the input variables are #data, a data.frame cantaining the varibles: "X","Y" (coordinates in any format),and "dateTime" (POSIXct fromat).  #secTol, TOLARANCE SEC FOR NEXT BURST, #minBurstFix, Min number ofpoints in burst, and #sampRate, the numinal sampling rate of the tag. The output is a data.frame with the following variables: "X", "Y", "dateTime", "TIME", "timeGapBurst" (a unique burst integer ID ), "pointInBurst" (number of points in the specific burst)
	file: smoothingFilters filter includes 3 functions:
		A <- AvgSmooth(dataBurst,Weight = c(0.25,0.5,0.25)) -  a wraper to two smoothing functions: outlayersmoot and movMean. It operates each of the function on groups of location defined by its "timeGapBurst" value. The input variables are "dataBurst" a data.frame with "X", "Y" and "timeGapBurst" coloumn names and "Weight", an input for the moving avarge smooth. It returns the same data frame with smoothed coordinates. 
		A <- movMean(dataBurst,Weight = c(0.25,0.5,0.25),replace=T) - This function smooths a track by setting any point as weighted average of its neighboring points, The input variables are "dataBurst": a data.frame containing the variables: "X","Y" (coordinates in UTM/ITM format), and "Weight" the weights for the weighted average. The output is the same data.frame with smoothed coordinates. In the case that replace=False is specified, only the "x", "Y" coloumns will be returned!
		A <- outlayerSmooth(dataBurst) - 	This function smooths a track by identifying outlayers, and set thier values as avarage of its neighbors.The input variable is dataBurst: a data.frame containing the variables: "X","Y" (coordinates in UTM/ITM format).  The output is the same data.frame with smoothed coordinates. it is advised to operate the function on separated time-bursts of the data (as wrapped in the function "AvgSmooth")
	

