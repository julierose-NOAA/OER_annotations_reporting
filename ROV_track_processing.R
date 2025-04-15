#import, smooth, and visualize ROV track data with the ultimate goal of 
#calculating ROV distance traveled per dive
#note all OER coordinates are in WGS84

#-------------------------------------------------------------------------------
#NEW FUNCTIONS
#move these to functions folder and add source() when this work is finished


#reads in .csv file and stores the filename in a column, for use in 
#automating extraction of dive number in the ROV_clean function
ROV_import <- function(filename){
  ROV <- read.csv(filename, header = TRUE)
  ROV$dive_number <- filename
  ROV
}

#adds expedition name, extracts dive number, selects relevant columns, converts
#unixtime to UTC
ROV_clean <- function(x){
  x |> 
    dplyr::mutate(expedition = data_name) |> 
    dplyr::select(expedition, dive_number, "UNIXTIME","DEPTH","ALT","LAT_DD","LON_DD") |> 
    dplyr::rename(unix_time = "UNIXTIME", depth_m = "DEPTH", altitude_m = "ALT",
                  latitude_dd = "LAT_DD", longitude_dd = "LON_DD") |>
    dplyr::mutate(UTC = lubridate::as_datetime(unix_time)) |> 
    dplyr::mutate(across(dive_number, \(x) stringr::str_extract(x, "DIVE(\\d{2})"))) |> 
    dplyr::mutate(across(dive_number, \(x) stringr::str_replace(x, "DIVE",""))) |> 
    dplyr::mutate(dive_number = as.numeric(dive_number))
}
#-------------------------------------------------------------------------------
#creates vector of filenames in the ROV_tracks folder
ROV_paths <- list.files(paste0(wd, "/ROV_tracks"),
                        pattern = "[.]csv$", full.names = TRUE)

#for each file in the ROV_paths vector, applies import function - creates a 
#list where each element is the ROV track data for a single dive
ROV_import_list <- purrr::map(ROV_paths, \(x) ROV_import(x))

#applies cleaning function to each element in the list and combines the list
#elements together by row into a data frame with all dives
ROV_import_clean <- ROV_import_list |> 
  purrr::map(ROV_clean) |> 
  purrr::list_rbind()

View(ROV_import_clean)

#Joins the ROV track data frame with the benthic times data frame to facilitate
#extracting the benthic portion of each dive
ROV_join <- dplyr::left_join(ROV_import_clean, benthic_times,
                   dplyr::join_by("dive_number" == "dive_number"))

str(ROV_join)

#extracts benthic part of each dive
ROV_benthic <- ROV_join |> 
  dplyr::group_by(dive_number) |> 
  dplyr::filter(UTC>=benthic_start & UTC<=benthic_end) |> 
  dplyr::ungroup()

# ROV_test_import <- read.csv("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX1803/ROV_tracks/EX1803_DIVE03_ROVtrack.csv")
# 
# #eventually pull this from benthic_times dataframe for multiple dives
# ROV_benthic_start_time <- as.POSIXct("2018-04-16 14:56:48", tz = "UTC")
# ROV_benthic_end_time <- as.POSIXct("2018-04-16 21:36:19", tz = "UTC")
# 
# ROV_test <- ROV_test_import |> 
#   dplyr::select("UNIXTIME","DEPTH","ALT","LAT_DD","LON_DD") |> 
#   dplyr::rename(unix_time = "UNIXTIME", depth_m = "DEPTH", altitude_m = "ALT",
#                 latitude_dd = "LAT_DD", longitude_dd = "LON_DD") |> 
#   dplyr::mutate(UTC = lubridate::as_datetime(unix_time)) |>
#   dplyr::filter(UTC>=ROV_benthic_start_time & UTC<=ROV_benthic_end_time)
# 
# #eventually add expedition and dive number
# 
# View(ROV_test)

#-------------------------------------------------------------------------------
#smoothing lat/long/depth because the high resolution data can be messy

#running average of lat/long/depth
ROV_SMA <- ROV_benthic |> 
  dplyr::group_by(dive_number) |> 
  dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd, n = 1000),
                Lon_SMA = TTR::SMA(longitude_dd, n = 1000),
                Depth_SMA = TTR::SMA(depth_m, n = 1000)) |> 
  dplyr::ungroup()

str(ROV_SMA)

#-------------------------------------------------------------------------------
#View tracks in leaflet
library(leaflet)

#if you want to pick a center point to set view
ROV_SMA |>
  dplyr::filter(dive_number == 1) |> 
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~longitude_dd, lat = ~latitude_dd, radius = 1, popup = ~paste0(latitude_dd, "-", longitude_dd))

#visually compare original and smooths
ROV_SMA |>
  dplyr::filter(dive_number == 1)
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~longitude_dd, lat = ~latitude_dd, group = ~dive_number)

ROV_SMA |> 
  dplyr::filter(dive_number == 1) |>
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~Lon_SMA, lat = ~Lat_SMA)

#---------------------------------------------------------------------
#Distance calculations
library(geosphere)

#-------------------------------------------------------------------------------
#NEW FUNCTION
#move to functions folder and add source() when this work is finished
#use cbind instead of select because distHaversine requires a matrix input
#the zero at the end of the column ensures the correct number of rows
ROV_distance <- function(data, lat, long){
  data |> 
    dplyr::mutate(Haversine = c(distHaversine(cbind({{long}},{{lat}})),0),
                  depth_distance_m = c(diff(altitude_m),0),
                  distance_3D_m = sqrt((depth_distance_m^2) + (Haversine^2)),
                  speed = distance_3D_m/0.2)
                  #outlier = speed > min_outlier)
  
}

#-------------------------------------------------------------------------------
#Test function on smoothed data
#
library(ggplot2)

ROV_SMA_test <- ROV_SMA |> 
  dplyr::group_by(dive_number) |> 
  ROV_distance(lat = Lat_SMA, long = Lon_SMA) |>
  dplyr::ungroup()

ROV_SMA_test |> 
  dplyr::group_by(dive_number) |> 
  dplyr::summarise(dist = sum(distance_3D_m, na.rm = TRUE),
                   med_speed = median(speed, na.rm = TRUE))
  