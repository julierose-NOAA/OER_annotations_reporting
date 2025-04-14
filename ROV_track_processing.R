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
  dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd, n = 4),
                Lon_SMA = TTR::SMA(longitude_dd, n = 4),
                Depth_SMA = TTR::SMA(depth_m, n = 4)) |> 
  dplyr::ungroup()

str(ROV_SMA)

# ROV_test_SMA100 <- ROV_test |> 
#   dplyr::mutate(Lat_SMA_100 = TTR::SMA(latitude_dd, n = 100),
#                 Lon_SMA_100 = TTR::SMA(longitude_dd, n = 100),
#                 Depth_SMA_100 = TTR::SMA(depth_m, n = 100))
# 
# ROV_test_SMA1000 <- ROV_test |> 
#   dplyr::mutate(Lat_SMA_1000 = TTR::SMA(latitude_dd, n = 1000),
#                 Lon_SMA_1000 = TTR::SMA(longitude_dd, n = 1000),
#                 Depth_SMA_1000 = TTR::SMA(depth_m, n = 1000))
# 
# ROV_test_SMA10000 <- ROV_test |> 
#   dplyr::mutate(Lat_SMA_10000 = TTR::SMA(latitude_dd, n = 10000),
#                 Lon_SMA_10000 = TTR::SMA(longitude_dd, n = 10000),
#                 Depth_SMA_10000 = TTR::SMA(depth_m, n = 10000))

#-------------------------------------------------------------------------------
#View tracks - try out leaflet
library(leaflet)

#pick a center point to set view
ROV_test_fourth_rows |>  
  leaflet() |> 
  addTiles() |>  
  addCircleMarkers(lng = ~longitude_dd, lat = ~latitude_dd, radius = 1, popup = ~paste0(latitude_dd, "-", longitude_dd))

#compare original and smooths
ROV_test |>  
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~longitude_dd, lat = ~latitude_dd) |> 
  setView(lng = -91.6823141862185, lat = 27.0086355473185, zoom = 20)

ROV_test_half_rows |>  
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~longitude_dd, lat = ~latitude_dd) |> 
  setView(lng = -91.6823141862185, lat = 27.0086355473185, zoom = 20)

ROV_test_fourth_rows |>  
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~longitude_dd, lat = ~latitude_dd) |> 
  setView(lng = -91.6823141862185, lat = 27.0086355473185, zoom = 20)

ROV_test_SMA |>  
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~Lon_SMA_4, lat = ~Lat_SMA_4) |> 
  setView(lng = -91.6823141862185, lat = 27.0086355473185, zoom = 20)

ROV_test_SMA100 |>  
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~Lon_SMA_100, lat = ~Lat_SMA_100) |> 
  setView(lng = -91.6823141862185, lat = 27.0086355473185, zoom = 20)

ROV_test_SMA1000 |>  
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~Lon_SMA_1000, lat = ~Lat_SMA_1000) |> 
  setView(lng = -91.6823141862185, lat = 27.0086355473185, zoom = 20)

ROV_test_SMA10000 |>  
  leaflet() |> 
  addTiles() |>  
  addPolylines(lng = ~Lon_SMA_10000, lat = ~Lat_SMA_10000) |> 
  setView(lng = -91.6823141862185, lat = 27.0086355473185, zoom = 20)

#---------------------------------------------------------------------
#Distance calculations
library(geosphere)

ROV_distance <- function(data, lat, long){
  data_subset <- data |>
    dplyr::select({{long}},{{lat}})
  data_mat <- as.matrix(data_subset)
  data_hav <- distHaversine(data_mat)
  data_full <- c(data_hav, 0) #to facilitate join
  data_join <- data |> 
    dplyr::mutate(Haversine = data_full,
                  depth_distance_m = c(diff(altitude_m),0),
                  distance_3D_m = sqrt((depth_distance_m^2) + (Haversine^2)),
                  speed = distance_3D_m/0.2,
                  outlier = speed > min_outlier)
}


ROV_outliers <- sum(ROV_test_dist$outlier, na.rm = TRUE)
ROV_distance_traveled <- sum(ROV_test_dist$distance_3D_m, na.rm = TRUE)
#-------------------------------------------------------------------------------
#Test function on smoothed data
#
library(ggplot2)


ROV_distance_SMA <- ROV_distance(ROV_test_SMA, lat = Lat_SMA_4, long = Lon_SMA_4)
View(ROV_distance_SMA)
sum(ROV_distance_SMA$outlier, na.rm = TRUE)
sum(ROV_distance_SMA$distance_3D_m, na.rm = TRUE)

ROV_distance_SMA1000 <- ROV_distance(ROV_test_SMA1000, lat = Lat_SMA_1000, long = Lon_SMA_1000)
sum(ROV_distance_SMA1000$outlier, na.rm = TRUE)
sum(ROV_distance_SMA1000$distance_3D_m, na.rm = TRUE)

ROV_med_speed <- median(ROV_test_dist$speed, na.rm = TRUE)
ggplot(ROV_test_dist, aes(x = speed)) +
  geom_histogram(color = "#001743", fill = "#C6E6F0") +
  stat_bin(geom = 'text', aes(label = ..count..), position = position_stack(vjust = 1.1), hjust = 0.2, angle = 30) +
  labs(x = "Speed (m/s)", y = "Count", title = "2-point ROV speed calculations based on 3D distance traveled",
       subtitle = "Assumes two points are 0.2 seconds apart") +
  geom_vline(xintercept = as.numeric("0.536448"), linetype = "dotted", color = "#FF6C57", linewidth = 1.5) +
  annotate("text", x = 2, y = 29000, label = "Reported cruise speed", color = "#FF6C57") + 
  annotate("text", x = 2, y = 27000, label = "Median speed", color = "#3B469A") +
  geom_vline(xintercept = as.numeric(ROV_med_speed), color = "#3B469A", linewidth = 1.25) +
  theme_bw()

#-------------------------------------------------------------------------------
#outlier detection in ROV speed data
source(file.choose()) #select most recent Wilcox R stats functions from the Center
#for Open Science https://osf.io/xhe8u/
#note look into adding single function to this repo

#MAD-median outlier detection method. Selected because it is robust to right skew.
MadMed_out <- out(ROV_test_dist$speed)
MadMed_out_df <- as.data.frame(MadMed_out[[3]])
colnames(MadMed_out_df) = c("speed")
summary(MadMed_out_df)
min_outlier <- min(MadMed_out_df$speed)

#visualize outliers against full dataset - tweak annotation positions with new data
ggplot(ROV_test_dist, aes(x = speed)) +
  geom_histogram(color = "#001743", fill = "#C6E6F0") +
  geom_histogram(data = MadMed_out_df, color = "#3B469A", fill = "#3B469A") +
  labs(x = "Speed (m/s)", y = "Count", title = "MAD-Median outlier detection in ROV speed data", subtitle = "2-point ROV speed calculations based on 3D distance traveled") +
  geom_vline(xintercept = as.numeric("0.536448"), linetype = "dotted", color = "#FF6C57", linewidth = 1.5) +
  annotate("text", x = 1.75, y = 27000, label = "Reported cruise speed (0.54 m/s)", color = "#FF6C57") +
  annotate("text", x = 2.2, y = 4000, label = "Flagged outliers (>= 1.2 m/s)", color = "#3B469A") +
  theme_bw()

#-------------------------------------------------------------------------------
#Try a range of simple moving average window sizes and look at effects on 
#number of outliers and calculation of ROV total distance traveled

SMA_window <- seq(from = 1, to = 450, by = 5)
SMA_out <- c()
SMA_distance <- c()

for(i in SMA_window){
  ROV_smooth <- ROV_test |> 
    dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd, n = i),
                  Lon_SMA = TTR::SMA(longitude_dd, n = i),
                  Depth_SMA = TTR::SMA(depth_m, n = i))
  
  ROV_distance_smooth <- ROV_distance(ROV_smooth, lat = Lat_SMA, long = Lon_SMA)
  ROV_out_count <- sum(ROV_distance_smooth$outlier, na.rm = TRUE)
  SMA_out <- c(SMA_out,ROV_out_count)
  ROV_dist_m <- sum(ROV_distance_smooth$distance_3D_m, na.rm = TRUE)
  SMA_distance <- c(SMA_distance, ROV_dist_m)
}

SMA_df <- cbind(SMA_window, SMA_out, SMA_distance)
View(SMA_df)

ggplot(SMA_df, aes(x = SMA_window, y = SMA_out)) +
  geom_point() +
  labs(x = "Number of ROV position points used in simple moving average smooth", 
       y = "Number of outliers",
       title = "Change in number of outliers with increased smoothing") +
  theme_bw()

ggplot(SMA_df, aes(x = SMA_window, y = SMA_distance)) +
  geom_point() +
  labs(x = "Number of ROV position points used in simple moving average smooth", 
       y = "ROV distance traveled (m)",
       title = "Change in calculation of ROV distance traveled with increased smoothing") +
  theme_bw()
