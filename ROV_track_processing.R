#import, smooth, and visualize ROV track data with the ultimate goal of 
#calculating ROV distance traveled per dive
#note all OER coordinates are in WGS84

#develop functioning workflow for single dive before creating function
ROV_test_import <- read.csv("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX1803/ROV_tracks/EX1803_DIVE03_ROVtrack.csv")

#eventually pull this from benthic_times dataframe for multiple dives
ROV_benthic_start_time <- as.POSIXct("2018-04-16 14:56:48", tz = "UTC")
ROV_benthic_end_time <- as.POSIXct("2018-04-16 21:36:19", tz = "UTC")

ROV_test <- ROV_test_import |> 
  dplyr::select("UNIXTIME","DEPTH","ALT","LAT_DD","LON_DD") |> 
  dplyr::rename(unix_time = "UNIXTIME", depth_m = "DEPTH", altitude_m = "ALT",
                latitude_dd = "LAT_DD", longitude_dd = "LON_DD") |> 
  dplyr::mutate(UTC = lubridate::as_datetime(unix_time)) |>
  dplyr::filter(UTC>=ROV_benthic_start_time & UTC<=ROV_benthic_end_time)

#eventually add expedition and dive number

View(ROV_test)

#-------------------------------------------------------------------------------
#smoothing options for lat/long because the high resolution data can be messy

#every other data point
ROV_test_half_rows <- ROV_test[seq(1, nrow(ROV_test),2), ]
View(ROV_test_half_rows)

#every fourth data point
ROV_test_fourth_rows <- ROV_test[seq(1, nrow(ROV_test),4), ]
View(ROV_test_fourth_rows)

#running average of lat/long/depth
ROV_test_SMA <- ROV_test |> 
  dplyr::mutate(Lat_SMA_4 = TTR::SMA(latitude_dd, n = 4),
                Lon_SMA_4 = TTR::SMA(longitude_dd, n = 4),
                Depth_SMA_4 = TTR::SMA(depth_m, n = 4)) 
View(ROV_test_SMA)

ROV_test_SMA100 <- ROV_test |> 
  dplyr::mutate(Lat_SMA_100 = TTR::SMA(latitude_dd, n = 100),
                Lon_SMA_100 = TTR::SMA(longitude_dd, n = 100),
                Depth_SMA_100 = TTR::SMA(depth_m, n = 100))

ROV_test_SMA1000 <- ROV_test |> 
  dplyr::mutate(Lat_SMA_1000 = TTR::SMA(latitude_dd, n = 1000),
                Lon_SMA_1000 = TTR::SMA(longitude_dd, n = 1000),
                Depth_SMA_1000 = TTR::SMA(depth_m, n = 1000))

ROV_test_SMA10000 <- ROV_test |> 
  dplyr::mutate(Lat_SMA_10000 = TTR::SMA(latitude_dd, n = 10000),
                Lon_SMA_10000 = TTR::SMA(longitude_dd, n = 10000),
                Depth_SMA_10000 = TTR::SMA(depth_m, n = 10000))

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
                  outlier = speed > 1.5)
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
  labs(x = "Speed (m/s)", y = "Count", title = "2-point ROV speed calculations based on 3D distance traveled") +
  geom_vline(xintercept = as.numeric("0.536448"), linetype = "dotted", color = "#FF6C57", linewidth = 1.5) +
  annotate("text", x = 2, y = 29000, label = "Reported cruise speed", color = "#FF6C57") + 
  annotate("text", x = 2, y = 27000, label = "Median speed", color = "#3B469A") +
  geom_vline(xintercept = as.numeric(ROV_med_speed), color = "#3B469A", linewidth = 1.25) +
  theme_bw()
