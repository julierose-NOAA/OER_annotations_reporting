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

ROV_test_longlat <- ROV_test |> 
  dplyr::select(longitude_dd,latitude_dd)

ROV_test_longlat_mat <- as.matrix(ROV_test_longlat)
ROV_test_haversine <- distHaversine(ROV_test_longlat_mat)
sum(ROV_test_haversine)

ROV_test_haversine_full <- c(ROV_test_haversine, 0) #to facilitate join
ROV_test_dist <- ROV_test |> 
  dplyr::mutate(Haversine = ROV_test_haversine_full)

View(ROV_test_dist)

#calculate 3D distance, speed, and flag outliers
ROV_test_dist <- ROV_test_dist |> 
  dplyr::mutate(depth_distance_m = c(diff(altitude_m),0),
                distance_3D_m = sqrt((depth_distance_m^2) + (Haversine^2)),
                speed = distance_3D_m/0.2,
                outlier = speed > 1.5)

ROV_outliers <- sum(ROV_test_dist$outlier, na.rm = TRUE)
