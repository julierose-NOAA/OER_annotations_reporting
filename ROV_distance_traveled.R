#Tests for package availability and installs missing packages that are needed
#in order to run this code:
if(!require('dplyr'))install.packages('dplyr')
if(!require('lubridate'))install.packages('lubridate')
if(!require('geosphere'))install.packages('geosphere')
if(!require('leaflet'))install.packages('leaflet')
if(!require('stringr'))install.packages('stringr')
if(!require('TTR'))install.packages('TTR')
if(!require('ggplot2'))install.packages('ggplot2')
#------------------------------------------------------------------------------
#source functions
#need to manually set the file path for the functions folder within your local repository
function_names <- list.files(path = "C:/Users/julie.rose/Documents/GitHub/OER_annotations_reporting/Functions/", 
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)

#-------------------------------------------------------------------------------
#set up steps
#set file paths and data names that correspond to file names
ROV_filepath <- "C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX1803/ROV_tracks/"
expedition <- "EX1803"

#location of the benthic times data frame that is an output of the 
#Benthic_annotations_cleaning script. This contains start/end times for all dives
#in an expedition so only needs to be imported once.
benthic_times_wd <- paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/", 
                           expedition, "/exports/")

#import benthic times data frame
benthic_times <- read.csv(paste0(benthic_times_wd,"benthic_times_",expedition,".csv"), 
                          header = TRUE)
benthic_times$benthic_start <- lubridate::ymd_hms(benthic_times$benthic_start)
benthic_times$benthic_end <- lubridate::ymd_hms(benthic_times$benthic_end)
str(benthic_times) #check to make sure these are the right times for your expedition

#uses names of ROV track files in the expedition folder to generate the vector
#of dives to use in the loop below
ROV_dive_numbers <- list.files(path = ROV_filepath, pattern = "[.]csv$", full.names = TRUE)
ROV_dive_numbers <- sapply(ROV_dive_numbers, ROV_dive_number_extract, USE.NAMES = FALSE)

ROV_distance_traveled_vec <- c() #this will become the vector of distances
#-------------------------------------------------------------------------------
#the remaining code runs as a loop processing each dive and creating a dataframe
#please note processing time is approximately 8 minutes PER DIVE on a standard
#laptop

for(i in ROV_dive_numbers){
#import
ROV_import_df <- ROV_import(paste0(ROV_filepath,expedition,"_DIVE",i,"_ROVtrack.csv"))

#clean
ROV_clean_df <- ROV_clean(ROV_import_df)

#join with benthic times
ROV_join <- dplyr::left_join(ROV_clean_df, benthic_times,
                                  dplyr::join_by("dive_number" == "dive_number"))

#filter for benthic part of dive
ROV_benthic <- ROV_join |> 
  dplyr::filter(UTC>=benthic_start & UTC<=benthic_end)


#-------------------------------------------------------------------------------
#Smoothing
#iterate generation of smooths across full dataset, calculate distance traveled 
#for each smooth, save into vector
ROV_SMA_window <- seq(from = 1, to = nrow(ROV_benthic), by = 100)
ROV_SMA_distance <- c()

for(j in ROV_SMA_window){
  ROV_smooth <- ROV_benthic |> 
    dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd, n = j),
                  Lon_SMA = TTR::SMA(longitude_dd, n = j),
                  Depth_SMA = TTR::SMA(depth_m, n = j))
  ROV_distance_smooth <- ROV_distance(ROV_smooth, lat = Lat_SMA, long = Lon_SMA)
  ROV_distance_m <- sum(ROV_distance_smooth$distance_3D_m, na.rm = TRUE)
  ROV_SMA_distance <- c(ROV_SMA_distance, ROV_distance_m)
}

#create data frame of smoothing window and total ROV distance traveled
ROV_SMA_df <- as.data.frame(cbind(ROV_SMA_window, ROV_SMA_distance))

#-------------------------------------------------------------------------------
#Outlier Detection

#add column with differences between pairs of distances across rows
ROV_SMA_df <- ROV_SMA_df |> 
  dplyr::mutate(Distance_diff = c(diff(ROV_SMA_distance),0)) #zero needed to make full column

#MAD-median outlier detection across differences. Uses function described in 
# Wilcox, R.R. (2022) "Introduction to Robust Estimation and Hypothesis Testing"
# Fifth Edition, Elsevier. https://osf.io/xhe8u/
MadMed_out_dist <- out(ROV_SMA_df$Distance_diff) #output is a list
ROV_SMA_df_outliers <- as.data.frame(MadMed_out_dist[[3]])
colnames(ROV_SMA_df_outliers) = c("distance")
summary(ROV_SMA_df_outliers) #visual check

outlier_threshold <- ROV_SMA_df |> 
  dplyr::filter(!Distance_diff %in% ROV_SMA_df_outliers$distance) |> 
  dplyr::first()

ROV_distance_traveled <- outlier_threshold$ROV_SMA_distance

ROV_distance_traveled_vec <- c(ROV_distance_traveled_vec, ROV_distance_traveled)

print(paste0("Dive",i," completed"))
}

ROV_distance_df <- data.frame(expedition = expedition, 
                              dive_number = as.numeric(ROV_dive_numbers), 
                              distance_m = ROV_distance_traveled_vec)

write.csv(ROV_distance_df, paste0(wd,"/exports/", expedition,"_ROV_distance.csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
#Visualize outlier detection results

# ggplot2::ggplot(ROV_SMA_df, ggplot2::aes(x = ROV_SMA_window, y = ROV_SMA_distance)) +
#   ggplot2::geom_point() +
#   ggplot2::labs(x = "Number of ROV position points used in simple moving average smooth",
#        y = "ROV distance traveled (m)",
#        title = "Change in predicted ROV distance traveled with increased smoothing",
#        subtitle = expedition) +
#   ggplot2::geom_vline(xintercept = ROV_distance_traveled, color = "#FF6C57", linewidth = 1.5) +
#   ggplot2::theme_bw()

#------------------------------------------------------------------------------
#Visualize raw and smoothed track lines

# ROV_smooth_predicted <- ROV_benthic |>
#   dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd, n = ROV_threshold$ROV_SMA_window),
#                 Lon_SMA = TTR::SMA(longitude_dd, n = ROV_threshold$ROV_SMA_window),
#                 Depth_SMA = TTR::SMA(depth_m, n = ROV_threshold$ROV_SMA_window))
# 
# #raw data
# ROV_benthic |>
#   leaflet::leaflet() |>
#   leaflet::addTiles() |>
#   leaflet::addPolylines(lng = ~longitude_dd, lat = ~latitude_dd)
# 
# #smoothed data
# ROV_smooth_predicted |>
#   leaflet::leaflet() |>
#   leaflet::addTiles() |>
#   leaflet::addPolylines(lng = ~Lon_SMA, lat = ~Lat_SMA)