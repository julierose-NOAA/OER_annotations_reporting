#Scan individual dive summary .txt files and extract the distance traveled by
#the ROV in meters. This information is only reported in dive summary files
#for dives conducted after 2020

import_distance_traveled_post2020 <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 3, sep="")
  distance_traveled <- as.numeric(dive_summary[43])
}