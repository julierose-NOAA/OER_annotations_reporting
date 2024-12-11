#Scan individual dive summary .txt files and extract the benthic start time for
#dives conducted after 2020

import_benthic_start_post2020 <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 3, sep="")
  start_benthic <- as.POSIXct(dive_summary[9], tz="UTC", 
                              format = "%Y-%m-%dT%H:%M:%OS")
  
}