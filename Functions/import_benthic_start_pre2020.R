#Scan individual dive summary .txt files and extract the benthic start time for
#dives conducted prior to 2020

import_benthic_start_pre2020 <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 2, sep="\t")
  if(dive_summary[10] == "N/A") {
    print("Warning: Not a benthic dive")
  }
  else (start_benthic <- as.POSIXct(dive_summary[10], tz="UTC", 
                              format = "%Y-%m-%dT%H:%M:%OS")
  )
}