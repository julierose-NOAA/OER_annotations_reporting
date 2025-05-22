#Scan individual dive summary .txt files and extract the benthic end time for
#dives conducted prior to 2020

import_benthic_end_pre2020 <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 2, sep="\t")
  if(dive_summary[17] == "N/A") {
    print("Warning: Not a benthic dive")
  }
  else (end_benthic <- as.POSIXct(dive_summary[17], tz="UTC", 
                            format = "%Y-%m-%dT%H:%M:%OS")
  )
}