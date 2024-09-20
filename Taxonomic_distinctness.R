library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(vegan)
library(purrr)

#The inputs for this analysis are locally stored SeaTube annotation .csv files
#and dive summary .txt files. Each annotation needs to have its paired dive
#summary so that the benthic portion of the dive can be extracted. I think this
#could be easily adapted to select just the water column portion of the dive
#if that would be useful.

#these two functions scan individual dive summary .txt files and extract the
#benthic start/end times for later use in filtering the annotation file.
import_benthic_start <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 2, sep="\t")
  start_benthic <- as.POSIXct(dive_summary[10], tz="UTC", 
                              format = "%Y-%m-%dT%H:%M:%OS")
  
}

import_benthic_end <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 2, sep="\t")
  end_benthic <- as.POSIXct(dive_summary[24], tz="UTC", 
                            format = "%Y-%m-%dT%H:%M:%OS")
}