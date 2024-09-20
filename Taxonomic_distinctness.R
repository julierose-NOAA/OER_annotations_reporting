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

#this function cleans up the annotation file - selects relevant columns only, 
#renames columns, and reorders. Separates the expedition name from the dive 
#number. Selects the biological data by filtering on the "biota" column.

clean_annotation <- function(x) { 
  x |> 
    select(`Dive Name`, `Start Date`, `Annotation ID`, 
           `DEEPDISCOVERERNAV01_23975_Latitude`,
           `DEEPDISCOVERERNAV01_23975_Longitude`,
           `SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen Concentration`,
           `SBECTD9PLUSDEEPDISCOVERER_23978_Temperature`,
           `SBECTD9PLUSDEEPDISCOVERER_23978_Depth`,
           `SBECTD9PLUSDEEPDISCOVERER_23978_Practical Salinity`, 
           `Biota`,`Taxonomy`, `Phylum`, `Class`, `Order`, `Family`, `Genus`, 
           `Species`) |> 
    separate(`Dive Name`, c("cruise","dive_number"), sep = "_") |> 
    rename(date_time = `Start Date`,
           annotation_ID = `Annotation ID`,
           latitude_deg = `DEEPDISCOVERERNAV01_23975_Latitude`,
           longitude_deg = `DEEPDISCOVERERNAV01_23975_Longitude`,
           oxygen_mgl = `SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen Concentration`,
           temp_degC = `SBECTD9PLUSDEEPDISCOVERER_23978_Temperature`,
           depth_m = `SBECTD9PLUSDEEPDISCOVERER_23978_Depth`,
           salinity_psu = `SBECTD9PLUSDEEPDISCOVERER_23978_Practical Salinity`,
           biota = `Biota`,
           taxonomy = `Taxonomy`,
           phylum = `Phylum`,
           class = `Class`,
           order = `Order`,
           family = `Family`,
           genus = `Genus`,
           species = `Species`) |> 
    mutate(dive_number = toupper(dive_number)) |> 
    mutate(dive_number = gsub("DIVE","",dive_number)) |> 
    mutate(dive_number = as.numeric(dive_number)) |> 
    filter(biota == "Biota") 
}

#Read in a group of annotation files saved locally as .csv exports from SeaTube
#Save each file as a dataframe within a list, apply the clean function described
#above to each dataframe within this list, then combine them into a single df

annotation_paths<-list.files(
  "C:/Users/julie.rose/Documents/1-OER/Biodiversity/annotations", 
  pattern = "[.]csv$", full.names = TRUE)

annotation_list<-map(annotation_paths, 
                     \(x) read_csv(x, col_names = TRUE, na = ""))

annotation_clean<- annotation_list |> 
  map(clean_annotation) |> 
  list_rbind()

#Use function described above to get start and end times for the benthic
#portion of individual dives. Apply these functions across a group of dive 
#summary .txt files stored locally. Create a dataframe that contains dive number
#plus benthic start and end times for the group of dives.

dive_summary_paths<-list.files(
  "C:/Users/julie.rose/Documents/1-OER/Biodiversity/dive_summaries", 
  pattern = "[.]txt$", full.names = TRUE)

benthic_start_list<-map(dive_summary_paths, 
                        \(x) import_benthic_start(x))

benthic_end_list<-map(dive_summary_paths, 
                      \(x) import_benthic_end(x))

benthic_start<- as.POSIXct(unlist(benthic_start_list))
benthic_end<- as.POSIXct(unlist(benthic_end_list))
dive_number<-c(1:length(benthic_start)) #assumes your dives are
#numbered sequentially starting with 1, this code could use improvement

benthic_times<-data.frame(dive_number,benthic_start,benthic_end)

#Joins the clean annotations dataframe to the benthic times dataframe 
#and then filters the annotations data to only include the benthic portion of
#each dive.

benthic_join<-left_join(annotation_clean, benthic_times, 
                        join_by("dive_number" == "dive_number"))

benthic_annotations<- benthic_join |> 
  group_by(dive_number) |> 
  filter(date_time>=benthic_start & date_time<=benthic_end) |> 
  ungroup()
