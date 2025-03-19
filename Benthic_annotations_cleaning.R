#source functions
function_names <- list.files(path = "C:/Users/julie.rose/Documents/GitHub/OER_annotations_reporting/Functions/", 
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)

#set working directory
wd <- "C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX1903L2"
setwd(wd)

#set standard name to refer to your data
data_name <- "EX1903L2"

#create vector of dive numbers for your dataset. The dive landing pages are a 
#good place to find the dive numbers to start with
#https://www.ncei.noaa.gov/waf/okeanos-rov-cruises/
dive_number<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19) 

#create a vector of character descriptors of dives, which will be used to 
#download the dive summary text files
dive_names <- c("DIVE01","DIVE02", "DIVE03", "DIVE04", "DIVE05", "DIVE06", "DIVE07", "DIVE08", "DIVE09", "DIVE10", "DIVE11", "DIVE12", "DIVE13", "DIVE14", "DIVE15", "DIVE16", "DIVE17","DIVE18","DIVE19")

#------------------------------------------------------------------------------

#Read in SeaTube .csv annotation file(s) and apply clean_annotation function.
#This can accommodate a single .csv containing all dives or a group of annotation 
#files saved locally as .csv exports from SeaTube. Uses the number of files
#in the annotation folder to determine which import process to execute.
#If only one file is present in the folder, this code assumes that the file
#follows the naming convention "SeaTubeAnnotations_data_name.csv".

annotation_paths<-list.files(paste0(wd, "/annotations"), 
                             pattern = "[.]csv$", full.names = TRUE)

if (length(annotation_paths > 1)) {
  annotation_list<-purrr::map(annotation_paths, 
                              \(x) read.csv(x, header = TRUE, colClasses = 
                                              c("Common.Count" = "character",
                                                "Component" = "numeric"), na.strings = ""))
  
  annotation_clean<- annotation_list |> 
    purrr::map(clean_annotation) |> 
    purrr::list_rbind()
  
} else {
  annotation_import <- read.csv(paste0(wd, "/annotations/SeaTubeAnnotations_", 
                                       data_name, ".csv"), header = TRUE, 
                                colClasses = c("Common.Count" = "character",
                                               "Component" = "numeric"), na.strings = "")
  annotation_clean <- clean_annotation(annotation_import)
}

#check to make sure data imported in correct format and visually look correct
str(annotation_clean)
View(annotation_clean)

#-------------------------------------------------------------------------------
#Download dive summary text files for use in extracting the benthic portion of
#the dive, and ROV tracks .csv files to calculate distance traveled metric.
#Save to two new subdirectories within the existing expedition directory
data_name_lower <- tolower(data_name)
dir.create(paste0(wd,"/dive_summaries/"))
dir.create(paste0(wd,"/ROV_tracks"))

#This downloads available dive summary .txt files based on the dive name vector 
#above and prints an error if one is missing (UCH dives do not have dive summary 
#.txt files)
dive_summary_file_QAQC(dive_names)

#stop here and see if there are any missing dive summaries based on the output
#of the above code; update dive_names and dive_number if needed or else the code 
#below will be interrupted by a missing zip folder

dive_ancillary_file_extraction(dive_names)

#-------------------------------------------------------------------------------
#Apply the custom import functions across a group of dive summary .txt files 
#stored locally. Create a dataframe that contains dive number plus benthic start
#and end times for the group of dives. If/else conditional
#accommodates for formatting changes made to dive summaries after 2020.

dive_summary_paths<-list.files(paste0(wd, "/dive_summaries"), 
                               pattern = "[.]txt$", full.names = TRUE)

if (annotation_clean$date_time[1] < "2018-01-01" & annotation_clean$date_time[1] > "2016-12-31") {
  benthic_start_list<-purrr::map(dive_summary_paths, 
                                 \(x) import_benthic_start_2017(x))
} else if (annotation_clean$date_time[1] > "2017-12-31" & annotation_clean$date_time[1] < "2020-01-01") {
  benthic_start_list<-purrr::map(dive_summary_paths, 
                                 \(x) import_benthic_start_pre2020(x))
} else if (annotation_clean$date_time[1] > "2020-01-01") {
  benthic_start_list<-purrr::map(dive_summary_paths, 
                                 \(x) import_benthic_start_post2020(x)) 
}

if (annotation_clean$date_time[1] < "2018-01-01" & annotation_clean$date_time[1] > "2016-12-31") {
  benthic_end_list<-purrr::map(dive_summary_paths, 
                               \(x) import_benthic_end_2017(x))
} else if (annotation_clean$date_time[1] > "2017-12-31" & annotation_clean$date_time[1] < "2020-01-01") {
  benthic_end_list<-purrr::map(dive_summary_paths, 
                               \(x) import_benthic_end_pre2020(x))
} else if (annotation_clean$date_time[1] > "2020-01-01") {
  benthic_end_list<-purrr::map(dive_summary_paths, 
                               \(x) import_benthic_end_post2020(x))
}


#Stop here and look at the output. Are there any warnings that the dataset
#contains dives that were not benthic? If so, update the dive number list above

#Code below collapses list to a vector, removes any warnings that may be
#present, and converts character datetime to POSIX datetime
benthic_start <- benthic_start_list |> 
  sapply(paste) |> 
  stringr::str_subset("Not a benthic dive", negate = TRUE) |> 
  as.POSIXlt(tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")

benthic_end <- benthic_end_list |> 
  sapply(paste) |> 
  stringr::str_subset("Not a benthic dive", negate = TRUE) |> 
  as.POSIXlt(tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")

benthic_times<-data.frame(dive_number,benthic_start,benthic_end)

#visual check - are all the dives present? Do the start/stop times make sense?
View(benthic_times)

#-------------------------------------------------------------------------------
#Joins the clean annotations dataframe to the benthic times dataframe 
#and then filters the annotations data to only include the benthic portion of
#each dive. This join also removes any dives with no corresponding dive summary
#file (e.g. test dives, UCH dives, mid-water-only dives).

benthic_join<-dplyr::inner_join(annotation_clean, benthic_times, 
                               dplyr::join_by("dive_number" == "dive_number"))

benthic_annotations<- benthic_join |> 
  dplyr::group_by(dive_number) |> 
  dplyr::filter(date_time>=benthic_start & date_time<=benthic_end) |> 
  dplyr::ungroup()
View(benthic_annotations)

#Abundance was recorded in the comment column in some ASPIRE expeditions.
#Extracting the comments containing numbers is the first step to extracting
#this information for those expeditions. This will get moved to a separate
#script specific to the abundance extraction task.
benthic_annotations_numeric <- benthic_annotations |> 
  dplyr::filter(grepl("\\d+", comment))
View(benthic_annotations_numeric)

dir.create(paste0(wd,"/exports/"))
write.csv(benthic_annotations, paste0(wd, "/exports/benthic_annotations_", 
                                      data_name, ".csv"), row.names = FALSE)
