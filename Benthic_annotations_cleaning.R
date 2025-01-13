#source functions
function_names <- list.files(path = "C:/Users/julie.rose/Documents/GitHub/OER_biodiversity/Functions/", 
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)

#set working directory
wd <- "C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX2107"
setwd(wd)

#set standard name to refer to your data
data_name <- "EX2107"

#create vector of dive numbers for your dataset. The dive landing pages are a 
#good place to find the dive numbers to start with
#https://www.ncei.noaa.gov/waf/okeanos-rov-cruises/
dive_number<-c(1,3,4,5,6,7,8,9,10,11,12,13,14) 

#create a vector of character descriptors of dives, which will be used to 
#download the dive summary text files
dive_names <- c("DIVE01", "DIVE03", "DIVE04", "DIVE05", "DIVE06", "DIVE07", "DIVE08", "DIVE09", "DIVE10", "DIVE11", "DIVE12", "DIVE13", "DIVE14")

#------------------------------------------------------------------------------

#Read in SeaTube .csv annotation file(s) and apply clean_annotation function.
#This can accommodate a single .csv containing all dives or a group of annotation 
#files saved locally as .csv exports from SeaTube. Uses the number of files
#in the annotation folder to determine which import process to execute.

annotation_paths<-list.files(paste0(wd, "/annotations"), 
                             pattern = "[.]csv$", full.names = TRUE)

if (length(annotation_paths > 1)) {
  annotation_list<-purrr::map(annotation_paths, 
                              \(x) readr::read_csv(x, col_names = TRUE, na = ""))
  
  annotation_clean<- annotation_list |> 
    purrr::map(clean_annotation) |> 
    purrr::list_rbind()
  
} else {
  annotation_import <- readr::read_csv(paste0(wd, "/annotations/SeaTubeAnnotations_", 
                                              data_name, ".csv"), col_names = TRUE, 
                                       na = "")
  annotation_clean <- clean_annotation(annotation_import)
}

View(annotation_clean)

#-------------------------------------------------------------------------------
#Download dive summary text files for use in extracting the benthic portion of
#the dive, save to new subdirectory within the existing expedition directory
data_name_lower <- tolower(data_name)
dir.create(paste0(wd,"/dive_summaries/"))

#This downloads available dive summary .txt files based on the dive name vector 
#above and prints an error if one is missing (UCH dives do not have dive summary 
#.txt files)
dive_summary_file_QAQC(dive_names)

#stop here and see if there are any missing dive summaries based on the output
#of the above code; update dive_names and dive_number if needed or else the code 
#below will be interrupted by a missing zip folder

dive_summary_file_extraction(dive_names)

#-------------------------------------------------------------------------------
#Apply the custom import functions across a group of dive summary .txt files 
#stored locally. Create a dataframe that contains dive number plus benthic start
#and end times for the group of dives. If/else conditional
#accommodates for formatting changes made to dive summaries after 2020.

dive_summary_paths<-list.files(paste0(wd, "/dive_summaries"), 
                               pattern = "[.]txt$", full.names = TRUE)

if (annotation_clean$date_time[1] < "2020-01-01") {
  benthic_start_list<-purrr::map(dive_summary_paths, 
                                 \(x) import_benthic_start_pre2020(x))
} else if (annotation_clean$date_time[1] > "2020-01-01") {
  benthic_start_list<-purrr::map(dive_summary_paths, 
                                 \(x) import_benthic_start_post2020(x))
}

if (annotation_clean$date_time[1] < "2020-01-01") {
  benthic_end_list<-purrr::map(dive_summary_paths, 
                               \(x) import_benthic_end_pre2020(x))
} else if (annotation_clean$date_time[1] > "2020-01-01") {
  benthic_end_list<-purrr::map(dive_summary_paths, 
                               \(x) import_benthic_end_post2020(x))
}

benthic_start<- as.POSIXct(unlist(benthic_start_list))
benthic_end<- as.POSIXct(unlist(benthic_end_list))

benthic_times<-data.frame(dive_number,benthic_start,benthic_end)

#-------------------------------------------------------------------------------
#Joins the clean annotations dataframe to the benthic times dataframe 
#and then filters the annotations data to only include the benthic portion of
#each dive. This join also removes any dives with no corresponding dive summary
#file (e.g. test dives, UCH dives).

benthic_join<-dplyr::left_join(annotation_clean, benthic_times, 
                               dplyr::join_by("dive_number" == "dive_number"))

benthic_annotations<- benthic_join |> 
  dplyr::group_by(dive_number) |> 
  dplyr::filter(date_time>=benthic_start & date_time<=benthic_end) |> 
  dplyr::ungroup()
View(benthic_annotations)

dir.create(paste0(wd,"/exports/"))
write.csv(benthic_annotations, paste0(wd, "/exports/benthic_annotations_", 
                                      data_name, ".csv"), row.names = FALSE)
