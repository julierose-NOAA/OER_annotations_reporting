#source functions
function_names <- list.files(path = "C:/Users/julie.rose/Documents/GitHub/OER_biodiversity/Functions/", 
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)

#set working directory
wd <- "C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX2104"
setwd(wd)

#set standard name to refer to your data
data_name <- "EX2104"

#create vector of dive numbers for your dataset
dive_number<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19) #this needs updating for each
#analysis with the corresponding dives; it would be nice to extract this from
#the clean_annotations data frame or from the dive summaries themselves

benthic_annotations<-readr::read_csv(paste0(wd, "/exports/benthic_annotations_", 
                       data_name, ".csv"), col_names = TRUE)
View(benthic_annotations)
#-------------------------------------------------------------------------------

#Overall summary statistics for substrate and biological annotations
substrate_annotations <- benthic_annotations |> 
  dplyr::filter(taxonomy %in% c("CMECS", "Simplified CMECS")) |> 
  dplyr::select("dive_number", "component") |> 
  dplyr::group_by(dive_number) |> 
  dplyr::summarize(geoform_or_substrate = sum(!is.na(component)))
View(substrate_annotations)

biological_annotations <- benthic_annotations |>
  dplyr::filter(biota == "Biota") |> 
  dplyr::select("dive_number","species","genus","family","order","class","phylum") |> 
  dplyr::group_by(dive_number) |>
  dplyr::summarize(across(phylum:species, \(x) sum(!is.na(x))))
View(biological_annotations)

#if necessary, select subset of benthic start and end times below
#this could use some work to automate - maybe try adding dive number to
#benthic_start and benthic_end and do a join instead of cbind so that I don't
#have to manually update this part of the code
bottom_time_hours <- difftime(benthic_end, benthic_start, units = "hours")
#this needs fixing because benthic_end and benthic_start are from the cleaning
#script

if (benthic_annotations$date_time[1] > "2020-01-01") {
  distance<-purrr::map(dive_summary_paths, 
                       \(x) import_distance_traveled_post2020(x))
  summary_statistics <- cbind(biological_annotations, bottom_time_hours, 
                         distance_traveled_m = unlist(distance))
} else {
  summary_statistics <- cbind(biological_annotations, bottom_time_hours)
}

summary_statistics<-dplyr::left_join(summary_stats, substrate_annotations, 
                                join_by("dive_number" == "dive_number"))

View(summary_statistics)
write.csv(summary_statistics, paste0(wd, "/exports/summary_stats_", data_name, 
                                ".csv"),row.names = FALSE)