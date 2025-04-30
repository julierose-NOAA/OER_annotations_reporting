#Tests for package availability and installs missing packages that are needed
#in order to run this code:
if(!require('purrr'))install.packages('purrr')
if(!require('dplyr'))install.packages('dplyr')
if(!require('tidyr'))install.packages('tidyr')
if(!require('readr'))install.packages('readr')
#-------------------------------------------------------------------------------
#source functions
#need to manually set the file path for the functions folder within your local repository
function_names <- list.files(path = "C:/Users/julie.rose/Documents/GitHub/OER_biodiversity/Functions/", 
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)

#set working directory
wd <- "C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX2104"
setwd(wd)

#set standard name to refer to your data
data_name <- "EX2104"

benthic_annotations<-readr::read_csv(paste0(wd, "/exports/benthic_annotations_", 
                       data_name, ".csv"), col_names = TRUE)

dive_number<-unique(benthic_annotations$dive_number)
dive_number #stop here and cross-reference with dive summary text files - remove
#text files that have no annotations from the folder or else the ROV_metrics 
#code below will fail

View(benthic_annotations)
#-------------------------------------------------------------------------------

#Datetime when the ROV initiated on bottom ops
benthic_start <- benthic_annotations |> 
  dplyr::select("dive_number", "benthic_start") |> 
  dplyr::distinct()

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

#percentage of annotations flagged for review
percent_flagged <- benthic_annotations |> 
  dplyr::group_by(dive_number) |> 
  dplyr::summarize(percent_flagged = sum(flagged_for_review)/dplyr::n()*100)

#count annotations by dive for major phyla of interest to OER
interesting_phyla_count <- benthic_annotations |> 
  dplyr::group_by(dive_number) |> 
  dplyr::summarize(Cnidaria = sum(phylum == "Cnidaria", na.rm = TRUE),
                   Echinodermata = sum(phylum == "Echinodermata", na.rm = TRUE),
                   Porifera = sum(phylum == "Porifera", na.rm = TRUE))
                  
  
Vertebrata <- benthic_annotations |>
  dplyr::group_by(dive_number) |> 
  dplyr::filter(phylum == "Chordata") |>   
  dplyr::filter(! class %in% c("Thaliacea","Ascidiacea", "Appendicularia", "Larvacea")) |>
  tidyr::drop_na(class) |> 
  dplyr::summarize(Vertebrata = dplyr::n())

#count number of biological annotations that are identified as animals but have
#no phylum-level identification
unidentified_animalia <- benthic_annotations |> 
  dplyr::group_by(dive_number) |> 
  dplyr::filter(biota == "Biota", is.na(phylum), kingdom == "Animalia") |> 
  dplyr::summarize(Unidentified_Biota = dplyr::n())
View(unidentified_animalia)

#compare relative contributions of observed phyla to counts of total biological
#annotations
phyla_frequency <- benthic_annotations |> 
  dplyr::filter(biota == "Biota") |>
  tidyr::drop_na(phylum) |> 
  dplyr::group_by(dive_number,phylum) |> 
  dplyr::summarize(count = dplyr::n()) |> 
  dplyr::left_join(y=biological_annotations, by = "dive_number") |> 
  dplyr::mutate(percent = count/phylum.y*100) |> 
  dplyr::select(dive_number, phylum = phylum.x, count, percent)

#creates a data frame with every phylum observed in the overall expedition for 
#each dive with value = 0 filled in - need this for the heatmap visual
phyla_frequency_percent_all <- phyla_frequency |> 
  dplyr::select(!count) |> 
  tidyr::pivot_wider(names_from = dive_number, values_from = c(percent), 
                     values_fill = 0) |> 
  tidyr::pivot_longer(!phylum, names_to = "dive_number", values_to = "percent")
  
#calculate time on bottom based on benthic start and benthic end columns from
#the benthic_annotations data frame
bottom_time_hours <- benthic_annotations |> 
  dplyr::group_by(dive_number) |> 
  dplyr::reframe(bottom_time_hours = difftime(benthic_end, benthic_start, 
                                        units = "hours")) |> 
  dplyr::distinct()

bottom_time_hours$bottom_time_hours <- as.numeric(bottom_time_hours$bottom_time_hours)

#calculate mean depth during ROV time on bottom
mean_benthic_depth <- benthic_annotations |> 
  dplyr::group_by(dive_number) |> 
  dplyr::summarize(mean_depth = mean(depth_m))

#If post-2020, extract ROV distance traveled from the dive summary .txt files
#and combine with ROV bottom time into new ROV_metrics data frame, if pre-2020
#just rename bottom_time_hours to ROV_metrics
dive_summary_paths<-list.files(paste0(wd, "/dive_summaries"), 
                               pattern = "[.]txt$", full.names = TRUE)

if (benthic_annotations$date_time[1] > "2020-01-01") {
  distance<-purrr::map(dive_summary_paths, 
                       \(x) import_distance_traveled_post2020(x))
  ROV_metrics <- cbind(bottom_time_hours, 
                              distance_traveled_m = unlist(distance))
} else {
  ROV_metrics <- bottom_time_hours
}

#Join counts of biological annotations by taxonomy, counts of interesting phyla,
#counts of substrate annotations, and ROV dive information based on dive number

summary_statistics <- list(benthic_start, mean_benthic_depth, biological_annotations, percent_flagged, unidentified_animalia, 
                           interesting_phyla_count, Vertebrata, substrate_annotations, 
                           ROV_metrics) |> 
  purrr::reduce(dplyr::left_join, by = "dive_number")

#replace NA with 0 across whole data frame
summary_statistics[is.na(summary_statistics)] = 0

#add expedition column to aid in future cross-expedition synthesis
summary_statistics <- summary_statistics |> 
  dplyr::mutate(expedition = data_name) |> 
  dplyr::relocate(expedition, .before = dive_number)

View(summary_statistics)
write.csv(summary_statistics, paste0(wd, "/exports/summary_statistics_", data_name, 
                                ".csv"),row.names = FALSE)

write.csv(phyla_frequency_percent_all, paste0(wd, "/exports/phyla_frequency_percent_all_", data_name, 
                                     ".csv"),row.names = FALSE)
