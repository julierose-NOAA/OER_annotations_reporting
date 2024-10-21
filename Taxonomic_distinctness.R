library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(vegan)
library(purrr)
library(forcats)

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

#------------------------------------------------------------------------------

#function to calculate number of unique taxa at each taxonomic level for an
#individual dive

taxonomy_count <- function(x) { 
  x |> 
    summarize(
      across(phylum:species, \(x) n_distinct(x, na.rm = TRUE))
    )
}

#Pulls out just the dive number and taxonomy columns, counts the number of 
#unique values at each taxonomic level. Added new columns that normalize the
#counts for each taxonomic level by total number of taxa observed. This 
#normalization helps to put all dives on a similar scale which improves the
#visualization in the next step
annotations_taxonomy_count <- benthic_annotations |> 
  select("dive_number","species","genus","family","order","class","phylum") |> 
  group_by(dive_number) |> 
  taxonomy_count() |> 
  rowwise() |> 
  mutate(total_taxa = sum(species+genus+family+order+class+phylum)) |> 
  mutate(across(phylum:species, \(x) x/total_taxa, .names = "{.col}_norm"))

#rotates the normalized count dataframe and converts taxonomic level to a factor
#for easier plotting
annotations_taxonomy_forplot <- annotations_taxonomy_count |> 
  select("dive_number","species_norm","genus_norm","family_norm","order_norm",
         "class_norm","phylum_norm") |> 
  pivot_longer(phylum_norm:species_norm) |> 
  rename(taxonomic_level = name,
         normalized_count_unique = value) |> 
  mutate(taxonomic_level = as.factor(taxonomic_level),
         dive_number = as.factor(dive_number)) |> 
  mutate(taxonomic_level = fct_relevel(taxonomic_level, 
                                       c("phylum_norm","class_norm",
                                         "order_norm","family_norm",
                                         "genus_norm","species_norm"))) #this
#step makes the plot in the next step look nicer

#plots the normalized count for each taxonomic level, with dives shown in 
#different colors
ggplot(annotations_taxonomy_forplot, aes(x = taxonomic_level, 
                                         y = normalized_count_unique, 
                                         color = dive_number, 
                                         group = dive_number)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = -30, hjust = 0))
#the plot above could use some work - the individual dives are a little hard to
#distinguish

#plots the normalized count for each taxonomic level by individual dive
#depending on number of dives, try tweaking the ncol value in the facet_wrap to
#improve the visualization
ggplot(annotations_taxonomy_forplot, aes(x = taxonomic_level,
                                         y = normalized_count_unique,
                                         group = dive_number)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
  facet_wrap(~ dive_number, ncol = 3)

#-------------------------------------------------------------------------------

#The analysis of taxonomic distinctness in the vegan package needs two input
#files: 
#1) a data file with abundance (can be presence/absence) of each taxa,
#organized where individual dives are rows and taxa are columns; 
#2) a base taxonomy input file that shows how all of the taxa in the data file 
#are related to each other. Vegan provides the function taxa2dist to create
#this input from a data frame, organized where row names correspond to names of 
#individual taxa, and columns correspond to taxonomic level, from lowest to
#highest. 
#The taxondive function is set up to process multiple samples/dives at once, 
#so this code should not need iteration.

#this takes the combined clean annotations file and creates the base taxonomy 
#input to use with taxa2dist. It is currently set to include family through 
#phylum in the analysis, and can easily be modified based on the user-selected
#taxonomic levels from the data visualizations in the code above. It extracts
#the unique values across all dives and then converts the lowest taxonomic level
#to rownames, which is required for the taxa2dist function. Then it runs
#taxa2dist to create the input file for vegan (#2 above).

base_taxonomy <- benthic_annotations |> 
    select(family:phylum) |>  #modify this for different taxonomic level analysis
    drop_na() |> 
    distinct() |> 
    tibble::column_to_rownames(var = "family") |> #this too
    taxa2dist()

#add summary text
dive_taxa_pivot <- benthic_annotations |> 
  select(dive_number, phylum:family) |>  #modify this for different TL analysis
  group_by(dive_number) |> 
  drop_na() |> 
  distinct() |>  
  ungroup() |> 
  mutate(seen = 1) |> 
  select(dive_number, family, seen) |>  #this too
  pivot_wider(names_from = dive_number, values_from = seen, values_fill = 0)
  
  
dive_taxa <- as.data.frame(t(dive_taxa_pivot[,-1])) 
colnames(dive_taxa) <- dive_taxa_pivot$family  #this too

td_list <- taxondive(dive_taxa, base_taxonomy)

#taxondive outputs to a list, need to convert to data frame but the vectors are
#unequal length. Would be great to go straight to a data frame but for now first
#converting to a matrix works ok. Don't need the expected values in the analysis
#so for now just removing the last column of the matrix as I convert that to a
#data frame. Ideally I'd like to make this code cleaner.
td_mat <- matrix(unlist(td_list), nrow = 19, byrow = FALSE) #note nrow will vary based on dive number
td_df<- as.data.frame(td_mat[,1:7])
colnames(td_df)<- c('Species','Delta','Delta*','Lambda+','Delta+','sd_Delta+', 
                    'SDelta+')
td_df$dive_number <- 1:nrow(td_df) #assumes sequential dive numbering

write.csv(td_df,"C:/Users/julie.rose/Documents/1-OER/Biodiversity/taxonomic_distinctness.csv")

#visualize results
ggplot(data = td_df, aes(x = dive_number, y = Species)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Unique Taxa", x = "Dive Number", y = "Taxa Count") +
  scale_x_continuous(n.breaks = nrow(td_df))