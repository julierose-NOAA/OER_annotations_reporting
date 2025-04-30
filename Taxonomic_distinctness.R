library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(vegan)
library(purrr)
library(forcats)
library(stringr)

#The inputs for this analysis are locally stored SeaTube annotation .csv files
#and dive summary .txt files. Each annotation needs to have its paired dive
#summary so that the benthic portion of the dive can be extracted. I think this
#could be easily adapted to select just the water column portion of the dive
#if that would be useful.

#source functions
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
#create vector of dive numbers for your dataset
dive_number<-unique(benthic_annotations$dive_number)
dive_number

#------------------------------------------------------------------------------
#BEFORE RUNNING THIS CODE, check the summary statistics and decide which dives
#to include in the analysis. 

dives<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)

#if necessary, filter the benthic_annotations data frame before continuing
#with taxonomic distinctness calculation
benthic_annotations <- benthic_annotations |> 
  dplyr::filter(dive_number %in% dives)


#------------------------------------------------------------------------------

#Pulls out just the dive number and taxonomy columns, counts the number of 
#unique values at each taxonomic level. Added new columns that normalize the
#counts for each taxonomic level by total number of taxa observed. This 
#normalization helps to put all dives on a similar scale which improves the
#visualization in the next step
annotations_taxonomy_count <- benthic_annotations |> 
  dplyr::filter(biota == "Biota") |> 
  dplyr::select("dive_number","species","genus","family","order","class","phylum") |> 
  dplyr::group_by(dive_number) |> 
  taxonomy_count() |> 
  dplyr::rowwise() |> 
  dplyr::mutate(total_taxa = sum(species+genus+family+order+class+phylum)) |> 
  dplyr::mutate(across(phylum:species, \(x) x/total_taxa, .names = "{.col}_norm"))

#rotates the normalized count dataframe and converts taxonomic level to a factor
#for easier plotting
annotations_taxonomy_forplot <- annotations_taxonomy_count |> 
  dplyr::select("dive_number","species_norm","genus_norm","family_norm","order_norm",
         "class_norm","phylum_norm") |> 
  tidyr::pivot_longer(phylum_norm:species_norm) |> 
  dplyr::rename(taxonomic_level = name,
         normalized_count_unique = value) |> 
  dplyr::mutate(taxonomic_level = as.factor(taxonomic_level),
         dive_number = as.factor(dive_number)) |> 
  dplyr::mutate(taxonomic_level = forcats::fct_relevel(taxonomic_level, 
                                       c("phylum_norm","class_norm",
                                         "order_norm","family_norm",
                                         "genus_norm","species_norm"))) #this
#step makes the plot in the next step look nicer

#plots the normalized count for each taxonomic level, with dives shown in 
#different colors, saves as .png
png(paste0(wd, "/exports/taxonomic_count_all_", data_name, ".png"))

ggplot(annotations_taxonomy_forplot, aes(x = taxonomic_level, 
                                         y = normalized_count_unique, 
                                         color = dive_number, 
                                         group = dive_number)) +
  geom_line(linewidth = 1.25) +
  labs(title = "Number of Unique Taxa at Each Taxonomic Level",
       subtitle = "Normalized across dives by total observed taxa",
       x = "Taxonomic Level", y = "Normalized Count of Unique Taxa",
       color = "Dive Number") +
  theme(axis.text.x = element_text(size = 12, angle = -30, hjust = 0)) +
  theme(axis.title = element_text(size = 14))

dev.off()

#plots the normalized count for each taxonomic level by individual dive
#depending on number of dives, try tweaking the ncol value in the facet_wrap to
#improve the visualization, saves as .png
png(paste0(wd, "/exports/taxonomic_count_facet_", data_name, ".png"))

ggplot(annotations_taxonomy_forplot, aes(x = taxonomic_level,
                                         y = normalized_count_unique,
                                         group = dive_number)) +
  geom_line(linewidth = 1.25) +
  theme(axis.text.x = element_text(size = 12, angle = -30, hjust = 0)) +
  facet_wrap(~ dive_number, ncol = 3) +
  labs(title = "Number of Unique Taxa at Each Taxonomic Level",
       subtitle = "Normalized across dives by total observed taxa",
       x = "Taxonomic Level", y = "Normalized Count of Unique Taxa") +
  theme(axis.title = element_text(size = 14)) +
  theme(plot.margin = unit(c(.3, 1.5, .3, .3), "cm"))

dev.off()

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
  dplyr::filter(biota == "Biota") |> 
  dplyr::select(family:phylum) |>  #modify this for different taxonomic level analysis
  tidyr::drop_na() |> 
  dplyr::distinct() |> 
  tibble::column_to_rownames(var = "family") |> #this too
  vegan::taxa2dist()

#add summary text
dive_taxa_pivot <- benthic_annotations |> 
  dplyr::filter(biota == "Biota") |> 
  dplyr::select(dive_number, phylum:family) |>  #modify this for different TL analysis
  dplyr::group_by(dive_number) |> 
  tidyr::drop_na() |> 
  dplyr::distinct() |>  
  dplyr::ungroup() |> 
  dplyr::mutate(seen = 1) |> 
  dplyr::select(dive_number, family, seen) |>  #this too
  tidyr::pivot_wider(names_from = dive_number, values_from = seen, values_fill = 0)
  
  
dive_taxa <- as.data.frame(t(dive_taxa_pivot[,-1])) 
colnames(dive_taxa) <- dive_taxa_pivot$family  #this too

td_list <- vegan::taxondive(dive_taxa, base_taxonomy)

#taxondive outputs to a list, need to convert to data frame but the vectors are
#unequal length. Would be great to go straight to a data frame but for now first
#converting to a matrix works ok. Don't need the expected values in the analysis
#so for now just removing the last column of the matrix as I convert that to a
#data frame. Ideally I'd like to make this code cleaner.
td_mat <- matrix(unlist(td_list), nrow = length(dives), byrow = FALSE) 
td_df<- as.data.frame(td_mat[,1:7])
colnames(td_df)<- c('Species','Delta','Delta_Star','Lambda_Plus','Delta_Plus',
                    'sd_Delta_Plus', 'SDelta_Plus')
#need dive number column for visualizations
td_df$dive_number <- dives #references vector created after annotation QAQC step


write.csv(td_df, paste0(wd, "/exports/taxonomic_distinctness_", data_name, 
                        ".csv"), row.names = FALSE)

#visualize results - number of unique taxa across dives, saves as .png
png(paste0(wd, "/exports/unique_taxa_", data_name, ".png"))

ggplot(data = td_df, aes(x = dive_number, y = Species)) +
  geom_col() +
  labs(title = "Number of Unique Taxa", x = "Dive Number", y = "Taxa Count") +
  scale_x_continuous(n.breaks = nrow(td_df)) +
  geom_text(label = td_df$Species, nudge_y = 2)

dev.off()

#visualize average taxonomic distinctness across dives
png(paste0(wd, "/exports/tax_dist_", data_name, ".png"))

ggplot(data = td_df, aes(x = dive_number, y = Delta_Plus)) +
  geom_col() +
  labs(title = "Average Taxonomic Distinctness (Delta Plus)", x = "Dive Number",
       y = "Delta Plus") +
  scale_x_continuous(n.breaks = nrow(td_df))

dev.off()

#identify unusually low or high values that may be of interest using boxplot
#outlier detection method
delta_plus_out <- boxplot.stats(td_df$Delta_Plus)$out
delta_plus_out_row <- which(td_df$Delta_Plus %in% c(delta_plus_out))

td_df <- td_df |> 
  mutate(DeltaPlus_outlier = if_else(dive_number %in% delta_plus_out_row, TRUE, FALSE))

#plot unusual values in taxonomic distinctness across dives, visual accessibility
#checked using Colorgorical, saves as .png
png(paste0(wd, "/exports/tax_dist_out_", data_name, ".png"))

ggplot(data = td_df, aes(x = dive_number, y = Delta_Plus, fill = DeltaPlus_outlier)) +
  geom_col() +
  labs(title = "Average Taxonomic Distinctness (Delta Plus)", x = "Dive Number",
       y = "Delta Plus", fill = "Outlier?") +
  scale_x_continuous(n.breaks = nrow(td_df)) +
  scale_fill_manual(values = c("#818181","#6ad5eb")) 

dev.off()
