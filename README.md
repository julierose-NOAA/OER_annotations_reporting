# OER biodiversity analysis
Uses locally stored SeaTube annotation .csv files and OER dive summary .txt files
Annotations can be groups of dives saved as single .csv files or a combined .csv file with multiple dives

Imports groups of files, cleans files and extracts only the relevant information for the analysis.
Exports cleaned benthic annotations as .csv file

Uses dive summaries to filter for the benthic portion of each individual dive within the group

Calculates number of unique taxa at each taxonomic level for each dive and creates visualizations, to inform taxonomic distinctness analysis
Exports unique taxa visualizations as png files

Calculates taxonomic distinctness across groups of dives

Generates and exports relevant visualizations as png files

Uses boxplot outlier detection to flag unusual values across a group of dives

Planned: other biodiversity metrics
