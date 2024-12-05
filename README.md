# OER biodiversity analysis
Uses locally stored SeaTube annotation .csv files and OER dive summary .txt files. Annotations can be groups of dives saved as single .csv files or a combined .csv file with multiple dives
Uses dive summary .txt files to filter for the benthic portion of each individual dive within the group

Imports groups of files, cleans files, generates summary statistics for each dive, and extracts relevant information for the analysis.
Exports cleaned benthic annotations and dive summary statistics as .csv files

Dive summary statistics include number of total biological annotations, annotations by taxonomic level, and bottom time. Post-2020 dives also include distance traveled by the ROV.

Calculates number of unique taxa at each taxonomic level for each dive and creates visualizations, to inform taxonomic distinctness analysis. Exports unique taxa visualizations as png files.

Calculates taxonomic distinctness across groups of dives

Generates and exports relevant visualizations as png files

Uses boxplot outlier detection to flag unusual values across a group of dives

Planned: other biodiversity metrics
