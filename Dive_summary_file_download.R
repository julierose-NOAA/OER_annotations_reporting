#Two functions to automate the download of dive summary .txt files. These files
#are used in Benthic_annotations_cleaning.R to identify the benthic start and
#stop times and extract the benthic annotations from the full annotation 
#SeaTube .csv files. These files are also used in Benthic_summary_statistics.R
#to generate ROV metrics.

#UCH dives have no dive summary .txt file. This function downloads all 
#available dive summary .txt files from the user-generated dive_name vector
#and prints a warning if one is missing
dive_summary_file_QAQC <- function(dive_names) {
for(i in dive_names){
  
url <- paste0("https://oer.hpc.msstate.edu/okeanos/",data_name_lower,"/", data_name_lower,"-",i,"-ancillary-data.zip")

suppressWarnings(UCH_test <- try(download.file(url, destfile = paste0(wd,"/dive_summaries/",i, "-ancillary-data.zip")), silent = TRUE))
if(inherits(UCH_test, "try-error")){
  print(paste0("No ", i," Dive summary file"))
}
}
}

#Once the QAQC is run and the dive_name vector is updated to only include
#dives for which summary .txt files are available, this function unzips the 
#folder and retains only the summary .txt file for each dive

dive_summary_file_extraction <- function(dive_names) {
for(i in dive_names){
zip_file_paths <- unzip(paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/",i,"-ancillary-data.zip"),
                        exdir = paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/"),
                        list = TRUE)

unzip(paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/",i,"-ancillary-data.zip"),
      exdir = paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/"),
      files = zip_file_paths[2,1])

file.rename(from = paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/",zip_file_paths[2,1]),
            to = paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/",i,".txt"))
            
unlink(x = paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/",i,"-ancillary-data.zip"), recursive = TRUE)
unlink(x = paste0("C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/",data_name,"/dive_summaries/",zip_file_paths[1,1]), recursive = TRUE)
}
}
