#Automating dive summary text file downloads
#Using EX2014 as a test case

dive_names <- c("DIVE01", "DIVE02", "DIVE03", "DIVE04", "DIVE05", "DIVE06", "DIVE07", "DIVE08", "DIVE09", "DIVE10", "DIVE11")
data_name_lower <- tolower(data_name)
dir.create(paste0(wd,"/dive_summaries/"))

for(i in dive_names){
  
url <- paste0("https://oer.hpc.msstate.edu/okeanos/",data_name_lower,"/", data_name_lower,"-",i,"-ancillary-data.zip")

download.file(url, destfile = paste0(wd,"/dive_summaries/",i,"-ancillary-data.zip"))

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

#test code to identify UCH dives (these do not have dive summaries), print a warning, and not interrupt the loop of a full-expedition download
UCH_url <- "https://oer.hpc.msstate.edu/okeanos/ex2103/ex2103-DIVE11-ancillary-data.zip"

suppressWarnings(UCH_test <- try(download.file(UCH_url, destfile = paste0(wd,"/dive_summaries/download_test/","DIVE11-ancillary-data.zip")), silent = TRUE))
if(inherits(UCH_test, "try-error")){
  print("No Dive summary file")
}


