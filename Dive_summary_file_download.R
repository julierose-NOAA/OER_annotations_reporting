#Automating dive summary text file downloads, using EX2107 as a test case because
#there is a UCH dive in the middle of the expedition

wd <- "C:/Users/julie.rose/Documents/1-OER/Biodiversity/expeditions/EX2107"
data_name <- "EX2107"
dive_names <- c("DIVE03", "DIVE04", "DIVE05", "DIVE06", "DIVE07", "DIVE08", "DIVE09", "DIVE10", "DIVE11", "DIVE12", "DIVE13", "DIVE14")
data_name_lower <- tolower(data_name)
dir.create(paste0(wd,"/dive_summaries/"))

for(i in dive_names){
  
url <- paste0("https://oer.hpc.msstate.edu/okeanos/",data_name_lower,"/", data_name_lower,"-",i,"-ancillary-data.zip")

suppressWarnings(UCH_test <- try(download.file(url, destfile = paste0(wd,"/dive_summaries/",i, "-ancillary-data.zip")), silent = TRUE))
if(inherits(UCH_test, "try-error")){
  print(paste0("No ", i," Dive summary file"))
}
}

#stop here and see if there are any missing dive summaries based on the output
#of the above code; update dive_names if needed or else the code below will be
#interrupted by a missing zip folder

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


