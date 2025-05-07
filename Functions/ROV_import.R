#imports ROV track .csv file and uses file name to extract dive number

ROV_import <- function(filename){
  ROV <- read.csv(filename, header = TRUE)
  ROV$dive_number <- filename
  ROV
}