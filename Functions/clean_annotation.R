#Takes a data frame created from a raw SeaTube annotation file, selects
#columns of interest, renames them, and filters for biological and geological
#annotations using the "taxonomy" column. Separates the expedition name from
#the dive number. Accommodates differences in dive naming conventions across
#ASPIRE expeditions.

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
           `Species`,`Component`) |> 
    mutate(across(`Dive Name`, \(x) str_replace(x, "-", "_"))) |>
    mutate(across(`Dive Name`, \(x) word(x,1))) |> 
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
           species = `Species`,
           component = `Component`) |> 
    mutate(dive_number = toupper(dive_number)) |> 
    mutate(dive_number = gsub("DIVE","",dive_number)) |> 
    mutate(dive_number = as.numeric(dive_number)) |> 
    filter(taxonomy %in% c("WoRMS","WoRDSS","CMECS", "Simplified CMECS"))
}