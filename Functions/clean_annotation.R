#Takes a data frame created from a raw SeaTube annotation file, selects
#columns of interest, renames them, and filters for biological and geological
#annotations using the "taxonomy" column. Separates the expedition name from
#the dive number. Accommodates differences in dive naming conventions across
#ASPIRE expeditions. If/else is used because "Common.Count" column is only 
#present in some of the ASPIRE expeditions but may contain abundance
#information useful to future data users.

clean_annotation <- function(x) { 
  has_count <- c("EX1803","EX1811","EX1903L2","EX1905L2","EX1907","EX2104")
  no_count <- c("EX1711","EX2103","EX2107","EX2201","EX2205","EX2206")
  
  if(data_name %in% has_count) {
  x |> 
    dplyr::select(`Dive.Name`, `Start.Date`, `Annotation.ID`, `To.Be.Reviewed`,
                  `Comment`, `Common.Count`, `Creator.First.Name`, `Creator.Last.Name`,
                  `Creator.Email`, `Modifier.First.Name`, `Modifier.Last.Name`,
                  `Modifier.Email`,
                  `DEEPDISCOVERERNAV01_23975_Latitude`,
                  `DEEPDISCOVERERNAV01_23975_Longitude`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen.Concentration`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Temperature`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Depth`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Practical.Salinity`, 
                  `Biota`,`Taxonomy`, `Kingdom`, `Phylum`, `Class`, `Order`, `Family`,
                  `Genus`, `Species`,`Component`, `Substrate.Origin`, 
                  `Substrate.Class`, `Substrate.Subclass`, `Substrate.Group`,
                  `Substrate.Subgroup`, `Geoform.Origin`, `Geoform`, `Geoform.Type`) |> 
      dplyr::mutate(across(`Dive.Name`, \(x) stringr::str_replace(x, "-", "_"))) |>
      dplyr::mutate(across(`Dive.Name`, \(x) stringr::str_replace(x, ":", " "))) |> 
      dplyr::mutate(across(`Dive.Name`, \(x) stringr::word(x,1))) |> 
      tidyr::separate(`Dive.Name`, c("expedition","dive_number"), sep = "_") |>  
      dplyr::rename(date_time = `Start.Date`,
                    annotation_ID = `Annotation.ID`,
                    flagged_for_review = `To.Be.Reviewed`,
                    creator_first_name = `Creator.First.Name`,
                    creator_last_name = `Creator.Last.Name`,
                    creator_email = `Creator.Email`,
                    modifier_first_name = `Modifier.First.Name`,
                    modifier_last_name = `Modifier.Last.Name`,
                    modifier_email = `Modifier.Email`,
                    comment = `Comment`,
                    count = `Common.Count`,
                    latitude_deg = `DEEPDISCOVERERNAV01_23975_Latitude`,
                    longitude_deg = `DEEPDISCOVERERNAV01_23975_Longitude`,
                    oxygen_mgl = `SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen.Concentration`,
                    temp_degC = `SBECTD9PLUSDEEPDISCOVERER_23978_Temperature`,
                    depth_m = `SBECTD9PLUSDEEPDISCOVERER_23978_Depth`,
                    salinity_psu = `SBECTD9PLUSDEEPDISCOVERER_23978_Practical.Salinity`,
                    biota = `Biota`,
                    taxonomy = `Taxonomy`,
                    kingdom = `Kingdom`,
                    phylum = `Phylum`,
                    class = `Class`,
                    order = `Order`,
                    family = `Family`,
                    genus = `Genus`,
                    species = `Species`,
                    component = `Component`,
                    substrate_origin = `Substrate.Origin`, 
                    substrate_class = `Substrate.Class`, 
                    substrate_subclass =`Substrate.Subclass`,
                    substrate_group = `Substrate.Group`,
                    substrate_subgroup = `Substrate.Subgroup`,
                    geoform_origin = `Geoform.Origin`,
                    geoform = `Geoform`,
                    geoform_type = `Geoform.Type`) |>
      dplyr::mutate(dive_number = toupper(dive_number)) |>
      dplyr::mutate(dive_number = gsub("DIVE","",dive_number)) |>
      dplyr::mutate(dive_number = as.numeric(dive_number)) |>
      dplyr::mutate(date_time = strptime(date_time, tz = "UTC", format = "%Y%m%dT%H%M%OSZ")) |> 
      dplyr::filter(taxonomy %in% c("WoRMS","WoRDSS","CMECS", "Simplified CMECS"))
} else if (data_name %in% no_count) {
  x |> 
    dplyr::select(`Dive.Name`, `Start.Date`, `Annotation.ID`, `To.Be.Reviewed`,
                  `Comment`, `Creator.First.Name`, `Creator.Last.Name`,
                  `Creator.Email`, `Modifier.First.Name`, `Modifier.Last.Name`,
                  `Modifier.Email`,
                  `DEEPDISCOVERERNAV01_23975_Latitude`,
                  `DEEPDISCOVERERNAV01_23975_Longitude`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen.Concentration`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Temperature`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Depth`,
                  `SBECTD9PLUSDEEPDISCOVERER_23978_Practical.Salinity`, 
                  `Biota`,`Taxonomy`, `Kingdom`, `Phylum`, `Class`, `Order`, `Family`,
                  `Genus`, `Species`,`Component`, `Substrate.Origin`, 
                  `Substrate.Class`, `Substrate.Subclass`, `Substrate.Group`,
                  `Substrate.Subgroup`, `Geoform.Origin`, `Geoform`, `Geoform.Type`) |> 
    dplyr::mutate(across(`Dive.Name`, \(x) stringr::str_replace(x, "-", "_"))) |>
    dplyr::mutate(across(`Dive.Name`, \(x) stringr::str_replace(x, ":", " "))) |> 
    dplyr::mutate(across(`Dive.Name`, \(x) stringr::word(x,1))) |> 
    tidyr::separate(`Dive.Name`, c("expedition","dive_number"), sep = "_") |>  
    dplyr::rename(date_time = `Start.Date`,
                  annotation_ID = `Annotation.ID`,
                  flagged_for_review = `To.Be.Reviewed`,
                  creator_first_name = `Creator.First.Name`,
                  creator_last_name = `Creator.Last.Name`,
                  creator_email = `Creator.Email`,
                  modifier_first_name = `Modifier.First.Name`,
                  modifier_last_name = `Modifier.Last.Name`,
                  modifier_email = `Modifier.Email`,
                  comment = `Comment`,
                  latitude_deg = `DEEPDISCOVERERNAV01_23975_Latitude`,
                  longitude_deg = `DEEPDISCOVERERNAV01_23975_Longitude`,
                  oxygen_mgl = `SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen.Concentration`,
                  temp_degC = `SBECTD9PLUSDEEPDISCOVERER_23978_Temperature`,
                  depth_m = `SBECTD9PLUSDEEPDISCOVERER_23978_Depth`,
                  salinity_psu = `SBECTD9PLUSDEEPDISCOVERER_23978_Practical.Salinity`,
                  biota = `Biota`,
                  taxonomy = `Taxonomy`,
                  kingdom = `Kingdom`,
                  phylum = `Phylum`,
                  class = `Class`,
                  order = `Order`,
                  family = `Family`,
                  genus = `Genus`,
                  species = `Species`,
                  component = `Component`,
                  substrate_origin = `Substrate.Origin`, 
                  substrate_class = `Substrate.Class`, 
                  substrate_subclass =`Substrate.Subclass`,
                  substrate_group = `Substrate.Group`,
                  substrate_subgroup = `Substrate.Subgroup`,
                  geoform_origin = `Geoform.Origin`,
                  geoform = `Geoform`,
                  geoform_type = `Geoform.Type`) |>
    dplyr::mutate(dive_number = toupper(dive_number)) |>
    dplyr::mutate(dive_number = gsub("DIVE","",dive_number)) |>
    dplyr::mutate(dive_number = as.numeric(dive_number)) |>
    dplyr::mutate(date_time = strptime(date_time, tz = "UTC", format = "%Y%m%dT%H%M%OSZ")) |>
    dplyr::filter(taxonomy %in% c("WoRMS","WoRDSS","CMECS", "Simplified CMECS"))  
} else {
  print("Add data_name to has_count or no_count vectors in clean_annotation function")
}
}