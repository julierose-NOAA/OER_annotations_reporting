ROV_dive_number_extract <- function(x) {
  name <- stringr::str_extract(x, "DIVE(\\d{2})") 
  stringr::str_replace(name, "DIVE","")
}