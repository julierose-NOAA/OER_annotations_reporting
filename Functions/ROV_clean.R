#take raw ROV dive track data, selects relevant columns, creates a UTC datetime
#column to pair with the benthic_times data frame, and adds expedition and 
#dive number columns

ROV_clean <- function(x){
  if(expedition == "EX1711") {
  x |> 
    dplyr::mutate(expedition = expedition) |> 
    dplyr::select(expedition, dive_number, time..unix.sec.,depth..m.,alt..m.,lat..dec..deg..,lon..dec..deg..) |> 
    dplyr::rename(unix_time = time..unix.sec., depth_m = depth..m., altitude_m = alt..m.,
                  latitude_dd = lat..dec..deg.., longitude_dd = lon..dec..deg..) |>
    dplyr::mutate(UTC = lubridate::as_datetime(unix_time)) |> 
    dplyr::mutate(across(dive_number, \(x) stringr::str_extract(x, "DIVE(\\d{2})"))) |> 
    dplyr::mutate(across(dive_number, \(x) stringr::str_replace(x, "DIVE",""))) |> 
    dplyr::mutate(dive_number = as.numeric(dive_number))
  } else {
    x |> 
      dplyr::mutate(expedition = expedition) |> 
      dplyr::select(expedition, dive_number, "UNIXTIME","DEPTH","ALT","LAT_DD","LON_DD") |> 
      dplyr::rename(unix_time = "UNIXTIME", depth_m = "DEPTH", altitude_m = "ALT",
                    latitude_dd = "LAT_DD", longitude_dd = "LON_DD") |>
      dplyr::mutate(UTC = lubridate::as_datetime(unix_time)) |> 
      dplyr::mutate(across(dive_number, \(x) stringr::str_extract(x, "DIVE(\\d{2})"))) |> 
      dplyr::mutate(across(dive_number, \(x) stringr::str_replace(x, "DIVE",""))) |> 
      dplyr::mutate(dive_number = as.numeric(dive_number))
  }
}