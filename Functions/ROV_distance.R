#Calculates 3D distance traveled between pairs of rows within the ROV track
#data frame and saves as new column

ROV_distance <- function(data, lat, long){
  data |> 
    dplyr::mutate(Haversine = c(geosphere::distHaversine(cbind({{long}},{{lat}})),0),
                  depth_distance_m = c(diff(altitude_m),0),
                  distance_3D_m = sqrt((depth_distance_m^2) + (Haversine^2)))
  
}
