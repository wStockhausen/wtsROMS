#'
#' @title Create a bounding box to display a standard map area  for ROMS model output
#'
#' @description This function returns a bounding box to display a standard map area for ROMS model output.
#'
#' @details The bounding box is returned in list format, with coordinates defining the
#' bottomleft and topright boundaries for the area. These bounding boxes are slightly different
#' from those provided by \code{\link{[wtsGIS]getStandardBoundingBox}} and are optimized for
#' ROMS model output.
#'
#' Available areas are: "EBS", "Bering10k", "CGOA", "CGOA shelf"
#'
#' @param area - area to return the bounding box for ("EBS","Bering10k","CGOA","CGOA shelf")
#'
#' @return - bounding box in list format
#'
#' @export
#'
getStandardBoundingBox<-function(area=c("EBS","Bering10k","CGOA","CGOA shelf")){
  if (toupper(area[1])=="EBS")
    return(list(bottomleft=list(lon=-179,lat=54),
                topright  =list(lon=-157,lat=62.5)));
  if (toupper(area[1])==toupper("Bering10k"))
    return(list(bottomleft=list(lon=156.86111,lat=45.07368),
                topright  =list(lon=214.98959,lat=69.54647)));
  if (toupper(area[1])=="CGOA")
    return(list(bottomleft=list(lon=-162.67109,lat=46.68325),
                topright  =list(lon=-132.13292,lat=64.15562)));
  if (toupper(area[1])==toupper("CGOA shelf"))
    return(list(bottomleft=list(lon=-162.75,lat=54.5),
                topright  =list(lon=-135.70,lat=62.2)));
}
