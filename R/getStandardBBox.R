#'
#' @title Create a bounding box to display a standard map area for ROMS model output
#'
#' @description This function returns a bounding box to display a standard map area for ROMS model output.
#'
#' @details These bounding boxes are slightly different
#' from those provided by \code{\link[wtsGIS]{getStandardBBBox}} and are optimized for
#' ROMS model output.
#'
#' Available areas are: "EBS", "Bering10k", "CGOA", "CGOA shelf"
#'
#' @param area - area to return the bounding box for ("EBS","Bering10k","CGOA","CGOA shelf")
#' @param type - bounding box type to return ("sf" or "sp" for \pkg{sf} or \pkg{sp} types, respectively)
#'
#' @return - bounding box in \pkg{sf} (vector) or \pkg{sp} (list) format.
#'
#' @importFrom wtsGIS getBBox
#'
#' @export
#'
getStandardBBox<-function(area=c("EBS","Bering10k","CGOA","CGOA shelf"),
                                 type="sf"){
  if (toupper(area[1])=="EBS")
    lst = list(bottomleft=list(lon=-179,lat=54),
               topright  =list(lon=-157,lat=62.5));
  if (toupper(area[1])==toupper("Bering10k"))
    lst = list(bottomleft=list(lon=156.86111,lat=45.07368),
               topright  =list(lon=214.98959,lat=69.54647));
  if (toupper(area[1])=="CGOA")
    lst = list(bottomleft=list(lon=-162.67109,lat=46.68325),
               topright  =list(lon=-132.13292,lat=64.15562));
  if (toupper(area[1])==toupper("CGOA shelf"))
    lst = list(bottomleft=list(lon=-162.75,lat=54.5),
               topright  =list(lon=-135.70,lat=62.2));

  if (type!="sp")  lst = wtsGIS::getBBox(lst);#--convert to sf type

  return(lst)
}
