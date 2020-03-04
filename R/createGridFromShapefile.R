#'
#' @title Create a \code{sf} dataframe representing a ROMS grid from a shapefile
#'
#' @description Function to create a \code{sf} dataframe representing a ROMS grid from a shapefile.
#'
#' @param shapefile - path to shapefile derived from ROMS grid.
#' @param strCRS - coordinate reference for sahpefile
#'
#' @return A \code{sf} dataframe object representing the grid
#'
#' @details Uses \code{wtsGIS::createLayerFromShapefile} to create an sf dataframe from
#' a shapefile derived from a ROMS grid.
#'
#' @export
#'
createGridFromShapefile<-function(shapefile,
                                  strCRS = wtsGIS::getCRS("AlaskaAlbers")){
  roms_grid<-wtsGIS::createLayerFromShapefile(shapefile,
                                              strCRS = strCRS,
                                              as.sf = TRUE);
  return(roms_grid);
}