#'
#' @title Create a \code{tmap} map layer from an \code{sf} dataset representing a model output
#'
#' @description Function to create a \code{tmap} map layer from an \code{sf} dataset representing a model output.
#'
#' @param obj - \code{sf} dataset
#' @param palette -
#'
#' @return A \code{tmap} shape/fill combination map layer with polygons colored using the palette
#' to represent values of the ROMS variable in obj.
#'
#' @details Creates a \code{tmap} shape/fill combination map layer with polygons colored using the palette
#' to represent values of the ROMS variable in obj. If not provided, the palette will default to the
#' palette associated with the basemap used to plot the layer.
#'
#' @import tmap
#'
#' @export
#'
mapLayer_CreateFromModelOutput<-function(obj,
                                         palette=NULL){
  layer<-tmap::tm_shape(obj) + tmap::tm_fill(col=names(obj)[4],palette=palette);
  return(layer);
}
