#'
#' @title Create a \code{tmap} map layer from an \code{sf} dataset representing a model output
#'
#' @description Function to create a \code{tmap} map layer from an \code{sf} dataset representing a model output.
#'
#' @param obj - an \code{sf} polygon dataset.
#' @param basemap - basemap for map, or name associated with a pre-defined basemap (see \code{createBasemap}).
#' @param n	- preferred number of classes.
#' @param style	- method to process the color scale. Discrete options are "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", and "log10_pretty". For discrete options (except "log10_pretty"), see the details in tmap::classIntervals. Continuous options are "cont", "order", and "log10". The first maps the values of col to a smooth gradient, the second maps the order of values of col to a smooth gradient, and the third uses a logarithmic transformation.
#' @param breaks	- in case style=="fixed", breaks should be specified. The breaks argument can also be used when style="cont". In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param palette - a palette name or a vector of colors. See tmaptools::palette_explorer() for the named palettes. Use a "-" as prefix to reverse the palette. The default palette is taken from tm_layout's argument aes.palette.
#'
#' @return A \code{tmap} shape/fill combination map layer with polygons colored using the palette
#' to represent values of the ROMS variable in obj.
#'
#' @details Creates a \code{tmap} shape/fill combination map layer with polygons colored using the palette
#' to represent values of the ROMS variable in obj. If not provided, the palette will default to the
#' palette associated with the basemap used to plot the layer.
#'
#' @import tmap
#' @import wtsGIS
#'
#' @export
#'
mapLayer_Plot<-function(obj,
                        basemap,
                        n=10,
                        style=ifelse(is.null(breaks), "pretty", "fixed"),
                        breaks=NULL,
                        palette=list(cat = c("blue", "red"), div = c("blue", "red"), seq = c("blue","red"))){
  if (is.character(basemap)) {
    bbox<-wtsGIS::getStandardBoundingBox(basemap);
    basemap<-createBasemap(boundingbox=bbox,aes.palette=palette)
  }
  layer<-tmap::tm_shape(obj) + tmap::tm_fill(col=names(obj)[4]);
  map <- basemap + layer;
  print(map);
  return(invisible(map));
}
