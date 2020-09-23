#'
#' @title Create a \code{tmap} map layer from an \code{sf} dataset representing ROMS model output
#'
#' @description Function to create a \code{tmap} map layer from an \code{sf} dataset representing ROMS model output.
#'
#' @param obj - an \code{sf} polygon dataset.
#' @param col - name or index of values column to plot
#' @param n	- preferred number of classes.
#' @param style	- method to process the color scale.
#' @param breaks	- in case style=="fixed", breaks should be specified. The breaks argument can also be used when style="cont". In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param palette - a palette name, a vector of colors, or NULL. See tmaptools::palette_explorer() for the named palettes. Use a "-" as prefix to reverse the palette. The default palette is taken from the basemap (tm_layout's argument aes.palette).
#' @param basemap - basemap for map, or name associated with a pre-defined basemap (see \code{createBasemap}).
#' @param aes.palette - a list (with elements 'cat', 'div', 'seq') of palette names or a vector of colors for the basemap.
#' @param showMap - flag to print map
#'
#' @return A \code{tmap} map (bsaemap + shape/fill combination map layer) with polygons colored using the palette
#' to represent values of the ROMS variable in obj.
#'
#' @details Creates a \code{tmap} map (shape/fill combination map layer) with polygons colored using the palette
#' to represent values of the ROMS variable in obj. If not provided, the palette will default to the
#' palette associated with the basemap used to plot the layer.
#'
#' Discrete options for \code{style} are "fixed", "sd", "equal", "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", and "log10_pretty".
#' For discrete options (except "log10_pretty"), see the details in \code{tmap::classIntervals}.
#'
#' Continuous options for \code{style} are "cont", "order", and "log10". The first maps the values of \code{col}
#' to a smooth gradient, the second maps the order of values of col to a smooth gradient,
#' and the third uses a logarithmic transformation.
#'
#' @import tmap
#' @import wtsGIS
#'
#' @export
#'
mapLayer_Plot<-function(obj,
                        col=4,
                        n=10,
                        style=ifelse(is.null(breaks), "pretty", "fixed"),
                        breaks=NULL,
                        palette=c("blue","red"),
                        basemap=NULL,
                        aes.palette=list(cat = c("blue", "red"), div = c("blue", "red"), seq = c("blue","red")),
                        showMap=TRUE){
  if (is.character(basemap)) {
    message("mapLayer_Plot: creating basemap")
    bbox<-wtsGIS::getStandardBoundingBox(basemap);
    basemap<-wtsROMS::createBasemap(boundingbox=bbox,aes.palette=aes.palette)
    message("mapLayer_Plot: created basemap")
  }
  if (is.null(basemap)){
    msg<-"mapLayer_Plot: must supply a basemap or valid name for one."
    stop(msg);
  }
  if (inherits(obj,"tmap")) {
    layer<-obj;
  } else {
    message("mapLayer_Plot: creating map layer")
    layer<-mapLayer_CreateFromModelOutput(obj,col=col,n=n,style=style,breaks=breaks,palette=palette);
    message("mapLayer_Plot: created map layer")
  }
  map <- basemap + layer;
  if (showMap) {message("mapLayer_Plot: --printing map"); print(map); message("--finished printing map");}

  return(map);
}
