#'
#' @title Create a \pkg{tmap}-style map layer from a \code{sf} dataframe representing a model output
#'
#' @description Function to create a \pkg{tmap}-style map layer from an \code{sf} dataframe representing a model output.
#'
#' @param sf_dfr - \code{sf} dataset
#' @param col - name or index of values column to plot
#' @param n	- preferred number of classes.
#' @param style	- method to process the color scale. Discrete options are "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", and "log10_pretty". For discrete options (except "log10_pretty"), see the details in tmap::classIntervals. Continuous options are "cont", "order", and "log10". The first maps the values of col to a smooth gradient, the second maps the order of values of col to a smooth gradient, and the third uses a logarithmic transformation.
#' @param breaks	- in case style=="fixed", breaks should be specified. The breaks argument can also be used when style="cont". In that case, the breaks are mapped evenly to the sequential or diverging color palette.
#' @param palette - a palette name or a vector of colors. See tmaptools::palette_explorer() for the named palettes. Use a "-" as prefix to reverse the palette. The default palette is taken from tm_layout's argument aes.palette.
#'
#' @return A \pkg{tmap}-style shape/fill combination map layer with polygons colored using the palette
#' to represent values of the ROMS variable in \code{sf_dfr}.
#'
#' @details Creates a \pkg{tmap}-style shape/fill combination map layer with polygons colored using the palette
#' to represent values of the ROMS variable in \code{sf_dfr}. If not provided, the palette will default to the
#' palette associated with the basemap used to plot the layer.
#'
#' @import tmap
#'
#' @export
#'
tmap_CreateLayer<-function(sf_dfr,
                            col=4,
                            n=10,
                            style=ifelse(is.null(breaks), "pretty", "fixed"),
                            breaks=NULL,
                            palette=c("blue","red")){
  if (is.numeric(col)) {
    col<-names(sf_dfr)[col];
    message(paste0("mapLayer_CreateFromModelOutput: creating map layer for ",col));
  }
  layer <- tmap::tm_shape(sf_dfr) +
             tmap::tm_fill(col=col,n=n,style=style,breaks=breaks,palette=palette);
  return(layer);
}
