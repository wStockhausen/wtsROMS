#'
#' @title Create ROMS grid layers for maps based on the \pkg{ggplot2} package
#'
#' @description This function creates a ROMS grid layers for maps based on the \pkg{ggplot2} package.
#'
#' @details The returned list contains land and bathymetry layers (polygons), a fill scale for the bathymetry layer,
#' a map scale, and a partial theme.
#'
#' @param grid - a ROMS grid from call to \code{\link{getGrid}}
#' @param final.crs - representation of final (display) coordinate reference system (see details)
#' @param bbox - a bounding box (see details)
#' @param colors.bg - background color
#' @param colors.land - color for land
#' @param scale.bathym - \pkg{ggplot2} scale_fill_ object to use for the bathymetry fill scale
#' @param alpha.bathym - transparency for the bathymetry
#'
#' @return - a list with elements based on the \pkg{ggplot2} package:
#' \itemize{
#' \item{land - geom_sf layer for land-based grid polygons}
#' \item{bathym - geom_sf layer for bathymetry-based grid polygons}
#' \item{fill_scale - scale_fill_ object for bathymetry}
#' \item{map_scale - ggplot2 coord_ object}
#' \item{theme - partial ggplot2 theme with axis titles removed}
#' }
#'
#' @details The final coordinate reference system (\code{final.crs}) can be any object that
#' can be converted to a \code{sf::crs} object using \code{\link{get_crs}}.
#'
#' The bounding box (\code{bbox}) can be any object that can be converted
#' to a \code{sf::bbox} using \code{\link{getBBox}}.
#'
#' @import ggplot2
#' @import magrittr
#' @import sf
#' @import wtsGIS
#'
#' @export
#'
gg_CreateROMSGridLayers = function(grid,
                                   bathym.scale=c(seq(0,200,25),seq(250,500,50),seq(1000,6000,1000)),
                                   final.crs=wtsGIS::get_crs("WGS84"),
                                   bbox=wtsROMS::getStandardBBox("EBS"),
                                   colors.bg="white",
                                   colors.land="grey85",
                                   scale.bathym=ggplot2::scale_fill_viridis_d(option="plasma",direction=-1),
                                   alpha.bathym=1.0){
  #--convert to final crs
  grid %<>% sf::st_transform(final.crs);
  bbox %<>% wtsGIS::getBBox() %>% wtsGIS::transformBBox(final.crs);

  #--crop grid and create grid layers
  grid %<>% sf::st_crop(bbox);
  land = ggplot2::geom_sf(data=grid %>% subset(is.na(Z)),  fill=colors.land, colour=NA);
  watr = grid %>% subset(!is.na(Z));
  if (!is.null(bathym.scale)){
    nc = length(bathym.scale);
    labels = bathym.scale[2:nc];
    labels[1] = paste0(bathym.scale[1],"-",labels[1]);
    watr$Z = cut(watr$Z,breaks = bathym.scale, labels=labels);
  }
  bath = ggplot2::geom_sf(data=watr, mapping=ggplot2::aes(fill=Z), colour=NA, alpha=alpha.bathym);

  #--define coordinate scale for default basemap
  map_scale = ggplot2::coord_sf(xlim=c(bbox["xmin"],bbox["xmax"]),
                                ylim=c(bbox["ymin"],bbox["ymax"]),
                                crs=final.crs,
                                expand=FALSE,
                                clip="on",
                                default=TRUE);

  #--define theme
  #----define aspect ratio for panels
  asp = NULL; #--let ggplot2 work it out
  if (!sf::st_is_longlat(final.crs)) asp = (bbox["ymax"]-bbox["ymin"])/(bbox["xmax"]-bbox["xmin"]);
  #----remove axis titles (necessary when connectivity zone labels are included in map)
  theme = ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         plot.background = ggplot2::element_rect(fill=colors.bg),
                         panel.spacing = grid::unit(0.05,"cm"),
                         aspect.ratio=asp);

  return(list(land=land,bathym=bath,fill_scale=scale.bathym,map_scale=map_scale,theme=theme));
}

