#'
#' @title Create a \code{sf} dataframe representing a ROMS grid from a netcdf file
#'
#' @description Function to create a \code{sf} dataframe representing a ROMS grid from a netcdf file.
#'
#' @param netcdf - path to netcdf file with ROMS grid, or a ncdf4 object connected to a netcdf file
#' @param grid_type - "rho","psi","u","v" [only works now for "rho" grid type]
#' @param coord_type - "latlon","xy" [only works now for "latlon" type]
#' @param crs - coordinate reference for grid (latlon default is WGS84, xy default is sf::NA_crs_)
#' @param verbose - flag to print additional information
#'
#' @return A \code{sf} dataframe object representing the grid
#'
#' @details Creates an sf dataframe from a netcdf file representing a ROMS grid. 
#' 
#' The input \code{crs} can be anything that \code{\link[wtsGIS]{get_crs}} can convert to
#' an \pkg{sf}-style coordinate reference system object.
#'
#' @import ncdf4
#' @import tibble
#' @import wtsGIS
#' @import wtsUtilities
#'
#' @export
#'
netCDF_CreateROMSGrid<-function(netcdf,
                                 grid_type=c("rho","psi","u","v"),
                                 coord_type=c("latlon","xy"),
                                 crs=ifelse(tolower(coord_type[1])=="latlon",wtsGIS::get_crs("WGS84")[[1]],sf::NA_crs_),
                                 verbose=TRUE){
  if (class(netcdf)!="ncdf4"){
    if (verbose) message("netCDF_CreateGrid: opening netcdf file '",netcdf,"'");
    ncf<-netCDF_Open(netcdf);
    if (is.null(ncf)) return(NULL);
    on.exit(ncdf4::nc_close(ncf));
  } else {
    ncf<-netcdf;
  }

  grid_type  <-grid_type[1];
  coord_type<-coord_type[1];
  crs = wtsGIS::get_crs(crs);#--convert crs to sf-type crs object

  if (verbose) {
    message("netCDF_CreateGrid: Checking for valid grid and coord types")
    message("\tinput grid_type is ",grid_type);
    message("\tinput coord_type type is ",coord_type);
  }
  valid_grid_types<-c("rho","psi","u","v");
  if (!wtsUtilities::checkValidType(grid_type,valid_grid_types)) stop();
  valid_coord_types<-c("latlon","xy");
  if (!wtsUtilities::checkValidType(coord_type,valid_coord_types)) stop();

  if (tolower(coord_type)=="latlon"){
    #--coordinate type is geographic
      xn<-"lon_";
      yn<-"lat_";
  } else {
    #--coordinate type is cartesian (xy)
      xn<-"x_";
      yn<-"y_";
  }

  if (tolower(grid_type)=="rho"){
    #--grid type is rho
    #----will skip outermost rho points because these don't
    #----have corners defined
    xn<-paste0(xn,"psi");#--need coords at corners
    yn<-paste0(yn,"psi");#--need coords at corners
    lnc<-"lon_rho";      #--longitude for centers
    ltc<-"lat_rho";      #--latitude for centers
    n_eta<-ncf$dim$eta_rho$len;
    eta_bgn<-2;         #--eta for rho points starts at 2
    eta_end<-n_eta-1;   #--eta for rho points ends at n_eta-1
    n_eta<-n_eta-2;     #--new n_eta is n_eta-2
    n_xi<-ncf$dim$xi_rho$len;
    xi_bgn<-2;          #
    xi_end<-n_xi-1;     #
    n_xi<-n_xi-2;       #
  }

  vx<-ncdf4::ncvar_get(ncf,varid=xn);  #--value of x-coord at corner locations
  vy<-ncdf4::ncvar_get(ncf,varid=yn);  #--value of y-coord at corner locations
  ln<-ncdf4::ncvar_get(ncf,varid=lnc); #--value of lon-coord at center locations
  lt<-ncdf4::ncvar_get(ncf,varid=ltc); #--value of lat-coord at center locations

  k<-0;
  nt<-n_eta*n_xi;
  if (verbose) message("\tnumber of output cells will be ",nt);
  kid<-vector(mode="character",length=nt);#--cell ids
  kxi<-vector(mode="integer",  length=nt);#--cell xi_rho value
  ket<-vector(mode="integer",  length=nt);#--cell eta_rho value
  kpt<-vector(mode="list",     length=nt);#--vector of lists for point geometries
  kpl<-vector(mode="list",     length=nt);#--vector of lists for polygon geometries
  for (eta in eta_bgn:eta_end){
    for (xi in xi_bgn:xi_end){
      k<-k+1;
      xll<-vx[xi-1,eta-1];#--xcoord, lower left
      yll<-vy[xi-1,eta-1];#--ycoord, lower left
      xlr<-vx[xi  ,eta-1];#--xcoord, lower right
      ylr<-vy[xi  ,eta-1];#--ycoord, lower right
      xur<-vx[xi  ,eta  ];#--xcoord, upper right
      yur<-vy[xi  ,eta  ];#--ycoord, upper right
      xul<-vx[xi-1,eta  ];#--xcoord, upperleft
      yul<-vy[xi-1,eta  ];#--ycoord, upper left
      kid[k]<-paste0(xi,"_",eta);
      kxi[k]<-xi;
      ket[k]<-eta;
      kpt[[k]]<-sf::st_point(x=c(ln[xi,eta],lt[xi,eta]),dim="XY")
      kpl[[k]]<-sf::st_polygon(list(matrix(c(xll,yll,xlr,ylr,xur,yur,xul,yul,xll,yll),ncol=2,byrow=TRUE)), dim="XY");
    }
  }
  sfc_pts<-sf::st_sfc(kpt,crs=crs);#--create point sfc object, specify the coordinate reference system
  sfc_pls<-sf::st_sfc(kpl,crs=crs);#--create polygon sfc object, specify the coordinate reference system
  sf_dfr <-sf::st_sf(tibble::tibble(ID=kid,xi=kxi,eta=ket,polygons=sfc_pls,points=sfc_pts));
  return(sf_dfr);
}
