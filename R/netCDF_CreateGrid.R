#'
#' @title Create a \code{sf} dataframe representing a ROMS grid from a netcdf file
#'
#' @description Function to create a \code{sf} dataframe representing a ROMS grid from a netcdf file.
#'
#' @param netcdf - path to netcdf file with ROMS grid, or a ncdf4 object connected to a netcdf file
#' @param grid_type - "rho","psi","u","v" [only works now for "rho" grid type]
#' @param coord_type - "latlon","xy" [only works now for "latlon" type]
#' @param strCRS - coordinate reference for grid (latlon default is NAD83, xy default is sf::NA_crs_)
#' @param verbose - flag to print additional information
#'
#' @return A \code{sf} dataframe object representing the grid
#'
#' @details Creates an sf dataframe from a netcdf file representing a ROMS grid.
#' Masked cells (e.g., land) have a value of NA.
#'
#' @import ncdf4
#' @import wtsGIS
#' @import wtsUtilities
#'
#' @export
#'
netCDF_CreateGrid<-function(netcdf,
                             grid_type=c("rho","psi","u","v"),
                             coord_type=c("latlon","xy"),
                             strCRS = ifelse(tolower(coord_type[1])=="latlon",wtsGIS::get_crs("NAD83")[[1]],sf::NA_crs_),
                             verbose=TRUE){
  grid_type  <-grid_type[1];
  coord_type<-coord_type[1];
  strCRS    <-strCRS[1];

  if (verbose) {
    message("netCDF_CreateGrid: Checking for valid grid and coord types")
    message("\tinput grid_type is ",grid_type);
    message("\tinput coord_type type is ",coord_type);
  }
  valid_grid_types<-c("rho","psi","u","v");
  if (!wtsUtilities::checkValidType(grid_type,valid_grid_types)) stop();
  valid_coord_types<-c("latlon","xy");
  if (!wtsUtilities::checkValidType(coord_type,valid_coord_types)) stop();

  if (class(netcdf)!="ncdf4"){
    if (verbose) message("netCDF_CreateGrid: opening netcdf file '",netcdf,"'");
    ncf<-netCDF_Open(netcdf);
    if (is.null(ncf)) return(NULL);
    on.exit(ncdf4::nc_close(ncf));
  } else {
    ncf<-netcdf;
  }

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
    mn<-"mask_rho";      #--mask for centers
    n_eta<-ncf$dim$eta_rho$len;
    eta_bgn<-2;         #--eta for rho points starts at 2
    eta_end<-n_eta-1;   #--eta for rho points ends at n_eta-1
    n_eta<-n_eta-2;     #--new n_eta is n_eta-2
    n_xi<-ncf$dim$xi_rho$len;
    xi_bgn<-2;          #
    xi_end<-n_xi-1;     #
    n_xi<-n_xi-2;       #
  }

  vx<-ncvar_get(ncf,varid=xn);  #--value of x-coord at corner locations
  vy<-ncvar_get(ncf,varid=yn);  #--value of y-coord at corner locations
  ln<-ncvar_get(ncf,varid=lnc); #--value of lon-coord at center locations
  lt<-ncvar_get(ncf,varid=ltc); #--value of lat-coord at center locations
  vm<-ncvar_get(ncf,varid=mn);  #--value of mask at rho locations
  vh<-ncvar_get(ncf,varid="h"); #--value of bathymetric depth at rho locations

  k<-0;
  nt<-n_eta*n_xi;
  if (verbose) message("\tnumber of output cells will be ",nt);
  kid<-vector(mode="character",length=nt);
  kxi<-vector(mode="integer",  length=nt);
  ket<-vector(mode="integer",  length=nt);
  khv<-vector(mode="numeric",  length=nt);
  kln<-vector(mode="numeric",  length=nt);#--longitude at center point
  klt<-vector(mode="numeric",  length=nt);#--latitude at center point
  kgm<-vector(mode="list",     length=nt);#--vector of lists for polygon geometries
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
      kln[k]<-ln[xi,eta];
      klt[k]<-lt[xi,eta];
      khv[k]<-ifelse(is.na(vm[xi,eta])||(vm[xi,eta]!=0), vh[xi,eta],NA);#--mask = NA or !=0 indicates water at rho point
      kgm[[k]]<-sf::st_polygon(list(matrix(c(xll,yll,xlr,ylr,xur,yur,xul,yul,xll,yll),ncol=2,byrow=TRUE)), dim="XY");
    }
  }
  sfc<-sf::st_sfc(kgm,crs=strCRS);#specify the coordinate reference system
  dfr<-sf::st_sf(data.frame(ID=kid,xi=kxi,eta=ket,lon=kln,lat=klt,Z=khv,geometry=sfc));
  return(dfr);
}
