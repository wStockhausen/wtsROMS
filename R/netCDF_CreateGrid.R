#'
#' @title Create a \code{sf} dataframe representing a ROMS grid from a netcdf file
#'
#' @description Function to create a \code{sf} dataframe representing a ROMS grid from a netcdf file.
#'
#' @param netcdf - path to netcdf file with ROMS grid, or a ncdf4 object connected to a netcdf file
#' @param grid_type -
#' @param coord_type -
#' @param strCRS - coordinate reference for grid
#' @param verbose - flag to print additional information
#'
#' @return A \code{sf} dataframe object representing the grid
#'
#' @details Creates an sf dataframe from a netcdf file representing a ROMS grid.
#' Masked cells (land) have a value of NA.
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
                             strCRS = ifelse(tolower(coord_type[1])=="latlon",wtsGIS::getCRS("NAD83"),wtsGIS::getCRS("AlaskaAlbers")),
                             verbose=TRUE){
  grid_type  <-grid_type[1];
  coord_type<-coord_type[1];
  strCRS    <-strCRS[1];

  valid_grid_types<-c("rho","psi","u","v");
  if (!wtsUtilities::checkValidType(grid_type,valid_grid_types)) stop();
  valid_coord_types<-c("latlon","xy");
  if (!wtsUtilities::checkValidType(grid_type,valid_grid_types)) stop();

  ncf<-netCDF_Open(netcdf);
  if (is.null(ncf)) return(NULL);
  on.exit(ncdf4::nc_close(ncf));

  if (tolower(coord_type)=="latlon"){
    #--coordinate type is geographic
    if (tolower(grid_type=="rho")){
      xn<-"lon_psi";
      yn<-"lat_psi";
      mn<-"mask_rho";
    }
  } else {
    #--coordinate type is cartesian (xy)
    if (tolower(grid_type=="rho")){
      xn<-"x_psi";
      yn<-"y_psi";
      mn<-"mask_rho";
    }
  }
  n_eta<-ncf$dim$eta_psi$len;
  n_xi <-ncf$dim$xi_psi$len;

  vx<-ncvar_get(ncf,varid=xn);
  vy<-ncvar_get(ncf,varid=yn);
  vm<-ncvar_get(ncf,varid=mn);
  vh<-ncvar_get(ncf,varid="h");

  k<-0;
  nt<-(n_eta-1)*(n_xi-1)
  kid<-vector(mode="character",length=nt);
  kxi<-vector(mode="integer",  length=nt);
  ket<-vector(mode="integer",  length=nt);
  khv<-vector(mode="numeric",  length=nt);
  kgm<-vector(mode="list",     length=nt);
  for (eta in 1:(n_eta-1)){
    for (xi in 1:(n_xi-1)){
      k<-k+1;
      xl<-vx[xi  ,eta  ];
      xr<-vx[xi+1,eta  ];
      yb<-vy[xi  ,eta  ];
      yt<-vy[xi,  eta+1];
      kid[k]<-paste0(xi,"_",eta);
      kxi[k]<-xi;
      ket[k]<-eta;
      khv[k]<-ifelse(is.na(vm[xi+1,eta+1])||(vm[xi+1,eta+1]!=0), vh[xi+1,eta+1],NA);#mask = NA or !=0 indicates water
      kgm[[k]]<-sf::st_polygon(list(matrix(c(xl,yb,xr,yb,xr,yt,xl,yt,xl,yb),ncol=2,byrow=TRUE)), dim="XY");
    }
  }
  sfc<-sf::st_sfc(kgm,crs=strCRS);
  dfr<-sf::st_sf(data.frame(ID=kid,xi=kxi,eta=ket,Z=khv,geometry=sfc));
  return(dfr);
}
