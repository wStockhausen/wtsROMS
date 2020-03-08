#'
#' @title Create a \code{sf} dataframe representing a ROMS model output layer from a netcdf file
#'
#' @description Function to create a \code{sf} dataframe representing a ROMS model output layer from a netcdf file.
#'
#' @param netcdf - path to netcdf file with ROMS model output, or a ncdf4 object connected to a netcdf file
#' @param roms_grid - roms grid as an sf dataframe object
#' @param variable - variable name to extract
#' @param timeslice - time slice from file to extract
#' @param layer - vertical layer to extract (1: bottom)
#'
#' @return A \code{sf} dataframe object representing the layer
#'
#' @details Creates an sf dataframe from
#' a netcdf file representing a ROMS model output layer.
#'
#' @import ncdf4
#' @import wtsGIS
#' @import wtsUtilities
#'
#' @export
#'
netCDF_CreateModelOutput<-function(netcdf,
                                    roms_grid,
                                    variable,
                                    timeslice=1,
                                    layer=1){
  ncf<-netCDF_Open(netcdf);
  if (is.null(ncf)) return(NULL);
  on.exit(ncdf4::nc_close(ncf));

  n_eta<-ncf$dim$eta_psi$len;
  n_xi <-ncf$dim$xi_psi$len;

  vr<-ncdf4::ncvar_get(ncf,varid=variable);

  k<-0;
  nt<-(n_eta-1)*(n_xi-1)
  kid<-vector(mode="character",length=nt);
  kxi<-vector(mode="integer",  length=nt);
  ket<-vector(mode="integer",  length=nt);
  kvr<-vector(mode="numeric",  length=nt);
  kgm<-vector(mode="list",     length=nt);
  for (eta in 1:(n_eta-1)){
    for (xi in 1:(n_xi-1)){
      k<-k+1;
      kid[k]<-paste0(xi,"_",eta);
      kxi[k]<-xi;
      ket[k]<-eta;
      kvr[k]<-vr[xi+1,eta+1,layer,timeslice];
    }
  }
  dfr<-sf::st_sf(data.frame(ID=kid,xi=kxi,eta=ket,var=kvr,geometry=roms_grid$geometry));
  names(dfr)[4]=variable;
  return(dfr);
}
