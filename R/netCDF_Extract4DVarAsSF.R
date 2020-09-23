#'
#' @title Create a \code{sf} dataframe representing a single time step and depth layer from a ROMS 4DVar in a netcdf file
#'
#' @description Function to create a \code{sf} dataframe representing a single time step and depth layer from a ROMS 4DVar in a netcdf file.
#'
#' @param netcdf - path to netcdf file with ROMS model output, or a ncdf4 object connected to a netcdf file
#' @param roms_grid - roms grid as an sf dataframe object
#' @param variable - variable name to extract
#' @param timeslice - time slice from file to extract
#' @param layer - depth layer to extract [positive numbers start from bottom; non-negative numbers start from surface]
#' @param verbose - flag (T/F) to print info
#'
#' @return A \code{sf} dataframe object representing the layer
#'
#' @details Creates an sf dataframe representing a single time step and depth layer from a ROMS 4DVar in a netcdf file.
#'
#' @import ncdf4
#' @import wtsGIS
#' @import wtsUtilities
#'
#' @export
#'
netCDF_Extract4DVarAsSF<-function(netcdf,
                                  roms_grid,
                                  variable,
                                  timeslice=1,
                                  layer=1,
                                  verbose=FALSE){
  if (class(netcdf)!="ncdf4"){
    if (verbose) message("netCDF_CreateModelOutput: opening netcdf file '",netcdf,"'");
    ncf<-netCDF_Open(netcdf);
    if (is.null(ncf)) return(NULL);
    on.exit(ncdf4::nc_close(ncf));
  } else {
    ncf<-netcdf;
  }

  vi<-netCDF_GetInfoVarDims(ncf,variable=variable,verbose=verbose);
  vr<-ncdf4::ncvar_get(ncf,varid=variable,collapse_degen=FALSE);

  grid_type<-gsub("eta_","",grep("eta_",names(vi),value=TRUE),fixed=TRUE);
  if (verbose) message("\tgrid_type is '",grid_type,"'");

  id_eta<-grep("eta",names(vi));
  id_xi <-grep("xi", names(vi));
  id_s  <-grep("s_", names(vi));
  id_ot <-grep("ocean_time",names(vi));

  if (layer<1) layer<-vi[id_s]+layer;#--layer < 1 starts from surface
  if ((layer>vi[id_s])|(layer<1)) stop("netCDF_CreateModelOutput: requested layer does not exist.")

  n_eta<-vi[id_eta];
  n_xi <-vi[id_xi];
  if (grid_type=="rho"){
    eta_bgn<-2;
    eta_end<-n_eta-1;
    n_eta<-n_eta-2;
    xi_bgn<-2;
    xi_end<-n_xi-1;
    n_xi<-n_xi-2;
  }

  k<-0;
  nt<-n_eta*n_xi;
  if (verbose) message("\tnumber of output cells will be ",nt);
  kid<-vector(mode="character",length=nt);
  kxi<-vector(mode="integer",  length=nt);
  ket<-vector(mode="integer",  length=nt);
  kvr<-vector(mode="numeric",  length=nt);
  kgm<-vector(mode="list",     length=nt);
  for (eta in eta_bgn:eta_end){
    for (xi in xi_bgn:xi_end){
      k<-k+1;
      kid[k]<-paste0(xi,"_",eta);
      kxi[k]<-xi;
      ket[k]<-eta;
      kvr[k]<-vr[xi,eta,layer,timeslice];
    }
  }
  dfr<-sf::st_sf(data.frame(ID=kid,xi=kxi,eta=ket,var=kvr,geometry=roms_grid$geometry));
  names(dfr)[4]=variable;
  return(dfr);
}
