#'
#' @title Get ocean_times from a ROMS model netcdf file
#'
#' @description Function to get ocean_times from a ROMS model netcdf file.
#'
#' @param netcdf - path to ROMS model netcdf file (or previously opened connection)
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return tibble (dataframe) with columns ocean_times (in seconds since model origin) and dates (POSIXct dates),
#' or NULL if file is invalid.
#'
#' @details Dates associated with ocean_times are relative to the model reference date determined by
#' parsing the \code{units} associated with the \code{ocean_time} dimension in the netcdf dataset.
#'
#' @import ncdf4
#' @import tibble
#'
#' @export
#'
netCDF_GetOceanTimes<-function(netcdf,verbose=FALSE){
  if (class(netcdf)!="ncdf4"){
    if (verbose) message("netCDF_GetOceanTimes: opening netcdf file '",netcdf,"'");
    ncf<-netCDF_Open(netcdf);
    if (is.null(ncf)) return(NULL);
    on.exit(ncdf4::nc_close(ncf));
  } else {
    ncf<-netcdf;
  }

  dim<-ncf$dim$ocean_time;
  if (is.null(dim)) {
    msg<-"Dimension 'ocean_time' not found! The netcdf file is probably not a ROMS model output file";
    warning(msg);
    return(NULL);
  }
  ocean_time<-dim$vals;
  units_str <- dim$units;
  units<-strsplit(units_str," ",fixed=TRUE)[[1]];
  ref<-as.POSIXct(paste(units[3],units[4]),tz="GMT");
  dts<-as.POSIXct(ocean_time,origin=ref,tz="GMT");
  return(tibble::tibble(ocean_times=ocean_time,dates=dts));
}