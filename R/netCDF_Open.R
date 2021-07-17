#'
#' @title Open a netcdf file
#'
#' @description Function to open a netcdf file and return an opened connection.
#'
#' @param netcdf - path to netcdf file (or previously opened connection)
#'
#' @return object of class 'ncdf4', an opened connection to a netcdf file.
#'
#' @details The returned object should be closed by the user using\code{ncdf4::nc_close(...)}.
#' \code{netcdf} is simply passed through if it is already an open connection.
#'
#' @import ncdf4
#'
#' @export
#'
netCDF_Open<-function(netcdf){
  if (inherits(netcdf,"ncdf4")){
    return(netcdf);
  } else if (is.character(netcdf)){
    if (!startsWith(netcdf,"http")){
      message("Trying to access local netcdf file.")
      if (!file.exists(netcdf)){
        warning(paste0("--ERROR: input variable 'netcdf' ['",netcdf,"'] is not a valid file name."));
        return(NULL);
      }
    } else {
      message("Trying to access remote netcdf file.")
    }
    success<-FALSE;
    try({
      ncf <- ncdf4::nc_open(netcdf);
      return(ncf);
    });
    return(NULL);
  } else {
    warning("--ERROR: input variable 'netcdf' should be either a file name or a ncdf4 object.");
    return(NULL);
  }
}