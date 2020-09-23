#'
#' @title Get information on dimensions for a variable in a netcdf file
#'
#' @description Function to get information on dimensions for a variable in a netcdf file.
#'
#' @param netcdf - path to netcdf file (or previously opened connection)
#' @param variable - variable name to extract
#' @param verbose - flag (T/F) to print info
#'
#' @return vector of dimension sizes, with dimension names as element names
#'
#' @details None.
#'
#' @import ncdf4
#'
#' @export
#'
netCDF_GetInfoVarDims<-function(netcdf,
                             variable,
                             verbose=FALSE){
  if (class(netcdf)!="ncdf4"){
    if (verbose) message("netCDF_GetInfoVarDims: opening netcdf file '",netcdf,"'");
    ncf<-netCDF_Open(netcdf);
    if (is.null(ncf)) return(NULL);
    on.exit(ncdf4::nc_close(ncf));
  } else {
    ncf<-netcdf;
  }


  #get info on variable
  varInfo<-ncf$var[[variable]];
  if (is.null(varInfo)) stop("netCDF_VarDimsInfo: no variable named ",variable," found!");

  ndms<-varInfo$ndims;
  size<-varInfo$size;

  dims<-vector("numeric",length=ndms);
  for (idm in 1:ndms){
    dimInfo         <-varInfo$dim[[idm]];
    dims[idm]       <-dimInfo$len;
    names(dims)[idm]<-dimInfo$name;
  }
  return(dims);
}
