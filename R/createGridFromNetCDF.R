#'
#' @title Create a \code{sf} dataframe representing a ROMS grid from a netcdf file
#'
#' @description Function to create a \code{sf} dataframe representing a ROMS grid from a netcdf file.
#'
#' @param netcdf - path to netcdf file with ROMS grid, or a ncdf4 object connected to a netcdf file
#' @param strCRS - coordinate reference for grid
#'
#' @return A \code{sf} dataframe object representing the grid
#'
#' @details Creates an sf dataframe from
#' a netcdf file representing a ROMS grid.
#'
#' @import ncdf4
#' @import wtsGIS
#' @import wtsUtilities
#'
#' @export
#'
createGridFromNetCDF<-function(netcdf,
                               grid_type=c("rho","psi","u","v"),
                               coord_type=c("latlon","xy"),
                               strCRS = ifelse(tolower(coord_type[1])=="latlon",wtsGIS::getCRS("NAD83"),wtsGIS::getCRS("AlaskaAlbers")),
                               test=TRUE,
                               verbose=TRUE){
  grid_type  <-grid_type[1];
  coord_type<-coord_type[1];
  strCRS    <-strCRS[1];

  valid_grid_types<-c("rho","psi","u","v");
  if (!wtsUtilities::checkValidType(grid_type,valid_grid_types)) stop();
  valid_coord_types<-c("latlon","xy");
  if (!wtsUtilities::checkValidType(grid_type,valid_grid_types)) stop();

  if (inherits(netcdf,"ncdf4")){
    ncf <- netcdf;
  } else if (is.character(netcdf)){
    if (!file.exists(netcdf)){
      warning(paste0("--ERROR: input variable 'netcdf' ['",netcdf,"'] is not a valid file name."));
      return(FALSE);
    }
    success<-FALSE;
    try({
      ncf <- ncdf4::nc_open(netcdf);
      on.exit(ncdf4::nc_close(ncf));
      success<-TRUE;
    });
    if (!success) return(FALSE);
  } else {
    warning("--ERROR: input variable 'netcdf' should be either a file name or a ncdf4 object.");
    return(FALSE);
  }

  if (test){
    #print vars
    for (var in ncf$var){
      cat(paste0("id: ",var$id$id,", variable name: ",var$name,", long name: ",var$longname,"\n"));
    }
  }

  if (tolower(coord_type)=="latlon"){
    #--coordinate type is geographic

  } else {
    #--coordinate type is cartesian (xy)
  }
}
