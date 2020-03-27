#'
#' @title Get a ROMS model grid as an sf dataframe
#'
#' @description Function to get a ROMS model grid as an sf dataframe.
#'
#' @param name - name of grid to return
#' @param verbose - flag (T/F) to print additional information
#'
#' @details Returns an sf dataframe corresponding to the requested model grid,
#' or NULL if the name is not a name for an available grid.\cr
#' Available grids are:
#' \itemize{
#'  \item{"CGOA" - Coastal Gulf of Alaska 3km grid}
#'  \item{"Bering10K" - Bering Sea 10km grid}
#' }
#'
#' @export
#'
getGrid<-function(name,verbose=FALSE){
  roms_grid<-NULL;
  if (toupper(name)=="CGOA")
    fn<-system.file(file.path("extdata/grids","CGOA.RData"),package="wtsROMS");
    #fn<-file.path("inst/extdata/grids","CGOA.RData");#--for testing
  if (toupper(name)=="BERING10K")
    fn<-system.file(file.path("extdata/grids","Bering10k.RData"),package="wtsROMS");
    #fn<-file.path("inst/extdata/grids","Bering10k.RData");#--for testing

  load(fn,verbose=verbose);
  return(roms_grid);
}
