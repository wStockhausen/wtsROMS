#'
#' @title Get a ROMS model grid as an sf dataframe
#'
#' @description Function to get a ROMS model grid as an sf dataframe.
#'
#' @param name - name of grid to return
#' @param verbose - flag (T/F) to print additional information
#'
#' @details Returns an sf dataframe corresponding to the requested model grid.\cr
#' Available grids are:
#' \itemize{
#'  \item{"CGOA" - Coastal Gulf of Alaska 3km grid}
#' }
#'
#' @export
#'
getGrid<-function(name,verbose=FALSE){
  if (toupper(name)=="CGOA")
    fn<-system.file(file.path("extdata/grids","CGOA.RData"),package="rROMS");
    #fn<-file.path("inst/extdata/grids","CGOA.RData");#--for testing

  load(fn,verbose=verbose);
  return(roms_grid);
}
