#'
#' @title Get ROMS file names corresponding to integration dates
#'
#' @description Function to get ROMS file names corresponding to integration dates
#'
#' @param topDir - path to folder with ROMS output files
#' @param start - first time to include (as POSIXct object)
#' @param delt - integration timestep in seconds (default = 24*3600)
#'
#' @return a dataframe wth ROMS file information by date
#'
#' @import dplyr
#'
#' @export
#'
getInfo_ROMSFiles<-function(topDir,
                           start=as.POSIXct("1982-01-01 00:00:00",tz="GMT"),
                           delt=24*3600){
  #--get ocean times
  dfr_ots = wtsROMS::oceanTime_GetTimes(topDir,"avg_*.nc");
  #--extract file info

  dfrROMSfiles = list();
  loop=TRUE; dt = start;
  while(loop){
        dtp = format(dt,"%Y-%m-%d %H:%M:%S");
        cat("Searching for",dtp,"\n");
        res = wtsROMS::oceanTime_FindFile(dtp,dfr_ots,verbose=TRUE);
        if (!is.null(res)) {
          dfrROMSfiles[[dtp]] = res;
        } else {
          loop=FALSE;
        }
        dt = dt + delt;
  }#--loop
  dfrROMSfiles = dplyr::bind_rows(dfrROMSfiles);
  return(dfrROMSfiles);
}
