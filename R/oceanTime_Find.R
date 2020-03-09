#'
#' @title Find the file in which an input ocean_time is bracketed
#'
#' @description Function to find the file in which an input ocean_time is bracketed.
#'
#' @param ocean_time - time (in seconds relative to the calendar reference or as POSIXct date)
#' @param path - path to folder with model output files
#' @param pattern - pattern for model output filenames (e.g. "avg_*.nc")
#' @param ref - calendar reference (default = "1900-01-01 00:00:00")
#' @param verbose - flag to print extra information
#'
#' @return Name of model output file in which the input ocean_time is bracketed.
#'
#' @details None.
#'
#' @import utils
#'
#' @export
#'
oceanTime_Find<-function(ocean_time,
                         path,
                         pattern,
                         ref=as.POSIXct("1900-01-01 00:00:00",tz="GMT"),
                         verbose=FALSE) {
  #--convert ocean_time to numeric, if necessary
  ot0<-ocean_time;
  if (is.character(ocean_time)) ot0<-oceanTime_ConvertToNumeric(ocean_time,ref);
  #--get filenames in path that match pattern
  fns<-list.files(path=path,
                  pattern=utils::glob2rx(pattern),
                  full.names=TRUE);
  if (verbose) cat(fns,"\n");

  #--loop through files to find file inwhich input ocean_time
  #--is matched or bracketed
  match<-FALSE;   #flag indicating match
  fnr<-NULL;      #matched file
  last_ot<--Inf;  #ocean_time at previous time step
  last_fn<-NULL;  #file name at previous time step
  for (fn in fns){#--loop over filenames
    if (verbose) cat(paste0("testing file '",fn,"':\n"));
    ots<-netCDF_GetOceanTimes(fn)$ocean_times;
    for (ot in ots){#--loop over ocean_times in file
      if (verbose) cat(paste0("testing ",last_ot," <= ",ot0," < ",ot,"\n"));
      if ((last_ot<=ot0)&(ot0<ot)){
        match<-TRUE;
        fnr<-last_fn;
        bracket<-c(last_ot,ot);
      } else {
        last_fn<-fn;
        last_ot<-ot;
      }
      if (match) break;
    }#--ot
    if (match) break;
  }#--fn
  if (match){
    msg<-paste0("Bracketed ocean_time ",oceanTime_ConvertToPOSIXct(ot0,ref),
                " by ",
                paste(oceanTime_ConvertToPOSIXct(bracket,ref),collapse=", "));
    message(msg);
  } else {
    msg<-"no file was found.\n";
    warning(msg,immediate.=TRUE);
  }
  return(fnr);
}
