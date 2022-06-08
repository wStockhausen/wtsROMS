#'
#' @title Find the file in which an input ocean_time is bracketed
#'
#' @description Function to find the file in which an input ocean_time is bracketed.
#'
#' @param ocean_time - time (in seconds relative to the calendar reference or as POSIXct date)
#' @param dfr_files - dataframe with ocean times by file name (output from \code{\link{oceanTime_GetTimes}})
#' @param path - file path to folder with model output files
#' @param pattern - pattern for model output filenames (e.g. "avg_*.nc")
#' @param ref - calendar reference (default = "1900-01-01 00:00:00")
#' @param verbose - flag to print extra information
#'
#' @return One-row dataframe with columns:
#' \itemize{
#'   \item{ot,input ocean time (s)}
#'   \item{date,associated date}
#'   \item{fn,Name of model output file in which the input ocean_time is bracketed.}
#'   \item{ts,lefthand bracketing time slice in file}
#'   \item{bracket, bracketing times}
#' }
#'
#' @details None.
#'
#' @import utils
#'
#' @export
#'
oceanTime_FindFile<-function(ocean_time,
                             dfr_files=NULL,
                             path="",
                             pattern="*.nc",
                             ref=as.POSIXct("1900-01-01 00:00:00",tz="GMT"),
                             verbose=FALSE) {
  #--convert ocean_time to numeric, if necessary
  ot0<-ocean_time;
  if (is.character(ocean_time)) ot0<-oceanTime_ConvertToNumeric(ocean_time,ref);

  if (!is.null(dfr_files)){
    ot0d<-oceanTime_ConvertToPOSIXct(ot0,ref);
    idx = max(which(dfr_files$ocean_time<=ot0));
    cat("idx = ",idx,"\n")
    fnr = dfr_files$fn[idx];
    sub = dfr_files %>% subset(fn==fnr);
    tsr = max(which(sub$ocean_time<=ot0));
    if (!is.na(dfr_files$ocean_date[idx+1])){
      bracket = paste(c(dfr_files$ocean_date[idx],dfr_files$ocean_date[idx+1]),collapse=", ");
      msg<-paste0("Bracketed ocean_time ",ot0," by [",bracket,"]");
      message(msg);
      return(data.frame(ot=ot0,date=ot0d,fn=fnr,ts=tsr,bracket=bracket,stringsAsFactors=FALSE));
    }
  } else {
    #--get filenames in path that match pattern
    fns<-list.files(path=path,
                    pattern=utils::glob2rx(pattern),
                    full.names=TRUE);
    if (verbose) cat(fns,"\n");

    #--loop through files to find file in which input ocean_time
    #--is matched or bracketed
    match<-FALSE;   #flag indicating match
    fnr<-NULL;      #matched file
    tsr<-NULL;      #matched timeslice (timeslice for previous time step)
    last_fn<-NULL;  #file name at previous time step
    last_ot<--Inf;  #ocean_time at previous time step
    last_ts<-NULL;  #number of time slices in last file
    for (fn in fns){#--loop over filenames
      if (verbose) cat(paste0("testing file '",fn,"':\n"));
      ots<-netCDF_GetOceanTimes(fn)$ocean_times;
      tsr<-0;
      for (ot in ots){#--loop over ocean_times in file
        tsr<-tsr+1;
        if (verbose) cat(paste0("testing ",last_ot," <= ",ot0," < ",ot,"\n"));
        if ((last_ot<=ot0)&(ot0<ot)){
          match<-TRUE;
          fnr<-last_fn;
          tsr<-last_ts;
          bracket<-c(last_ot,ot);
        } else {
          last_fn<-fn;
          last_ot<-ot;
          last_ts<-tsr;
        }
        if (match) {break;}
      }#--ot
      if (match) {break;}
    }#--fn
    if (match){
      ot0d<-oceanTime_ConvertToPOSIXct(ot0,ref);
      bracket<-paste(oceanTime_ConvertToPOSIXct(bracket,ref),collapse=", ");
      msg<-paste0("Bracketed ocean_time ",ot0d," by [",bracket,"]");
      message(msg);
      return(data.frame(ot=ot0,date=ot0d,fn=fnr,ts=tsr,bracket=bracket,stringsAsFactors=FALSE));
    }
  }
  msg<-"no file was found.\n";
  warning(msg,immediate.=TRUE);
  return(NULL);
}
