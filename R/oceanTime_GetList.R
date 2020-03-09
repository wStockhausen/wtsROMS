#'
#' @title Get a dataframe of ocean_times associated with files in a folder
#'
#' @description Function to get a dataframe of ocean_times associated with files in a folder.
#'
#' @param path - path to folder with model output files
#' @param pattern - pattern for model output filenames (e.g. "avg_*.nc")
#' @param ref - calendar reference (default = "1900-01-01 00:00:00")
#' @param verbose - flag to print extra information
#'
#' @return Dataframe with filename ocean_time (in sconds), and ocean_date (POSIXct dates) as columns.
#'
#' @details Numeric values for ocean_times are in seconds
#' relative to the calendar reference date.
#'
#' @import utils
#'
#' @export
#'
oceanTime_GetList<-function(path,
                           pattern,
                           ref=as.POSIXct("1900-01-01 00:00:00",tz="GMT"),
                           verbose=FALSE) {
  #--get filenames in path that match pattern
  fns<-list.files(path=path,
                  pattern=utils::glob2rx(pattern),
                  full.names=TRUE);
  if (verbose) cat(fns,"\n");

  #--loop through files to find file inwhich input ocean_time
  #--is matched or bracketed
  dfr<-NULL;
  for (fn in fns){#--loop over filenames
    ots<-netCDF_GetOceanTimes(fn);
    if (verbose) cat(fn," : ",ots$ocean_times,"\n");
    rows<-data.frame(fn=fn,ocean_time=ots$ocean_times,ocean_date=ots$dates,stringsAsFactors=FALSE);
    cat(fn,":\n");
    print(rows[,2:3]);
    dfr<-rbind(dfr,rows);
  }#--fn
  return(dfr);
}
