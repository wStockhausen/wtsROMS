#'
#' @title Convert ocean_times from numeric to POSIXct
#'
#' @description Function to convert ocean_times from numeric to character.
#'
#' @param ot - ocean_time, as numeric vector
#' @param ref - calendar reference (default = "1900-01-01 00:00:00")
#'
#' @return Vector with ocean_times as POSIXct dates.
#'
#' @details Input numeric values associated with ocean_times should be in seconds
#' relative to the model reference date.
#'
#' @export
#'
oceanTime_ConvertToPOSIXct<-function(ot,
                                     ref="1900-01-01 00:00:00"){
  if (is.character(ref)) ref<-as.POSIXct(ref,tz="GMT");
  otc<-as.POSIXct(ot,tz="GMT",origin=ref);
  return(otc);
}

#'
#' @title Convert ocean_times from character or POSIXct to numeric
#'
#' @description Function to convert ocean_times from character or POSIXct to numeric.
#'
#' @param ot - ocean_time, as character or POSIXct vector
#' @param ref - calendar reference (default = "1900-01-01 00:00:00")
#'
#' @return Vector with ocean_times as numeric seconds since the reference date.
#'
#' @details Input numeric values associated with ocean_times should be a vector of character strings
#' formatted as 'YYY-MM-DD HH:MM:SS' or POSIXct.
#'
#' @export
#'
oceanTime_ConvertToNumeric<-function(ot,
                                     ref="1900-01-01 00:00:00"){
  if (is.character(ref)) ref<-as.POSIXct(ref,tz="GMT");
  if (is.character(ot)) otn<-as.numeric(as.POSIXct(ot,tz="GMT")-as.numeric(ref));
  if (is.POSIXct(ot))   otn<-as.numeric(ot)-as.numeric(ref);
  return(otn);
}
