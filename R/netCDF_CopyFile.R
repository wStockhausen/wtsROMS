#'
#' @title Copy a netCDF file, possibly dropping variables
#'
#' @description Function to copy a netCDF file, possibly dropping variables.
#'
#' @param infn - input file (file to copy)
#' @param outfn - output file
#' @param varNames - vector of variable names
#' @param varPatterns - pattern to match for variable names
#' @param action - what to do with specified variables ("keep" or "drop") on copy
#' @param test - flag (T/F) to test the copy process, but don't actually do it
#' @param verbose - flag to print extra information
#'
#' @return Dataframe with filename, ocean_time (in seconds), and ocean_date (POSIXct dates) as columns.
#'
#' @details Numeric values for ocean_times are in seconds
#' relative to the calendar reference date.
#'
#' @import ncdf4
#' @import utils
#'
#' @export
#'
netCDF_CopyFile<-function(infn,
                          outfn,
                          varNames=NULL,
                          varPatterns=NULL,
                          action=c("keep","drop"),
                          test=FALSE,
                          verbose=TRUE){
  if (inherits(infn,"ncdf4")){
    ncf <- infn;
  } else if (is.character(infn)){
    if (!file.exists(infn)){
      warning(paste0("--ERROR: 'infn' ['",infn,"'] is not a valid file name."));
      return(FALSE);
    }
    success<-FALSE;
    try({
          ncf <- ncdf4::nc_open(infn);
          on.exit(ncdf4::nc_close(ncf));
          success<-TRUE;
        });
    if (!success) return(FALSE);
  } else {
    warning("--ERROR: 'infn' should be either a file name or a ncdf4 object.");
    return(FALSE);
  }

  if (test){
    #print vars
    for (var in ncf$var){
      cat("id: ",var$id$id,", variable name: ",var$name,", long name: ",var$longname,"\n")
    }
  }

  keep_vars<-NULL;
  drop_vars<-NULL;
  if (action[1]=="keep"){
    if (test) cat("Keeping specified var names.\n")
    #determine var names to specifically keep
    keep_vars<-varNames;
    if (!is.null(varPatterns)){
      for (pattern in varPatterns){
        vars<-grep(pattern,names(ncf$var),value=TRUE);
        if (length(vars)>0) {
          if (test) cat("The following 'keep' candidates are matched by '",pattern,"':\n",paste0("\t",vars,"\n"));
          keep_vars<-c(keep_vars,vars);
        }
      }
    }
    idx <- keep_vars %in% names(ncf$var);
    if (sum(!idx)>0) stop(paste0("The following var names to keep were not found:\n",
                                    paste0("\t'",keep_vars[!idx],"'\n")));
    keep_vars<-keep_vars[idx];
  } else {
    #determine var names to drop
    if (test) cat("Dropping specified var names.\n")
    drop_vars<-varNames;
    if (!is.null(varPatterns)){
      for (pattern in varPatterns){
        vars<-grep(pattern,names(ncf$var),value=TRUE);
        if (length(vars)>0) {
          if (test) cat("The following 'drop' candidates are matched by '",pattern,"':\n",paste0("\t",vars,"\n"));
          drop_vars<-c(drop_vars,vars);
        }
      }
    }
    idx<-names(ncf$var) %in% drop_vars;
    keep_vars<-names(ncf$var)[!idx];
  }

  if (test|verbose){
    if (!is.null(drop_vars)) cat("The following vars will be dropped:\n",paste0("\t'",drop_vars,"'\n"));
    cat("The following vars will be kept:\n",paste0("\t'",keep_vars,"'\n"));
  }

  if (!test){
    #create new (or overwrite old) netCDF file
    if (file.exists(outfn)) file.remove(outfn);
    new_vars <- ncf$var[keep_vars];#list of vars corresponding to keep_vars
    #check that new_vars are ok
    cat(paste0("Checking ",length(new_vars)," == ",length(keep_vars),"\n"))
    str<-NULL;
    for (keep_var in keep_vars){
      var<-new_vars[[keep_var]];
      if (verbose) cat(paste0("checking var '",keep_var,"' with name '",var$name,"' is an ncvar4 object\n"))
      if (class(var)!="ncvar4") {
        str<-paste0("ERROR: '",var$name,"' is not a netCDF variable!\n");
        cat(str);
        var
      }
      if (!is.null(str)) stop(str);
    }
    new_ncf<-ncdf4::nc_create(outfn,new_vars,verbose=verbose);

    for (var_name in names(new_vars)){
      data<-ncdf4::ncvar_get(ncf,var_name,collapse_degen=FALSE);
      ncdf4::ncvar_put(new_ncf,var_name,data);
    }

    #close files
    ncdf4::nc_close(new_ncf);
  }
  #ncf closes on exit if it was opened in this function
  return(TRUE); #signal file was copied successfully
}
