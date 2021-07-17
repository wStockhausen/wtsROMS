#'
#' @title Get information on dimensions and variables in a netcdf file
#'
#' @description Function to get information on dimensions and variables in a netcdf file.
#'
#' @param netcdf - path to netcdf file (or previously opened connection)
#' @param verbose - flag to print debugging info
#'
#' @return list of dataframes with information on dimensions ("dims") and variables ("vars").
#'
#' @details None.
#'
#' @import ncdf4
#'
#' @export
#'
netCDF_GetInfo<-function(netcdf,verbose=FALSE){
  if (class(netcdf)!="ncdf4"){
    if (verbose) message("netCDF_GetInfo: opening netcdf file '",netcdf,"'");
    ncf<-netCDF_Open(netcdf);
    if (is.null(ncf)) return(NULL);
    on.exit(ncdf4::nc_close(ncf));
  } else {
    ncf<-netcdf;
  }

  #create dataframe with info on dimensions
  nd<-length(ncf$dim);

  id<-vector(mode="integer",length=nd);
  nm<-vector(mode="character",length=nd);
  ln<-vector(mode="integer",length=nd);
  un<-vector(mode="character",length=nd);
  mm<-vector(mode="character",length=nd);
  k<-0
  for (dim in ncf$dim){
    k<-k+1;
    id[k]<-dim$id;
    nm[k]<-dim$name;
    ln[k]<-dim$len;
    un[k]<-dim$units;
    mm[k]<-paste0(min(dim$vals),":",max(dim$vals));
  }
  dfr_dims<-data.frame(id=id,name=nm,len=ln,minmax=mm,units=un,stringsAsFactors=FALSE);

  #create dataframe with info on variables
  nv<-length(ncf$var)
  id<-vector(mode="integer",length=nv);
  nm<-vector(mode="character",length=nv);
  ln<-vector(mode="character",length=nv);
  un<-vector(mode="character",length=nv);
  vs<-vector(mode="character",length=nv);
  dm<-vector(mode="character",length=nv);
  k<-0
  for (var in ncf$var){
    k<-k+1;
    id[k]<-var$id$id;
    nm[k]<-var$name;
    ln[k]<-var$longname;
    un[k]<-var$units;
    vs[k]<-paste0(var$varsize,collapse=":");
    dms<-NULL;
    for (dim in var$dim) dms<-c(dms,dim$name);
    dm[k]<-paste0(dms,collapse="; ");
  }
  dfr_vars<-data.frame(id=id,name=nm,size=vs,dims=dm,units=un,"long name"=ln,stringsAsFactors=FALSE);
  return(list(dims=dfr_dims,vars=dfr_vars));
}
