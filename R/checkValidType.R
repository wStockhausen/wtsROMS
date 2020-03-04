checkValidType<-function(type,
                         valid_types){
  name <-deparse(substitute(type));
  msg<-paste0(name," is invalid. Got '",type,"' but expected one of: ",paste0("'",valid_types,"'",collapse=","));
  if (any(tolower(type)==valid_types)) return(TRUE);
  cat(msg,"\n")
  return(FALSE);
}