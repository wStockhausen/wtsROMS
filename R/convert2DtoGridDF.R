#'
#' @title Create a \pkg{tibble} tibble representing a ROMS 2d spatial variable
#'
#' @description Function to create a \pkg{tibble} tibble representing a ROMS 2d spatial variable.
#'
#' @param m - matrix (xi x eta) from call to
#' @param name - name for the `value` column (defaults to "value")
#'
#' @return A \pkg{tibble} tibble object representing the grid
#'
#' @details Creates a tibble from a matrix representing a ROMS 2d spatial variable.
#' The matrix should (probably) represent the full xi,eta dimensions of the ROMS model. Any
#' decimation should be done afterward on the resulting tibble. This will ensure the xi, eta
#' indices in the dataframe correspond to the correct grid cells in the original output.
#'
#' The returned \pkg{sf} object has columns:
#' \itemize{
#'   \item{ID - string "xi_eta"}
#'   \item{xi - index value}
#'   \item{eta - eta index value}
#'   \item{`name` - matrix value at `[xi,eta]`}
#' }
#'
#' @import tibble
#'
#' @export
#'
convert2DtoGridDF<-function(m,name="value"){
  #--m is converted to a vector by row, so
  #----column index (eta) cycles fastest
  #----row index (xi) cycles slowest
  nxi  = dim(m)[1]; neta = dim(m)[2];
  xi  = rep(1:nxi, each=neta);
  eta = rep(1:neta,times=nxi);
  tbl = tibble::tibble(ID=paste0(xi,"_",eta),xi=xi,eta=eta,value=as.vector(m));
  names(tbl)[4] = name;#--change 'value' column to input name
  return(tbl);
}