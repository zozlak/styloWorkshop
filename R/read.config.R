#' prepares stylo functions parameters
#' @description 
#' Used internally by \code{\link{stylo2}}, \code{\link{classify2}},
#' \code{\link{rolling.classify2}}, \code{\link{rolling.delta2}},
#' \code{\link{oppose2}}.
#' @param file path to the file with a stored condig
#' @param ... manualy set parameters
#' @return list of parameters
read.config = function(file, ...){
  vars = list(...)
  if(file.exists(file)){
    source(file, local = TRUE) 
    vars = append(vars, mget(ls()))
    vars = vars[!duplicated(names(vars))]
  }
  return(vars)  
}
