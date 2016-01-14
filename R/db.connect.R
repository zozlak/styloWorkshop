#' establishes connection with the corpus database
#' @description 
#' Connection is set up in the \code{.src} variable of the user environment, so
#' it can be used by \code{\link{get.texts}} automatically.
#' @param dbname database name
#' @param host database host
#' @param user user name
#' @param password user password
#' @param ... other parameters to be passed to dplyr::src_postgres
#' @return connection
#' @export
db.connect = function(dbname = 'styloworkshop', host = 'helios.arz.oeaw.ac.at', user = 'styloworkshop', password = '95CrQCvwP6', ...){
  src = dplyr::src_postgres(dbname, host, user = user, password = password, ...)
  assign('.src', src, 1)
  return(invisible(src))
}
