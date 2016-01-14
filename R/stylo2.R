#' runs the stylo() function
#' @description Allows to read configuration from file without using GUI.
#' @param file path to the saved configuration
#' @param ... any other parameters to be passed to the
#'   \code{\link{stylo}} function
#' @export
#' @import stylo
stylo2 = function(file = 'stylo_config.txt', ...){
  do.call('stylo::stylo', read.config(file, ...))
}
