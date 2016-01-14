#' runs the classify() function
#' @description Allows to read configuration from file without using GUI.
#' @param file path to the saved configuration
#' @param ... any other parameters to be passed to the
#'   \code{\link{classify}} function
#' @export
#' @import stylo
classify2 = function(file = 'classify_config.txt', ...){
  do.call('stylo::classify', read.config(file, ...))
}
