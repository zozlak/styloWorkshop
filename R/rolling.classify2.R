#' runs the rolling.classify() function
#' @description Allows to read configuration from file without using GUI.
#' @param file path to the saved configuration
#' @param ... any other parameters to be passed to the
#'   \code{\link{rolling.classify}} function
#' @export
#' @import stylo
rolling.classify2 = function(file = 'rolling.classify_config.txt', ...){
  do.call('stylo::rolling.classify', read.config(file, ...))
}
