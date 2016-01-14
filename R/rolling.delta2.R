#' runs the rolling.delta() function
#' @description Allows to read configuration from file without using GUI.
#' @param file path to the saved configuration
#' @param ... any other parameters to be passed to the
#'   \code{\link{rolling.delta}} function
#' @export
#' @import stylo
rolling.delta2 = function(file = 'rolling.delta_config.txt', ...){
  do.call('stylo::rolling.delta', read.config(file, ...))
}
