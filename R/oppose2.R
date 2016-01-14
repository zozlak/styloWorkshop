#' runs the oppose() function
#' @description Allows to read configuration from file without using GUI.
#' @param file path to the saved configuration
#' @param ... any other parameters to be passed to the
#'   \code{\link{oppose}} function
#' @export
#' @import stylo
oppose2 = function(file = "oppose_config.txt", ...){
  do.call('stylo::oppose', read.config(file, ...))
}
