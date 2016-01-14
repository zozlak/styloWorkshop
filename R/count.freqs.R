#' converts stylo.corpus into features frequency matrix
#' @param corpus stylo.corpus object (obtained from
#'   \code{\link{parse.corpus}})
#' @param relative should frequencies be relative to text length or absolute
#' @param absent.sensitive see \code{\link{make.table.of.frequencies}}
#' @seealso \code{\link{make.table.of.frequencies}}, 
#'   \code{\link{parse.corpus}}
#' @export
count.freqs = function(corpus, relative = TRUE, absent.sensitive = TRUE){
  stopifnot(
    is(corpus, 'stylo.corpus')
  )
  
  features = sort(table(unlist(corpus)), decreasing = TRUE)
  freqs = stylo::make.table.of.frequencies(corpus, names(features), absent.sensitive, relative)
  return(freqs)
}
