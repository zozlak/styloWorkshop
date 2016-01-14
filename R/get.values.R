#' returns values frequency list for a given column or columns set
#' @param data set of texts (obtained from \code{\link{get.texts}})
#' @param columns column name (or vector of column names)
#' @return data.frame with frequency list
#' @seealso \code{\link{get.texts}}
#' @import dplyr
#' @export
#' @examples 
#' \dontrun{
#'   # connect to the texts database
#'   db.connect()
#'   
#'   # fetch and filter texts
#'   allTexts = get.texts()
#'   
#'   # get frequency list for all authors
#'   get.values(allTexts, 'author')
#' }
get.values = function(data, columns){
  stopifnot(
    is.tbl(data),
    is.vector(columns), is.character(columns), all(!is.na(columns)), all(!columns %in% 'text'), all(columns %in% colnames(data))
  )
  data %>% 
    group_by_(.dots = columns) %>%
    summarize_(n = ~ n()) %>%
    collect() %>%
    arrange_('desc(n)') %>%
    as.data.frame() %>%
    return()
}
