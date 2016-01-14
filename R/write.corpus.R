#' writes texts to a disk in a format supported by the stylo package
#' @param data set of texts to be written (obtained from
#'   \code{\link{get.texts}})
#' @param groupBy1 variable name (or vector of variable names) describing stylo
#'   text class (texts grouping variable, e.g. author)
#' @param groupBy2 variable name (or vector of variable names) describing stylo
#'   text subclass (e.g. text title)
#' @param directory directory path to write corpus into
#' @param sample fraction of texts to sample from each group (from 0 to 1)
#' @return data.frame all texts which were not sampled
#' @param limit maximum number of characters in each file
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
#'   interestingTexts = allTexts %>%
#'     filter(
#'       author %in% c('ABronte', 'CBronte', 'EBronte', 'Austen', 'Dickens', 
#'         'Eliot', 'Fielding', 'Richardson', 'Sterne', 'Thackeray', 'Trollope')
#'     ) %>%
#'     collect()
#'   
#'   # write to disk in the stylo package format
#'   # (for the stylo() function, so in directory called "corpus")
#'   # taking the author as a main category and the title as a subcategory
#'   write.corpus(interestingTexts, 'author', 'title', 'corpus')
#'   
#'   # write to disk in the stylo package format
#'   # (for the classify() function, so there are two sets of text in 
#'   #   directories "primary_set" and "secondary_set")
#'   # taking the author as a main category and the title as a subcategory
#'   primarySet = interestingTexts %>%
#'     group_by(author) %>%
#'     sample_frac(0.7) # assign 70% of each uthors' texts to the primary set
#'   secondarySet = interestingTexts %>%
#'     anti_join(primarySet %>% select(text_id)) # all other goes to secondary set
#'   write.corpus(primarySet, 'author', 'title', 'primary_set')
#'   write.corpus(secondarySet, 'author', 'title', 'secondary_set')
#' }
write.corpus = function(data, groupBy1 = character(), groupBy2 = character(), directory = 'corpus', sample = 1, limit = 10^7){
  stopifnot(
    is.data.frame(data) | is.tbl(data),
    is.vector(groupBy1), is.character(groupBy1), all(!is.na(groupBy1)), all(!groupBy1 %in% 'text'), all(groupBy1 %in% colnames(data)),
    is.vector(groupBy2), is.character(groupBy2), all(!is.na(groupBy2)), all(!groupBy2 %in% 'text'), all(groupBy2 %in% colnames(data)),
    is.vector(directory), is.character(directory), length(directory) == 1,
    is.vector(sample), is.numeric(sample), length(sample) == 1, all(!is.na(sample)), all(sample >= 0), all(sample <= 1),
    is.vector(limit), is.numeric(limit), length(limit) == 1, all(!is.na(limit))
  )
  if(is(data, 'tbl_sql')){
    data = collect(data)
  }
  
  if(length(groupBy1) == 0){
    data$group1 = data$text_id 
  }else{
    data$group1 = do.call('paste', data[, groupBy1])
  }
  data$group1 = sub('_', '', data$group1)
  
  if(length(groupBy2) == 0){
    data$group2 = data$text_id 
  }else{
    data$group2 = do.call('paste', data[, groupBy2])
  }
  data$group2 = sub('_', '', data$group2)
  
  data$title = paste(data$group1, data$group2, sep = '_')
  
  data = group_by_(data, 'title')
  data = data %>%
    mutate_(
      nchar = ~ cumsum(nchar)
    ) %>%
    filter_(~ nchar < limit) %>%
    summarize_(text  = ~ paste(text, collapse = '\n'))

  if(!dir.exists(directory)){
    dir.create(directory, recursive = TRUE)
  }
  for(i in seq_along(data$title)){
    writeLines(data$text[i], paste(directory, paste0(sub('[/\\\\]', '_', data$title[i]), '.txt'), sep = '/'))
  }
}
