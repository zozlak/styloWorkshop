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
#'   # fetch all texts from the "British Fiction" source
#'   allTexts = get.texts()
#'   interestingTexts = allTexts %>%
#'     filter(source == 'British Fiction')
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
#'   secondarySet = write.corpus(interestingTexts, 'author', 'title', 'primary_set', 0.7)
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
  if(exists('.src', 1)){
    src = get('.src', 1)
  }else{
    stop('run db.connect() first')
  }
  if(is(data, 'tbl_sql')){
    data = collect(data)
  }
  
  toWrite = data
  if(length(groupBy1) == 0){
    toWrite$group1 = data$text_id 
  }else{
    toWrite$group1 = do.call('paste', data[, groupBy1])
  }
  toWrite$group1 = sub('_', '', toWrite$group1)
  
  if(length(groupBy2) == 0){
    toWrite$group2 = data$text_id 
  }else{
    toWrite$group2 = do.call('paste', data[, groupBy2])
  }
  toWrite$group2 = sub('_', '', toWrite$group2)
  
  toWrite = toWrite %>%
    select_('text_id', 'group1', 'group2', 'nchar')
  
  result = NULL
  if(sample < 1){
    toWrite = toWrite %>%
      group_by_('group1') %>%
      sample_frac(sample)
    result = data %>% 
      anti_join(toWrite %>% select_('text_id'))
  }

  write.title = function(data){
    data = data %>%
      summarize_(
        title = ~ paste(first(group1), first(group2), sep = '_'),
        text  = ~ paste(text, collapse = '\n')
      )
    path = paste(directory, paste0(sub('[/\\\\]', '_', data$title[1]), '.txt'), sep = '/')
    writeLines(data$text[1], path)
    return(1)
  }
  
  if(!dir.exists(directory)){
    dir.create(directory, recursive = TRUE)
  }
  
  toWrite %>% 
    group_by_('group1', 'group2') %>%
    mutate_(nchar = ~ cumsum(nchar)) %>%
    filter_(~ nchar < limit)
  
  texts = tbl(src, sql("SELECT text_id, text FROM texts")) %>%
    semi_join(data %>% select_('text_id'), copy = TRUE) %>%
    collect()
  
  toWrite %>%
    inner_join(texts) %>%
    do_(res = ~ write.title(.))
    
  return(result)
}
