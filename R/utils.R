
get_question_label <- function(question_id, metadata) {
  # Find the metadata entry for this question
  question_meta <- metadata %>%
    filter(variable == question_id) %>%
    first()
  
  if (!is.null(question_meta) && !is.na(question_meta$label)) {
    return(question_meta$label)
  } else {
    return(question_id)
  }
}