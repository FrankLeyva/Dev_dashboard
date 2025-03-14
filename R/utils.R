
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
get_column_from_question <- function(data, question_id) {
  # Get the column mapping from attributes
  col_mapping <- attr(data, "col_mapping")
  
  # Try to find column with exact name first
  if (question_id %in% names(data)) {
    return(question_id)
  }
  
  # Try to find column using the mapping
  if (!is.null(col_mapping) && question_id %in% names(col_mapping)) {
    return(col_mapping[[question_id]])
  }
  
  # Final fallback: look for any column that starts with the question_id
  for (col in names(data)) {
    if (startsWith(col, question_id)) {
      return(col)
    }
  }
  
  # If all else fails, return NULL
  return(NULL)
}

# Function to check if standardized columns exist and handle missing ones
check_standardized_columns <- function(data) {
  required_cols <- c("DISTRICT", "GENDER", "AGE_GROUP")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  
  if (length(missing_cols) > 0) {
    # Add dummy values for missing columns
    for (col in missing_cols) {
      data[[col]] <- NA
      warning(paste("Missing standardized column:", col, "- adding NA values"))
    }
  }
  
  return(data)
}

# Function to safely prepare numeric data
prepare_numeric_data <- function(data, value_col) {
  if (is.null(data) || is.null(value_col) || !value_col %in% names(data)) {
    return(NULL)
  }
  
  # Check and add standardized columns
  data <- check_standardized_columns(data)
  
  # Convert value column to numeric safely
  result <- data %>%
    mutate(
      value = suppressWarnings(as.numeric(as.character(!!sym(value_col)))),
      district = as.factor(DISTRICT),
      gender = as.factor(GENDER),
      age_group = as.factor(AGE_GROUP)
    ) %>%
    # Remove NA values after conversion
    filter(!is.na(value))
  
  return(result)
}