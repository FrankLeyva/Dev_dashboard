load_survey_data <- function(survey_id = "PER_2023") {
  if (survey_id == "PAR_2023") {
    response_path <- "data/processed/PAR_2023_responses.csv"
    metadata_path <- "data/processed/PAR_2023_metadata_classified.csv"
  } else if (survey_id == "PER_2024") { 
    response_path <- "data/processed/PER_2024_responses.csv"
    metadata_path <- "data/processed/PER_2024_metadata_classified.csv"
  } else { 
    response_path <- "data/processed/PER_2023_responses.csv"
    metadata_path <- "data/processed/PER_2023_metadata_classified.csv"
  }
  
  # Load the data
  responses <- read.csv(response_path, encoding = "utf-8")
  metadata <- read.csv(metadata_path, encoding = "cp1252")
  
  # Fix column access by finding column names that start with the required prefix
  # This helps when working with columns like "Q2Distrito electoral local" instead of just "Q2"
  fix_column_names <- function(df) {
    # Create a mapping of simple column names to actual column names
    col_mapping <- list()
    for (col in names(df)) {
      # Extract the question number prefix (e.g., "Q2" from "Q2Distrito electoral local")
      prefix <- regmatches(col, regexpr("^Q\\d+", col))
      if (length(prefix) > 0 && prefix != "") {
        col_mapping[[prefix]] <- col
      }
    }
    
    # Add these mappings as an attribute to the dataframe for easy access
    attr(df, "col_mapping") <- col_mapping
    return(df)
  }
  
  # Apply the column name fix
  responses <- fix_column_names(responses)
  
  # Standardize the data
  std_responses <- standardize_survey_data(responses, survey_id)
  
  # Return the data
  list(
    responses = std_responses,
    raw_responses = responses,  # Keep original for reference
    metadata = metadata,
    survey_id = survey_id,
    config = survey_config[[survey_id]]  # Include config
  )
}

# Export to global environment explicitly
assign("load_geo_data", function(geo_data_path) {
  tryCatch({
    if (!file.exists(geo_data_path)) {
      stop(paste("File not found:", geo_data_path))
    }
    sf::st_read(geo_data_path, quiet = TRUE)
  }, error = function(e) {
    stop(paste("Error loading geographic data:", e$message))
  })
}, envir = .GlobalEnv)

standardize_survey_data <- function(responses, survey_id) {
  # Get config for this survey
  config <- survey_config[[survey_id]]
  
  # Create a copy of the data
  std_data <- responses
  
  # Get column mapping from attributes
  col_mapping <- attr(responses, "col_mapping")
  
  # Function to get the actual column name for a given question ID
  get_col_name <- function(q_id) {
    if (q_id %in% names(col_mapping)) {
      return(col_mapping[[q_id]])
    } else if (q_id %in% names(std_data)) {
      return(q_id)
    } else {
      message(paste("Warning: Column", q_id, "not found in the dataset"))
      return(NULL)
    }
  }
  
  # Get the actual column names for demographic columns
  district_col <- get_col_name(config$district_col)
  gender_col <- get_col_name(config$gender_col)
  age_col <- get_col_name(config$age_col)
  
  # Extract the demographic data if columns exist
  district_values <- if (!is.null(district_col)) std_data[[district_col]] else NA
  gender_values <- if (!is.null(gender_col)) std_data[[gender_col]] else NA
  age_values <- if (!is.null(age_col)) std_data[[age_col]] else NA
  
  # Apply mappings if provided
  if (!is.null(config$gender_mapping) && !is.null(gender_values)) {
    gender_values <- sapply(as.character(gender_values), function(val) {
      if (val %in% names(config$gender_mapping)) {
        return(config$gender_mapping[val])
      } else {
        return(val)
      }
    })
  }
  
  if (!is.null(config$age_mapping) && !is.null(age_values)) {
    age_values <- sapply(as.character(age_values), function(val) {
      if (val %in% names(config$age_mapping)) {
        return(config$age_mapping[val])
      } else {
        return(val)
      }
    })
  }
  
  # Add standardized columns to the dataset
  std_data$DISTRICT <- district_values
  std_data$GENDER <- gender_values
  std_data$AGE_GROUP <- age_values
  
  # Add metadata columns for reference
  std_data$SURVEY_ID <- survey_id
  
  # Transfer the column mapping attribute
  attr(std_data, "col_mapping") <- col_mapping
  
  return(std_data)
}