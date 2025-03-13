load_survey_data <- function(survey_id = "PER_A_2023") {
  if (survey_id == "PAR_A_2023") {
    response_path <- "data/processed/PAR_A_2023_responses.csv"
    metadata_path <- "data/processed/PAR_A_2023_metadata_classified.csv"
  } else if (survey_id == "PAR_B_2023") { 
    response_path <- "data/processed/PAR_B_2023_responses.csv"
    metadata_path <- "data/processed/PAR_A_2023_metadata_classified.csv"
  } else if (survey_id == "PER_B_2023") { 
    response_path <- "data/processed/PER_B_2023_responses.csv"
    metadata_path <- "data/processed/PER_A_2023_metadata_classified.csv"
  } else if (survey_id == "PER_2024") { 
    response_path <- "data/processed/PER_2024_responses.csv"
    metadata_path <- "data/processed/PER_2024_metadata_classified.csv"
  } 
   else  { 
    response_path <- "data/processed/PER_A_2023_responses.csv"
    metadata_path <- "data/processed/PER_A_2023_metadata_classified.csv"
  }
  
  
  # Load the data
  responses <- read.csv(response_path, encoding = "utf-8")
  metadata <- read.csv(metadata_path, encoding = "cp1252")
  
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
  
  # Extract the demographic columns
  district_values <- std_data[[config$district_col]]
  gender_values <- std_data[[config$gender_col]]
  age_values <- std_data[[config$age_col]]
  
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
  
  return(std_data)
}