
load_survey_data <- function(survey_id = "PER_2023") {
  if (survey_id == "PAR_2023") {
    response_path <- "data/processed/PAR_2023_responses.csv"
    metadata_path <- "data/processed/PAR_2023_metadata_classified.csv"
  } else if (survey_id == "PER_2024_V1") { 
    response_path <- "data/processed/PER_2024_responses.csv"
    metadata_path <- "data/processed/PER_2024_metadata_classified.csv"
  } else if (survey_id == "PAR_2024_V1") {
    response_path <- "data/processed/PAR_2024_responses.csv"
    metadata_path <- "data/processed/PAR_2024_metadata_classified.csv"
  } else if (survey_id == "PAR_2024_V2") {
    response_path <- "data/processed/PAR_2024_responses_V3.csv"
    metadata_path <- "data/processed/PAR_2024_metadata_classified_V3.csv"
  } else if (survey_id == "PER_2024_V2") {
    response_path <- "data/processed/PER_2024_responses_V3.csv"
    metadata_path <- "data/processed/PER_2024_metadata_classified_V3.csv"
  } else { 
    response_path <- "data/processed/PER_2023_responses.csv"
    metadata_path <- "data/processed/PER_2023_metadata_classified.csv"
  }
  
  message("Loading survey data from: ", response_path)
  
  # Load the data
  responses <- read.csv(response_path, encoding = "utf-8")
  metadata <- read.csv(metadata_path, encoding = "cp1252")
  
  # Check if gender column exists
  if (survey_id == "PER_2023" && !"Q101" %in% names(responses)) {
    message("Q101 column not found in PER_2023. Available columns: ", paste(head(names(responses)), collapse=", "), "...")
  } else {
    message("Q101 column found in dataset")
  }
  
  # Standardize the data
  std_responses <- standardize_survey_data(responses, survey_id)
  
  # Verify standardized gender data
  gender_counts <- table(std_responses$GENDER, useNA = "ifany")
  message("Standardized gender distribution: ", paste(names(gender_counts), "=", gender_counts, collapse=", "))
  
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


# ENHANCED DATA LOADER CODE
# Replace or add this improved version of the standardize_survey_data function

standardize_survey_data <- function(responses, survey_id) {
  # Get config for this survey
  config <- survey_config[[survey_id]]
  
  # Create a copy of the data
  std_data <- responses
  
  # Extract the demographic columns and handle potential issues
  district_values <- tryCatch({
    std_data[[config$district_col]]
  }, error = function(e) {
    warning(paste("Error extracting district values from column", config$district_col, ":", e$message))
    rep(NA, nrow(std_data))
  })
  
  gender_values <- tryCatch({
    std_data[[config$gender_col]]
  }, error = function(e) {
    warning(paste("Error extracting gender values from column", config$gender_col, ":", e$message))
    rep(NA, nrow(std_data))
  })
  
  age_values <- tryCatch({
    std_data[[config$age_col]]
  }, error = function(e) {
    warning(paste("Error extracting age values from column", config$age_col, ":", e$message))
    rep(NA, nrow(std_data))
  })
  
  # Debug info - print sample of raw values
  message("Sample raw district values: ", paste(head(district_values, 5), collapse=", "))
  message("Sample raw gender values: ", paste(head(gender_values, 5), collapse=", "))
  message("Sample raw age values: ", paste(head(age_values, 5), collapse=", "))
  
  # Apply mappings if provided
  if (!is.null(config$gender_mapping) && !is.null(gender_values)) {
    gender_values <- sapply(as.character(gender_values), function(val) {
      if (val %in% names(config$gender_mapping)) {
        return(config$gender_mapping[val])
      } else {
        if (!is.na(val)) {
          warning(paste("No gender mapping found for value:", val))
        }
        return(val)
      }
    })
  } else {
    message("No gender mapping applied: mapping=", !is.null(config$gender_mapping), ", values=", !is.null(gender_values))
  }
  
  if (!is.null(config$age_mapping) && !is.null(age_values)) {
    age_values <- sapply(as.character(age_values), function(val) {
      if (val %in% names(config$age_mapping)) {
        return(config$age_mapping[val])
      } else {
        if (!is.na(val)) {
          warning(paste("No age mapping found for value:", val))
        }
        return(val)
      }
    })
  }
  
  # Debug info - print sample of mapped values
  message("Sample mapped gender values: ", paste(head(gender_values, 5), collapse=", "))
  
  # Add standardized columns to the dataset
  std_data$DISTRICT <- district_values
  std_data$GENDER <- gender_values
  std_data$AGE_GROUP <- age_values
  
  # Add metadata columns for reference
  std_data$SURVEY_ID <- survey_id
  
  return(std_data)
}

