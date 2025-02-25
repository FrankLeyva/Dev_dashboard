load_survey_data <- function(response_path, metadata_path) {
  responses <- read.csv(response_path, encoding = "utf-8")
  metadata <- read.csv(metadata_path, encoding = "cp1252")
  list(
    responses = responses,
    metadata = metadata
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

