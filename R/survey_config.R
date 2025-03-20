
survey_config <- list(
  PER_2023 = list(
    district_col = "Q2",
    gender_col = "Q101",  # This is correct
    age_col = "Q103",
    # Added gender mapping that was missing
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    )
  ),
  PER_2024_V1 = list(
    district_col = "Q2",
    gender_col = "Q101",
    age_col = "Q103",
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    )
  ),
  PER_2024_V2 = list(
    district_col = "Q2",
    gender_col = "Q101",
    age_col = "Q103",
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    )
  ),
  PAR_2023 = list(
    district_col = "Q2",
    gender_col = "Q144",
    age_col = "Q146",
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 anos",
      "2" = "18 a 44 anos",
      "3" = "18 a 59 anos",
      "4" = "60 anos o mas"
    )
  ),PAR_2024_V1 = list(
    district_col = "Q2",
    gender_col = "Q142",
    age_col = "Q144",
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 anos",
      "2" = "18 a 44 anos",
      "3" = "18 a 59 anos",
      "4" = "60 anos o mas"
    )
  ),
  PAR_2024_V2 = list(
    district_col = "Q2",
    gender_col = "Q142",
    age_col = "Q144",
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 anos",
      "2" = "18 a 44 anos",
      "3" = "18 a 59 anos",
      "4" = "60 anos o mas"
    )
  ),
  binary_response_config = list(
    treat_na_as_negative_by_default = TRUE,
    question_exceptions = list(
      "PAR:Q5" = FALSE,   
      "PAR:Q88" = FALSE
    )
  )
)