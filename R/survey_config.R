survey_config <- list(
  PER_2023 = list(
    # For PER_2023, try to find columns based on prefixes rather than exact names
    district_col = "Q2",  # Actually "Q2Distrito electoral local"
    gender_col = "Q101",  # Actually "Q101Sexo de la persona entrevistada"
    age_col = "Q103",     # Actually "Q103Seleccione Rango de edad"
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 años",
      "2" = "30 a 44 años",
      "3" = "45 a 59 años",
      "4" = "60 años o más"
    )
  ),
  PER_2024 = list(
    district_col = "Q2",
    gender_col = "Q101",
    age_col = "Q103",
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 años",
      "2" = "30 a 44 años",
      "3" = "45 a 59 años",
      "4" = "60 años o más"
    )
  ),
  PAR_2023 = list(
    district_col = "Q2",  # Actually "Q2Distrito electoral local"
    gender_col = "Q144",  # Actually "Q144Sexo"
    age_col = "Q146",     # Actually "Q146Rango de edad"
    gender_mapping = c(
      "1" = "Hombre",
      "2" = "Mujer",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 años",
      "2" = "30 a 44 años",
      "3" = "45 a 59 años",
      "4" = "60 años o más"
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