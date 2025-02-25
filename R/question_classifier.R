classify_questions <- function(metadata) {
  # Group questions by scale_type from metadata
  question_types <- list(
    razon = metadata$variable[metadata$scale_type == "Razon"],
    intervalo = metadata$variable[metadata$scale_type == "Intervalo"],
    ordinal = metadata$variable[metadata$scale_type == "Ordinal"],
    categorico = metadata$variable[metadata$scale_type == "Categorica"],
    binaria = metadata$variable[metadata$scale_type == "Binaria"],
    nominal = metadata$variable[metadata$scale_type == "Nominal (Abierta)"]
  )
  return(question_types)
}