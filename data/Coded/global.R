# Create a list of visualization configurations
viz_config <- list(
  Q66 = list(
    section = "razon",
    title = "¿Cuántos vehículos de motor hay disponibles en su hogar?",
    filters = c("district", "gender", "age"),
    plots = list(
      bar = list(
        type = "bar",
        title = "Distribución de Respuestas",
        x = "Q66",
        y = "count"
      ),
      horizontal = list(
        type = "bar",
        orientation = "h",
        title = "Distribución Horizontal",
        x = "count",
        y = "Q66"
      ),
      pie = list(
        type = "pie",
        title = "Distribución Horizontal",
        values = "count",
        labels = "Q66"
      )


    )
  )
  # Add more questions following the same pattern
)

# Generate filter UI based on configuration
generate_filters <- function(question_id, available_filters) {
  filters <- list()
  
  if ("district" %in% available_filters) {
    filters$district <- checkboxGroupInput(
      paste0("district_", question_id),
      "Filtrar por Distrito:",
      choices = NULL
    )
  }
  
  if ("gender" %in% available_filters) {
    filters$gender <- checkboxGroupInput(
      paste0("gender_", question_id),
      "Filtrar por Género:",
      choices = NULL
    )
  }
  
  if ("age" %in% available_filters) {
    filters$age <- checkboxGroupInput(
      paste0("age_", question_id),
      "Filtrar por Edad:",
      choices = NULL
    )
  }
  
  return(filters)
}

# Generate card UI for a visualization
generate_viz_card <- function(question_id, viz_config) {
  card(
    card_body(
      layout_sidebar(
        sidebar = sidebar(
          generate_filters(question_id, viz_config[[question_id]]$filters)
        ),
        h4(viz_config[[question_id]]$title),
        verbatimTextOutput(paste0("summary_", question_id)),
        plotlyOutput(paste0("plot_", question_id))
      )
    )
  )
}