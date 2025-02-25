razon_config <- list(
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
      ))))




# First, let's create the helper function for generating filter UI
generate_filters <- function(question_id, available_filters) {
  filter_list <- list()
  
  if ("district" %in% available_filters) {
    filter_list$district <- checkboxGroupInput(
      paste0("district_", question_id),
      "Filtrar por Distrito:",
      choices = NULL
    )
  }
  
  if ("gender" %in% available_filters) {
    filter_list$gender <- checkboxGroupInput(
      paste0("gender_", question_id),
      "Filtrar por Género:",
      choices = NULL
    )
  }
  
  if ("age" %in% available_filters) {
    filter_list$age <- checkboxGroupInput(
      paste0("age_", question_id),
      "Filtrar por Edad:",
      choices = NULL
    )
  }
  
  return(filter_list)
}

# Then create the visualization card generator
generate_viz_card <- function(question_id, config) {
  card(
    card_body(
      layout_sidebar(
        sidebar = sidebar(
          do.call(tagList, generate_filters(question_id, config[[question_id]]$filters))
        ),
        h4(config[[question_id]]$title),
        verbatimTextOutput(paste0("summary_", question_id)),
        plotlyOutput(paste0("plot_", question_id))
      )
    )
  )
}

# Now the page layout
razon <- page_fillable(
  titlePanel("Razón"),
  
  layout_columns(
    col_widths = 12,
    # Introduction card
    card(
      p("Las preguntas de razón tienen una respuesta numérica en sus preguntas, los números son representaciones precisas de la respuesta del participante dejando poco a interpretación"),
      h3("Ejemplos"), 
      p("¿Cuántos vehículos de motor hay disponibles en su hogar?"),
      p("¿Cuántas bicicletas para niños hay disponibles en su hogar?"),
      h3("Posibles Filtros:"),
      p(HTML("<strong>Distrito</strong>, <strong>Grupo de Edad</strong>, <strong>Género</strong>"))
    ),
    
    # Visualization cards
    do.call(tagList, lapply(names(razon_config), function(question_id) {
      generate_viz_card(question_id, razon_config)
    }))
  )
)
