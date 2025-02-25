# ui.R
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    primary = "#0d6efd"
  ),
  
  navset_tab(
    nav_panel(
      title = "Vista General de Datos",
      icon = icon("database"),
      
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Total de Respuestas",
          value = textOutput("total_responses"),
          showcase = icon("users")
        ),
        value_box(
          title = "Preguntas",
          value = textOutput("total_questions"),
          showcase = icon("question")
        )
      ),
      
      card(
        card_header("Resumen de Clasificación"),
        tableOutput("classification_summary")
      )
    ),
    
    nav_panel(
      title = "Clasificación de Preguntas",
      icon = icon("sitemap"),
      
      layout_sidebar(
        sidebar = sidebar(
          selectInput(
            "question_type",
            "Tipo de Pregunta",
            choices = c(
              "Razón" = "razon",
              "Intervalo" = "intervalo",
              "Ordinal" = "ordinal",
              "Categórico" = "categorico",
              "Binaria" = "binaria",
              "Nominal" = "nominal"
            )
          ),
          checkboxInput("show_metadata", "Mostrar Metadata", FALSE)
        ),
        card(
          card_header("Preguntas por Tipo"),
          DT::dataTableOutput("questions_by_type")
        )
      )
    ),
    
    nav_panel(
      title = "Prueba de Módulos",
      icon = icon("vial"),
      
      layout_sidebar(
        sidebar = sidebar(
          selectInput(
            "test_module",
            "Módulo a Probar",
            choices = c(
              "Razón" = "razon",
              "Intervalo" = "intervalo",
              "Ordinal" = "ordinal",
              "Categórico" = "categorico",
              "Binaria" = "binaria",
              "Nominal" = "nominal"
            )
          ),
          selectInput(
            "test_question",
            "Pregunta de Prueba",
            choices = NULL
          )
        ),
        conditionalPanel(
          condition = "input.test_module == 'razon'",
          razonUI("razon_test")
        ),
        conditionalPanel(
          condition = "input.test_module == 'intervalo'",
          intervalUI("interval_test")
        ),
        conditionalPanel(
          condition = "input.test_module == 'ordinal'",
          ordinalUI("ordinal_test")
        ),
        conditionalPanel(
          condition = "input.test_module == 'categorico'",
          categoricoUI("categorico_test")
        ),
        conditionalPanel(
          condition = "input.test_module == 'binaria'",
          binaryUI("binary_test")
        ),
        conditionalPanel(
          condition = "input.test_module == 'nominal'",
          nominalUI("nominal_test")
        )
      )
    )
  )
)