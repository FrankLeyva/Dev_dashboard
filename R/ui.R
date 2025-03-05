library(bslib)
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    primary = "#0d6efd"
  ),
  layout_columns(
    fill = FALSE,
    card(
      card_header("Configuración de Encuesta"),
      selectInput(
        "survey_selector",
        "Seleccionar Encuesta:",
        choices = c(
          "Percepción Ciudadana (PER 2023)" = "PER_2023",
          "Participación Ciudadana (PAR 2023)" = "PAR_2023"
        ),
        selected = "PER"
      )
    )
  ),
  navset_tab(
    nav_panel(
      title = "Vista General de Datos",
      icon = icon("database"),
      
      card(
        card_header("Información de la Encuesta Actual"),
        textOutput("survey_name"),
        uiOutput("survey_info")
      )
      ,
      
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
          ),
          div(
            style = "margin-top: 10px; margin-bottom: 20px;",
            h5("Pregunta:"),
            div(
              style = "font-style: italic; margin-left: 5px;",
              textOutput("question_label")
            )
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