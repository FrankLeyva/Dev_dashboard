library(bslib)
library(shinyjs)
library(colourpicker) 
ui <- page_fluid(
  useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    primary = "#0d6efd"
  ),
  layout_columns(
    fill = FALSE,
    card(
      card_header("Configuración de Encuesta"),
      div(
        style = "display: flex; justify-content: space-between;",
        radioButtons(
          "survey_selector",
          "Seleccionar Encuesta:",
          choices = c(
          "Percepción Ciudadana 2023 (PER 2023)" = "PER_2023",
            "Participación Ciudadana 2023 (PAR 2023)" = "PAR_2023",
            "Percepción Ciudadana 2024 (PER 2024)" = "PER_2024",
            "Participación Ciudadana 2024 (PER 2024)" = "PAR_2024"
          ),
          selected = "PER_2024",
          inline = TRUE
        )
      ),
      tags$style(HTML("
        .radio-inline {
          width: 25%;
          margin-right: 0;
          text-align: center;
        }
        .shiny-options-group {
          width: 100%;
          display: flex;
          justify-content: space-between;
        }
      "))
    )
  ),
  navset_tab(
    id = "main_tabs",  

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
    ),
    nav_panel(
      title = "Búsqueda de Preguntas",
      icon = icon("search"),
      
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Buscar Preguntas en la Encuesta"),
          layout_columns(
            col_widths = c(9, 3),
            textInput(
              "global_search",
              "Buscar por texto en las preguntas:",
              placeholder = "Ingrese texto para buscar..."
            ),
            actionButton(
              "execute_search",
              "Buscar",
              icon = icon("search"),
              width = "100%",
              class = "btn-primary mt-4"
            )
          ),
          DT::dataTableOutput("search_results_table"),
          uiOutput("search_info")
        )
      )
    ),
    nav_panel(
      title = "Personalización de Tema",
      icon = icon("paint-brush"),
      
      layout_sidebar(
        sidebar = sidebar(
          h4("Configuración de Colores"),
          
          # Primary colors
          colourpicker::colourInput("primary_color", "Color Primario", value = theme_config$colors$primary),
          colourpicker::colourInput("secondary_color", "Color Secundario", value = theme_config$colors$secondary),
          colourpicker::colourInput("highlight_color", "Color de Resaltado", value = theme_config$colors$highlight),
          
          hr(),
          
          h4("Configuración de Paletas"),
          selectInput("district_palette", "Paleta de Distritos",
                     choices = c("Default", "Viridis", "Plasma", "Inferno", "Magma", "Blues", "Greens"),
                     selected = "Default"),
          
          selectInput("gender_palette", "Paleta de Género",
                     choices = c("Default", "Pastel", "Dark", "Set1", "Set2"),
                     selected = "Default"),
                     selectInput("age_palette", "Paleta de Grupos de Edad",
           choices = c("Default", "Pastel", "Dark", "Set1", "Set2"),
           selected = "Default"),
                     
          hr(),
          
          h4("Tipografía"),
          selectInput("font_family", "Fuente Principal", 
                      choices = c("Arial", "Helvetica", "Times New Roman", "Georgia", "Verdana"), 
                      selected = theme_config$typography$font_family),
          
          numericInput("title_size", "Tamaño de Título", 
                       value = theme_config$typography$sizes$title, min = 10, max = 24),
          
          numericInput("axis_size", "Tamaño de Ejes", 
                       value = theme_config$typography$sizes$axis, min = 8, max = 18),
          
          hr(),
          
          actionButton("reset_theme", "Restablecer Valores Predeterminados", 
                       class = "btn-warning", width = "100%")
        ),
        
        card(
          card_header("Vista Previa"),
          
          # Sample visualizations to preview theme changes
          plotlyOutput("theme_preview_plot", height = "300px"),
          
          hr(),
          
          plotlyOutput("theme_preview_district", height = "300px"),
          hr(),
          
          plotlyOutput("theme_preview_gender", height = "300px"),
          hr(),
          
          plotlyOutput("theme_preview_age", height = "300px"),
          
          card_footer(
            div(
              style = "display: flex; justify-content: space-between;",
              actionButton("save_theme", "Guardar Tema", class = "btn-primary"),
              downloadButton("download_theme", "Exportar Tema"),
              fileInput("upload_theme", "Importar Tema", accept = ".json")
            )
          )
        )
      )
    )
  )
)