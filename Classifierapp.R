# app.R
# Load required packages
if (!require(shiny)) install.packages("shiny")
if (!require(DT)) install.packages("DT")
if (!require(tidyverse)) install.packages("tidyverse")

library(shiny)
library(DT)
library(tidyverse)

# Define scale types and their descriptions in Spanish
scale_types <- c(
    "Binaria" = "Binaria",
    "Razón" = "Razón",
    "Intervalo" = "Intervalo",
    "Ordinal" = "Ordinal",
    "Categórica" = "Categórica",
    "Nominal (Abierta)" = "Nominal (Abierta)",
    "Necesita Clasificación" = "Necesita Clasificación"
)

# Define UI
ui <- fluidPage(
    titlePanel("Clasificación de Variables SPSS"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("metadata", "Cargar CSV de Metadatos",
                     accept = c(".csv")),
            fileInput("responses", "Cargar CSV de Respuestas",
                     accept = c(".csv")),
            downloadButton("downloadData", "Descargar Clasificaciones"),
            hr(),
            helpText("Tipos de Escalas:"),
            tags$ul(
                tags$li("Binaria: Respuestas Sí/No, Verdadero/Falso"),
                tags$li("Razón: Numérica con cero absoluto (edad, ingresos)"),
                tags$li("Intervalo: Numérica sin cero absoluto (temperatura, fechas)"),
                tags$li("Ordinal: Categorías ordenadas (niveles de satisfacción)"),
                tags$li("Categórica: Categorías sin orden"),
                tags$li("Nominal (Abierta): Respuestas de texto")
            ),
            hr(),
            conditionalPanel(
                condition = "input.reviewMode",
                selectInput("currentScale", "Filtrar por Tipo de Escala:",
                          choices = scale_types),
                actionButton("prevVar", "Variable Anterior"),
                actionButton("nextVar", "Siguiente Variable"),
                hr(),
                verbatimTextOutput("currentVarInfo")
            )
        ),
        
        mainPanel(
            checkboxInput("reviewMode", "Activar Modo de Revisión Manual", FALSE),
            
            conditionalPanel(
                condition = "!input.reviewMode",
                DTOutput("variableTable")
            ),
            
            conditionalPanel(
                condition = "input.reviewMode",
                fluidRow(
                    column(12,
                        h4("Revisión de Variables"),
                        verbatimTextOutput("variableDetails"),
                        h4("Ejemplos de Respuestas"),
                        DTOutput("responseSamples"),
                        selectInput("newScale", "Seleccionar Tipo de Escala:",
                                  choices = scale_types),
                        textAreaInput("notes", "Notas:", rows = 3),
                        actionButton("saveClassification", "Guardar Clasificación")
                    )
                )
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    # Reactive values
    classifications <- reactiveVal(NULL)
    responses_data <- reactiveVal(NULL)
    current_var_index <- reactiveVal(1)
    
    # Read responses data
    observeEvent(input$responses, {
        req(input$responses)
        responses <- read.csv(input$responses$datapath)
        responses_data(responses)
    })
    
    # Read and process metadata
    observeEvent(input$metadata, {
        req(input$metadata)
        
        data <- read.csv(input$metadata$datapath)
        
        # Initial automatic classification
        classified_data <- data %>%
            mutate(
                scale_type = case_when(
                    (str_detect(tolower(type), "haven_labelled") | has_value_labels) &
                        str_detect(tolower(value_labels), "sí|si|no|true|false|0,1") ~ "Binaria",
                    str_detect(tolower(type), "numeric|float") & 
                        str_detect(tolower(label), "escala|calificación") ~ "Intervalo",
                    str_detect(tolower(type), "numeric|float") ~ "Razón",
                    has_value_labels & 
                        str_detect(tolower(label), "nivel|grado|satisfacción|frecuencia") ~ "Ordinal",
                    has_value_labels ~ "Categórica",
                    str_detect(tolower(type), "string|texto|character") & !has_value_labels ~ "Nominal (Abierta)",
                    TRUE ~ "Necesita Clasificación"
                ),
                notes = ""
            )
        
        classifications(classified_data)
    })
    
    # Filtered data for review mode
    filtered_data <- reactive({
        req(classifications())
        req(input$currentScale)
        
        classifications() %>%
            filter(scale_type == input$currentScale)
    })
    
    # Current variable for review
    current_variable <- reactive({
        req(filtered_data())
        req(current_var_index())
        
        if (nrow(filtered_data()) == 0) return(NULL)
        
        idx <- min(current_var_index(), nrow(filtered_data()))
        filtered_data()[idx, ]
    })
    
    # Get sample responses for current variable
    sample_responses <- reactive({
        req(current_variable())
        req(responses_data())
        
        var_name <- current_variable()$variable
        if (var_name %in% names(responses_data())) {
            responses <- responses_data()[[var_name]]
            # Remove NA values and get unique responses
            unique_responses <- unique(responses[!is.na(responses)])
            # Take up to 10 samples
            sample_responses <- head(unique_responses, 10)
            data.frame(
                "Respuesta" = sample_responses,
                "Frecuencia" = sapply(sample_responses, function(x) sum(responses == x, na.rm = TRUE))
            )
        } else {
            data.frame("Respuesta" = "No hay datos disponibles", "Frecuencia" = NA)
        }
    })
    
    # Display sample responses
    output$responseSamples <- renderDT({
        req(sample_responses())
        datatable(
            sample_responses(),
            options = list(pageLength = 5),
            selection = "none"
        )
    })
    
    # Navigate through variables
    observeEvent(input$nextVar, {
        req(filtered_data())
        current_var_index(min(current_var_index() + 1, nrow(filtered_data())))
    })
    
    observeEvent(input$prevVar, {
        current_var_index(max(current_var_index() - 1, 1))
    })
    
    # Display current variable details
    output$variableDetails <- renderText({
        req(current_variable())
        var <- current_variable()
        
        paste0(
            "Variable: ", var$variable, "\n",
            "Etiqueta: ", var$label, "\n",
            "Tipo: ", var$type, "\n",
            "Escala Actual: ", var$scale_type, "\n",
            "Tiene Etiquetas de Valor: ", ifelse(var$has_value_labels, "Sí", "No"), "\n",
            "Etiquetas de Valor: ", var$value_labels
        )
    })
    
    # Save classification
    observeEvent(input$saveClassification, {
        req(current_variable())
        req(input$newScale)
        
        current_data <- classifications()
        var_name <- current_variable()$variable
        current_data$scale_type[current_data$variable == var_name] <- input$newScale
        current_data$notes[current_data$variable == var_name] <- input$notes
        
        classifications(current_data)
        
        if (current_var_index() < nrow(filtered_data())) {
            current_var_index(current_var_index() + 1)
        }
    })
    
    # Render interactive table
    output$variableTable <- renderDT({
        req(classifications())
        
        datatable(
            classifications(),
            editable = list(target = "cell", disable = list(columns = c(1:4))),
            options = list(pageLength = 25),
            selection = "none",
            colnames = c("Variable", "Etiqueta", "Tipo", "Tiene Etiquetas", 
                        "Etiquetas de Valor", "Tipo de Escala", "Notas")
        )
    })
    
    # Handle cell edits
    observeEvent(input$variableTable_cell_edit, {
        info <- input$variableTable_cell_edit
        classifications(editData(classifications(), info))
    })
    
    # Download handler
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("clasificacion_variables_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(classifications(), file, row.names = FALSE)
        }
    )
    
    # Display current variable info
    output$currentVarInfo <- renderText({
        req(current_variable())
        req(filtered_data())
        
        paste0(
            "Revisando variable ", current_var_index(), " de ", nrow(filtered_data()), "\n",
            "Actual: ", current_variable()$variable
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)