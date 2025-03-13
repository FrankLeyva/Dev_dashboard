# Example implementation for the Services theme module
# This demonstrates how to structure the code for a thematic dashboard module

# UI Component for the Services Theme
servicesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Theme Header with Icon and Title
    fluidRow(
      column(
        width = 12,
        div(class = "theme-header",
            img(src = "icons/services_icon.svg", class = "theme-icon"),
            h2("Servicios Públicos"),
            p("Evaluación de la satisfacción ciudadana con los servicios públicos esenciales")
        )
      )
    ),
    
    # Tabs for different sections within the theme
    tabsetPanel(
      id = ns("services_sections"),
      type = "pills",
      
      # Overview Tab - Key findings and context
      tabPanel(
        "Panorama General",
        
        fluidRow(
          # Narrative Column
          column(
            width = 4,
            div(class = "narrative-panel",
                h3("Calidad de los Servicios Públicos"),
                p("Los servicios públicos son un elemento fundamental para la calidad de vida en Ciudad Juárez. La satisfacción de los ciudadanos con estos servicios refleja tanto la eficiencia gubernamental como el bienestar urbano."),
                p("Los resultados de la encuesta 2024 muestran variaciones importantes entre diferentes servicios, con mayor satisfacción en recolección de basura y menor en pavimentación y transporte público."),
                
                # Key Metrics Cards
                div(class = "metric-cards",
                    metricCardUI(ns("top_service"), "Servicio Mejor Evaluado"),
                    metricCardUI(ns("bottom_service"), "Servicio Peor Evaluado"),
                    metricCardUI(ns("overall_trend"), "Tendencia General")
                )
            )
          ),
          
          # Main Visualization Column
          column(
            width = 8,
            div(class = "main-viz-container",
                h4("Satisfacción con la Calidad de los Servicios Públicos"),
                # This will be populated by the server function
                plotlyOutput(ns("services_overview_plot"), height = "400px"),
                # Source and methodology note
                p(class = "source-note", "Fuente: Encuesta de Percepción, Participación Ciudadana y Buen Gobierno, Nov. 2024.")
            )
          )
        ),
        
        # Second row - District Comparison
        fluidRow(
          column(
            width = 12,
            h3("Comparación por Distritos"),
            div(class = "district-selector-container",
                selectInput(ns("overview_service"), "Seleccionar Servicio:",
                           choices = c("Agua potable", "Electricidad", "Recolección de basura", 
                                      "Alumbrado público", "Pavimentación", "Transporte público"))
            ),
            leafletOutput(ns("district_map"), height = "500px")
          )
        ),
        
        # Historical Trends
        fluidRow(
          column(
            width = 12,
            h3("Tendencias Históricas"),
            plotlyOutput(ns("historical_trends"), height = "300px")
          )
        )
      ),
      
      # Detailed Explorer Tab - For more in-depth analysis
      tabPanel(
        "Explorador Detallado",
        
        fluidRow(
          # Filter Panel
          column(
            width = 3,
            div(class = "filter-panel",
                h4("Filtros"),
                
                # Question selector
                selectInput(ns("question_selector"), "Pregunta:",
                           choices = NULL),  # Will be populated in server
                
                # Demographic filters
                selectInput(ns("district_filter"), "Distrito:", 
                           choices = NULL, multiple = TRUE),
                
                selectInput(ns("gender_filter"), "Género:",
                           choices = NULL, multiple = TRUE),
                
                selectInput(ns("age_filter"), "Grupo de Edad:",
                           choices = NULL, multiple = TRUE),
                
                # Visualization type selector (when applicable)
                conditionalPanel(
                  condition = "input.question_selector != 'overall_satisfaction'", 
                  ns = ns,
                  selectInput(ns("viz_type"), "Tipo de Visualización:",
                             choices = c("Barras", "Mapa", "Tabla"))
                ),
                
                # Action buttons
                actionButton(ns("reset_filters"), "Restablecer Filtros", 
                             icon = icon("refresh")),
                downloadButton(ns("download_data"), "Descargar Datos")
            )
          ),
          
          # Main visualization area
          column(
            width = 9,
            div(class = "viz-container",
                # Question title and description will be dynamically updated
                uiOutput(ns("question_title")),
                uiOutput(ns("question_description")),
                
                # The main visualization - updated based on selections
                uiOutput(ns("main_visualization")),
                
                # Insights panel - dynamically populated based on selection
                div(class = "insights-panel",
                    h4("Hallazgos Clave"),
                    uiOutput(ns("key_insights"))
                )
            )
          )
        )
      ),
      
      # Reports Tab - For specific reports or deep dives
      tabPanel(
        "Reportes Específicos",
        
        fluidRow(
          column(
            width = 12,
            h3("Reportes de Servicios Públicos"),
            p("Análisis detallados sobre aspectos específicos de los servicios públicos en Ciudad Juárez."),
            
            # Report cards
            div(class = "report-cards",
                reportCardUI(ns("water_report"), "Agua Potable", 
                            "Análisis de satisfacción, cobertura y calidad"),
                reportCardUI(ns("electricity_report"), "Electricidad", 
                            "Frecuencia de apagones y satisfacción"),
                reportCardUI(ns("waste_report"), "Recolección de Basura", 
                            "Cobertura, frecuencia y satisfacción"),
                reportCardUI(ns("lighting_report"), "Alumbrado Público", 
                            "Cobertura y percepción de seguridad"),
                reportCardUI(ns("streets_report"), "Calles y Pavimentación", 
                            "Estado de la infraestructura vial"),
                reportCardUI(ns("transport_report"), "Transporte Público", 
                            "Accesibilidad y calidad del servicio")
            )
          )
        )
      ),
      
      # Metadata Tab - For technical users
      tabPanel(
        "Metodología y Datos",
        
        fluidRow(
          column(
            width = 6,
            h3("Metodología"),
            p("Información detallada sobre el diseño de la encuesta, la metodología de muestreo y el análisis de datos."),
            
            h4("Preguntas Incluidas"),
            dataTableOutput(ns("questions_table")),
            
            h4("Consideraciones Metodológicas"),
            uiOutput(ns("methodology_notes"))
          ),
          
          column(
            width = 6,
            h3("Acceso a Datos"),
            p("Datos completos para análisis avanzados."),
            
            # Data preview
            h4("Vista Previa de Datos"),
            dataTableOutput(ns("data_preview")),
            
            # Download options
            div(class = "download-options",
                downloadButton(ns("download_full"), "Descargar Conjunto Completo"),
                downloadButton(ns("download_services"), "Descargar Solo Servicios"),
                downloadButton(ns("download_metadata"), "Descargar Metadatos")
            ),
            
            # API information (if applicable)
            h4("Información de API"),
            verbatimTextOutput(ns("api_info"))
          )
        )
      )
    )
  )
}

# Server function for the Services Theme
servicesServer <- function(id, data, metadata, geo_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Prepare the services data
    services_data <- reactive({
      # Filter the data to include only service-related questions
      # This is a simplified example - actual implementation would be more detailed
      service_questions <- c("Q29", "Q35", "Q40", "Q46", "Q51", "Q55")
      
      filtered_data <- data %>%
        select(all_of(c(service_questions, "DISTRICT", "GENDER", "AGE_GROUP")))
      
      return(filtered_data)
    })
    
    # Populate filter choices based on available data
    observe({
      updateSelectInput(session, "district_filter",
                        choices = unique(services_data()$DISTRICT))
      
      updateSelectInput(session, "gender_filter",
                        choices = unique(services_data()$GENDER))
      
      updateSelectInput(session, "age_filter",
                        choices = unique(services_data()$AGE_GROUP))
      
      # Populate question selector
      question_list <- list(
        "Satisfacción General" = "overall_satisfaction",
        "Agua Potable" = "Q29",
        "Electricidad" = "Q35",
        "Recolección de Basura" = "Q40",
        "Alumbrado Público" = "Q46",
        "Calles y Pavimentación" = "Q51",
        "Semáforos y Señalización" = "Q55"
      )
      
      updateSelectInput(session, "question_selector",
                        choices = question_list)
    })
    
    # Overview Tab Outputs
    
    # Key metrics
    output$top_service <- renderUI({
      # Logic to determine top-rated service
      # Simplified example
      div(
        h3("Recolección de Basura"),
        p("9.1/10"),
        span(class = "trend positive", "+0.2 vs 2023")
      )
    })
    
    output$bottom_service <- renderUI({
      # Logic to determine lowest-rated service
      div(
        h3("Pavimentación"),
        p("5.4/10"),
        span(class = "trend negative", "-0.3 vs 2023")
      )
    })
    
    output$overall_trend <- renderUI({
      # Overall trend calculation
      div(
        h3("Promedio General"),
        p("7.3/10"),
        span(class = "trend neutral", "+0.1 vs 2023")
      )
    })
    
    # Main overview plot
    output$services_overview_plot <- renderPlotly({
      # Create a bar chart similar to the one in the example on page 59
      service_means <- data.frame(
        Service = c("Recolección de basura", "Electricidad", "Agua potable", 
                   "Drenaje y alcantarillado", "Transporte público (BRT)",
                   "Alumbrado público", "Semáforos", "Disponibilidad de áreas verdes",
                   "Calles y pavimentación", "Transporte público (Ruteras)"),
        Rating = c(9.1, 8.7, 8.5, 7.8, 7.8, 7.3, 7.1, 7.1, 5.6, 5.4),
        Color = c("#e67c73", "#f6bf26", "#57bb8a", "#4285f4", "#b6cff5", 
                 "#e67c73", "#f6bf26", "#57bb8a", "#4285f4", "#b6cff5")
      )
      
      # Sort by rating
      service_means <- service_means[order(-service_means$Rating), ]
      
      # Create the plot
      plot_ly(
        data = service_means,
        x = ~Service,
        y = ~Rating,
        type = "bar",
        marker = list(color = ~Color),
        text = ~paste0(Service, ": ", Rating),
        hoverinfo = "text"
      ) %>%
        layout(
          title = "",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "Calificación (1-10)", range = c(0, 10)),
          margin = list(b = 120)
        )
    })
    
    # District map
    output$district_map <- renderLeaflet({
      # Get the selected service
      selected_service <- input$overview_service
      
      # Map the selection to the corresponding question
      question_mapping <- list(
        "Agua potable" = "Q29",
        "Electricidad" = "Q35",
        "Recolección de basura" = "Q40",
        "Alumbrado público" = "Q46",
        "Pavimentación" = "Q51",
        "Transporte público" = "Q74"  # Example - adjust as needed
      )
      
      question_id <- question_mapping[[selected_service]]
      
      # Calculate average by district
      district_avg <- services_data() %>%
        group_by(DISTRICT) %>%
        summarize(avg_rating = mean(get(question_id), na.rm = TRUE))
      
      # Create a color palette
      pal <- colorNumeric(
        palette = "Blues",
        domain = district_avg$avg_rating
      )
      
      # Create the map
      leaflet(geo_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(district_avg$avg_rating[match(No_Distrit, district_avg$DISTRICT)]),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = district_avg$avg_rating,
          position = "bottomright",
          title = "Satisfacción Promedio"
        )
    })
    
    # Historical trends
    output$historical_trends <- renderPlotly({
      # Example historical data
      historical_data <- data.frame(
        Year = c(2019, 2020, 2021, 2022, 2023, 2024),
        Water = c(8.1, 8.2, 7.9, 7.9, 8.5, 8.5),
        Electricity = c(8.5, 8.6, 8.3, 8.4, 8.7, 8.7),
        Waste = c(8.9, 8.8, 8.7, 8.8, 9.0, 9.1)
      )
      
      # Create the plot
      plot_ly() %>%
        add_trace(
          data = historical_data,
          x = ~Year,
          y = ~Water,
          type = "scatter",
          mode = "lines+markers",
          name = "Agua Potable",
          line = list(color = "#57bb8a")
        ) %>%
        add_trace(
          data = historical_data,
          x = ~Year,
          y = ~Electricity,
          type = "scatter",
          mode = "lines+markers",
          name = "Electricidad",
          line = list(color = "#f6bf26")
        ) %>%
        add_trace(
          data = historical_data,
          x = ~Year,
          y = ~Waste,
          type = "scatter",
          mode = "lines+markers",
          name = "Recolección de Basura",
          line = list(color = "#e67c73")
        ) %>%
        layout(
          title = "Evolución de la Satisfacción con Servicios Principales",
          xaxis = list(title = "Año"),
          yaxis = list(title = "Calificación (1-10)", range = c(5, 10))
        )
    })
    
    # Detailed Explorer Tab Outputs
    
    # Dynamic question title
    output$question_title <- renderUI({
      # Get the selected question
      question_id <- input$question_selector
      
      if (question_id == "overall_satisfaction") {
        title <- "Satisfacción General con Servicios Públicos"
      } else {
        # Look up the question text from metadata
        question_meta <- metadata %>%
          filter(variable == question_id) %>%
          first()
        
        title <- question_meta$label
      }
      
      h3(title)
    })
    
    # Dynamic question description
    output$question_description <- renderUI({
      # Get the selected question
      question_id <- input$question_selector
      
      # Description text (would be based on question metadata)
      if (question_id == "overall_satisfaction") {
        description <- "Visión general de la satisfacción ciudadana con todos los servicios públicos municipales."
      } else if (question_id == "Q29") {
        description <- "Evaluación de la satisfacción con el servicio de agua potable, considerando factores como disponibilidad, presión y calidad."
      } else if (question_id == "Q35") {
        description <- "Evaluación de la satisfacción con el servicio de electricidad, considerando factores como continuidad del servicio y atención a fallas."
      } else if (question_id == "Q40") {
        description <- "Evaluación de la satisfacción con el servicio de recolección de basura, considerando frecuencia y calidad del servicio."
      } else if (question_id == "Q46") {
        description <- "Evaluación de la satisfacción con el servicio de alumbrado público, considerando cobertura y funcionamiento."
      } else if (question_id == "Q51") {
        description <- "Evaluación de la satisfacción con el estado de calles y pavimentación, considerando la calidad y el mantenimiento."
      } else if (question_id == "Q55") {
        description <- "Evaluación de la satisfacción con semáforos y señalización vial, considerando funcionamiento y visibilidad."
      }
      
      p(description)
    })
    
    # Main visualization - dynamically updates based on selections
    output$main_visualization <- renderUI({
      question_id <- input$question_selector
      viz_type <- input$viz_type
      
      if (question_id == "overall_satisfaction") {
        # For overall satisfaction, always show the overview bar chart
        plotlyOutput(ns("overall_service_plot"), height = "500px")
      } else if (viz_type == "Barras") {
        plotlyOutput(ns("question_bar_plot"), height = "500px")
      } else if (viz_type == "Mapa") {
        leafletOutput(ns("question_map_plot"), height = "500px")
      } else if (viz_type == "Tabla") {
        dataTableOutput(ns("question_data_table"))
      }
    })
    
    # Bar plot for individual questions
    output$question_bar_plot <- renderPlotly({
      # Filter data based on selected filters
      filtered_data <- services_data()
      
      if (length(input$district_filter) > 0) {
        filtered_data <- filtered_data %>% 
          filter(DISTRICT %in% input$district_filter)
      }
      
      if (length(input$gender_filter) > 0) {
        filtered_data <- filtered_data %>% 
          filter(GENDER %in% input$gender_filter)
      }
      
      if (length(input$age_filter) > 0) {
        filtered_data <- filtered_data %>% 
          filter(AGE_GROUP %in% input$age_filter)
      }
      
      # Get the selected question
      question_id <- input$question_selector
      
      # Calculate averages by district
      district_avg <- filtered_data %>%
        group_by(DISTRICT) %>%
        summarize(avg_rating = mean(get(question_id), na.rm = TRUE),
                 count = n())
      
      # Create the plot
      plot_ly(
        data = district_avg,
        x = ~DISTRICT,
        y = ~avg_rating,
        type = "bar",
        text = ~paste0("Distrito ", DISTRICT, ": ", round(avg_rating, 1),
                      "<br>Respuestas: ", count),
        hoverinfo = "text",
        marker = list(color = "#4285f4")
      ) %>%
        layout(
          title = "",
          xaxis = list(title = "Distrito"),
          yaxis = list(title = "Calificación Promedio", range = c(0, 10))
        )
    })
    
    # Key insights panel - dynamically updates based on selection
    output$key_insights <- renderUI({
      # This would be based on actual analysis of the data
      # Simplified example
      question_id <- input$question_selector
      
      if (question_id == "Q29") {
        tagList(
          p("Los distritos 3, 5, 8, 9 y 10 muestran niveles de satisfacción superiores al promedio."),
          p("El distrito 2 reporta la satisfacción más baja con el servicio de agua (8.2/10)."),
          p("Se observa una tendencia positiva en la satisfacción desde 2019, con un incremento significativo en 2023.")
        )
      } else if (question_id == "Q51") {
        tagList(
          p("Calles y pavimentación es uno de los servicios peor evaluados por los ciudadanos."),
          p("Existe una brecha significativa entre distritos, con variaciones de hasta 2.5 puntos."),
          p("Los distritos con mayor reporte de baches muestran los niveles de satisfacción más bajos.")
        )
      } else {
        # Default insights
        p("Seleccione una pregunta para ver hallazgos específicos.")
      }
    })
    
    # Methodology and Data Tab Outputs
    
    # Questions table
    output$questions_table <- renderDataTable({
      # Table of questions included in this theme
      questions_df <- metadata %>%
        filter(variable %in% c("Q29", "Q35", "Q40", "Q46", "Q51", "Q55")) %>%
        select(Variable = variable, Pregunta = label, Tipo = scale_type)
      
      datatable(
        questions_df,
        options = list(
          pageLength = 10,
          dom = 'tp'
        )
      )
    })
    
    # Download handlers
    output$download_data <- downloadHandler(
      filename = function() {
        paste("servicios-publicos-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Filter data based on current selection
        filtered_data <- services_data()
        
        if (!is.null(input$question_selector) && input$question_selector != "overall_satisfaction") {
          selected_columns <- c(input$question_selector, "DISTRICT", "GENDER", "AGE_GROUP")
          filtered_data <- filtered_data %>% select(all_of(selected_columns))
        }
        
        if (length(input$district_filter) > 0) {
          filtered_data <- filtered_data %>% filter(DISTRICT %in% input$district_filter)
        }
        
        if (length(input$gender_filter) > 0) {
          filtered_data <- filtered_data %>% filter(GENDER %in% input$gender_filter)
        }
        
        if (length(input$age_filter) > 0) {
          filtered_data <- filtered_data %>% filter(AGE_GROUP %in% input$age_filter)
        }
        
        write.csv(filtered_data, file, row.names = FALSE)
      }
    )
  })
}

# Helper function for metric cards
metricCardUI <- function(id, title) {
  div(
    class = "metric-card",
    h4(title),
    uiOutput(id))
  }
  
  # Helper function for report cards 
  reportCardUI <- function(id, title, description) {
    div(
      class = "report-card",
      h4(title),
      p(description),
      actionButton(id, "Ver Reporte", class = "btn-sm")
    )
  }