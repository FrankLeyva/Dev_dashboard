# Dummy implementation of the thematic dashboard
# This simplified example focuses on structure and interactions

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)

# ===== SAMPLE DATA =====
# Creating dummy data that mimics your survey structure
set.seed(123)

# Generate 100 survey responses
n_responses <- 100
survey_data <- data.frame(
  # Demographics
  DISTRICT = sample(1:9, n_responses, replace = TRUE),
  GENDER = sample(c("Hombre", "Mujer"), n_responses, replace = TRUE),
  AGE_GROUP = sample(c("18-29", "30-44", "45-59", "60+"), n_responses, replace = TRUE),
  
  # Services Questions (Q29-Q55)
  Q29 = round(runif(n_responses, 5, 10), 1),  # Water satisfaction
  Q35 = round(runif(n_responses, 4, 10), 1),  # Electricity satisfaction
  Q40 = round(runif(n_responses, 5, 10), 1),  # Waste collection satisfaction
  Q46 = round(runif(n_responses, 3, 9), 1),   # Street lighting satisfaction
  Q51 = round(runif(n_responses, 2, 8), 1),   # Street quality satisfaction
  Q55 = round(runif(n_responses, 3, 9), 1),   # Traffic signals satisfaction
  
  # Mobility Questions (Q66, Q72, Q75)
  Q66 = sample(0:3, n_responses, replace = TRUE),  # Vehicles in household
  Q72_1 = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.7, 0.3)),  # Walking
  Q72_2 = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.9, 0.1)),  # Bicycle
  Q72_8 = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.4, 0.6)),  # Private vehicle
  Q72_9 = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.6, 0.4)),  # Public bus
  Q72_10 = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.8, 0.2)), # EcoBus
  Q75 = round(runif(n_responses, 3, 8), 1)  # Public transport satisfaction
)

# Create sample metadata
metadata <- data.frame(
  variable = c("Q29", "Q35", "Q40", "Q46", "Q51", "Q55", 
              "Q66", "Q72_1", "Q72_2", "Q72_8", "Q72_9", "Q72_10", "Q75"),
  label = c("Satisfacción con el servicio de agua", 
           "Satisfacción con el servicio de electricidad", 
           "Satisfacción con el servicio de recolección de basura", 
           "Satisfacción con el alumbrado público", 
           "Satisfacción con calles y pavimentación", 
           "Satisfacción con semáforos y señales viales",
           "Vehículos disponibles en el hogar",
           "Camina como medio de transporte",
           "Bicicleta como medio de transporte",
           "Vehículo propio como medio de transporte",
           "Camión/Rutera como medio de transporte",
           "EcoBus como medio de transporte",
           "Satisfacción con transporte público"),
  scale_type = c(rep("Intervalo", 6), "Razón", rep("Binaria", 5), "Intervalo"),
  theme = c(rep("services", 6), rep("mobility", 7))
)

# ===== HELPER FUNCTIONS =====

# Create a bar chart for service satisfaction
create_service_bar <- function(data, question_id, district_filter = NULL, 
                              gender_filter = NULL, age_filter = NULL) {
  # Apply filters if needed
  filtered_data <- data
  
  if (!is.null(district_filter) && length(district_filter) > 0) {
    filtered_data <- filtered_data %>% filter(DISTRICT %in% district_filter)
  }
  
  if (!is.null(gender_filter) && length(gender_filter) > 0) {
    filtered_data <- filtered_data %>% filter(GENDER %in% gender_filter)
  }
  
  if (!is.null(age_filter) && length(age_filter) > 0) {
    filtered_data <- filtered_data %>% filter(AGE_GROUP %in% age_filter)
  }
  
  # Get question metadata
  question_meta <- metadata %>% filter(variable == question_id)
  question_label <- question_meta$label
  
  # Calculate averages by district
  district_avg <- filtered_data %>%
    group_by(DISTRICT) %>%
    summarize(avg_rating = mean(get(question_id), na.rm = TRUE),
             count = n(),
             .groups = 'drop')
  
  # Create bar chart
  plot_ly(
    data = district_avg,
    x = ~as.factor(DISTRICT),
    y = ~avg_rating,
    type = "bar",
    text = ~paste0("Distrito ", DISTRICT, ": ", round(avg_rating, 1),
                 "<br>Respuestas: ", count),
    hoverinfo = "text",
    marker = list(color = "#4285f4")
  ) %>%
    layout(
      title = question_label,
      xaxis = list(title = "Distrito"),
      yaxis = list(title = "Calificación Promedio", range = c(0, 10))
    )
}

# Create a district map for service satisfaction
create_district_map <- function(data, question_id, district_filter = NULL, 
                              gender_filter = NULL, age_filter = NULL) {
  # Apply filters if needed
  filtered_data <- data
  
  if (!is.null(district_filter) && length(district_filter) > 0) {
    filtered_data <- filtered_data %>% filter(DISTRICT %in% district_filter)
  }
  
  if (!is.null(gender_filter) && length(gender_filter) > 0) {
    filtered_data <- filtered_data %>% filter(GENDER %in% gender_filter)
  }
  
  if (!is.null(age_filter) && length(age_filter) > 0) {
    filtered_data <- filtered_data %>% filter(AGE_GROUP %in% age_filter)
  }
  
  # Get question metadata
  question_meta <- metadata %>% filter(variable == question_id)
  question_label <- question_meta$label
  
  # Calculate averages by district
  district_avg <- filtered_data %>%
    group_by(DISTRICT) %>%
    summarize(avg_rating = mean(get(question_id), na.rm = TRUE),
             count = n(),
             .groups = 'drop')
  
  # Create a color palette
  pal <- colorNumeric(
    palette = "Blues",
    domain = c(0, 10)
  )
  
  # For this demo, we'll create a simple rectangular "map" for each district
  # In a real implementation, you would use actual GIS data
  
  # Create a grid layout for districts
  districts <- data.frame(
    DISTRICT = 1:9,
    lat = rep(c(31.75, 31.7, 31.65), each = 3),
    lng = rep(c(-106.5, -106.45, -106.4), 3)
  )
  
  # Join with our data
  districts <- districts %>%
    left_join(district_avg, by = "DISTRICT")
  
  # Create leaflet map
  leaflet(districts) %>%
    addTiles() %>%
    addCircleMarkers(
      ~lng, ~lat,
      radius = 20,
      color = "white",
      weight = 1,
      fillColor = ~pal(avg_rating),
      fillOpacity = 0.7,
      popup = ~paste0("<strong>Distrito ", DISTRICT, "</strong><br>",
                    "Calificación: ", round(avg_rating, 1), "/10<br>",
                    "Respuestas: ", count)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = c(0, 10),
      title = question_label,
      opacity = 0.7
    )
}

# Create transportation mode chart for mobility 
create_transport_chart <- function(data, district_filter = NULL, 
                                  gender_filter = NULL, age_filter = NULL) {
  # Apply filters if needed
  filtered_data <- data
  
  if (!is.null(district_filter) && length(district_filter) > 0) {
    filtered_data <- filtered_data %>% filter(DISTRICT %in% district_filter)
  }
  
  if (!is.null(gender_filter) && length(gender_filter) > 0) {
    filtered_data <- filtered_data %>% filter(GENDER %in% gender_filter)
  }
  
  if (!is.null(age_filter) && length(age_filter) > 0) {
    filtered_data <- filtered_data %>% filter(AGE_GROUP %in% age_filter)
  }
  
  # Calculate percentages for each transport mode
  transport_modes <- data.frame(
    Mode = c("Caminando", "Bicicleta", "Vehículo propio", "Camión/Rutera", "EcoBus"),
    Percentage = c(
      mean(filtered_data$Q72_1, na.rm = TRUE) * 100,
      mean(filtered_data$Q72_2, na.rm = TRUE) * 100,
      mean(filtered_data$Q72_8, na.rm = TRUE) * 100,
      mean(filtered_data$Q72_9, na.rm = TRUE) * 100,
      mean(filtered_data$Q72_10, na.rm = TRUE) * 100
    )
  )
  
  # Create bar chart
  plot_ly(
    data = transport_modes,
    x = ~reorder(Mode, -Percentage),
    y = ~Percentage,
    type = "bar",
    marker = list(color = c("#e67c73", "#f6bf26", "#4285f4", "#57bb8a", "#b6cff5"))
  ) %>%
    layout(
      title = "Modos de Transporte",
      xaxis = list(title = ""),
      yaxis = list(title = "Porcentaje (%)")
    )
}

# ===== UI DEFINITION =====
ui <- page_navbar(
  title = "Así Estamos Juárez 2024",
  theme = bs_theme(
    version = 5,
    bootswatch = "lumen",
    primary = "#4285f4",
    secondary = "#57bb8a"
  ),
  
  # Add custom CSS for styling
  header = tags$head(
    tags$style(HTML("
      /* Global Styles */
      body {
        font-family: 'Roboto', 'Helvetica Neue', Helvetica, Arial, sans-serif;
        color: #333333;
      }
      
      .theme-header {
        display: flex;
        align-items: center;
        margin-bottom: 30px;
        padding: 10px 0;
        border-bottom: 2px solid #eaeaea;
      }
      
      .theme-header h2 {
        margin: 0;
        font-size: 2rem;
        font-weight: 500;
      }
      
      .theme-header p {
        margin: 5px 0 0 0;
        color: #555;
        font-size: 1rem;
      }
      
      .filter-card {
        background-color: #f9f9f9;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      
      .metric-card {
        background-color: white;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        padding: 15px;
        margin-bottom: 20px;
        text-align: center;
      }
      
      .metric-card h3 {
        font-size: 2rem;
        margin: 5px 0;
        font-weight: 600;
      }
      
      .metric-card p {
        color: #777;
        margin: 0;
      }
      
      .nav-pills .nav-link.active {
        background-color: #4285f4;
      }
      
      .services-theme h2, .services-theme h3 {
        color: #e67c73;
      }
      
      .mobility-theme h2, .mobility-theme h3 {
        color: #4285f4;
      }
    "))
  ),
  
  # Overview Panel
  nav_panel(
    title = "Inicio",
    icon = icon("home"),
    
    fluidRow(
      column(
        width = 12,
        div(style = "text-align: center; padding: 30px 0;",
            h1("Así Estamos Juárez 2024"),
            h2("Encuesta de Percepción Ciudadana"),
            p("Explorando la calidad de vida y satisfacción ciudadana en Ciudad Juárez")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 3,
        div(class = "metric-card",
            h3("9.1"),
            p("Recolección de basura")
        )
      ),
      column(
        width = 3,
        div(class = "metric-card",
            h3("8.5"),
            p("Agua potable")
        )
      ),
      column(
        width = 3,
        div(class = "metric-card",
            h3("5.4"),
            p("Pavimentación")
        )
      ),
      column(
        width = 3,
        div(class = "metric-card",
            h3("52%"),
            p("Usa vehículo propio")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        h3("Navegación por Temas"),
        p("Seleccione un tema para explorar los resultados de la encuesta:")
      )
    ),
    
    fluidRow(
      column(
        width = 3,
        actionButton("goto_services", "Servicios Públicos", 
                   class = "btn-lg btn-block", 
                   style = "background-color: #e67c73; color: white; width: 100%;",
                   icon = icon("wrench"))
      ),
      column(
        width = 3,
        actionButton("goto_mobility", "Movilidad", 
                   class = "btn-lg btn-block", 
                   style = "background-color: #4285f4; color: white; width: 100%;",
                   icon = icon("car"))
      ),
      column(
        width = 3,
        actionButton("goto_health", "Salud", 
                   class = "btn-lg btn-block", 
                   style = "background-color: #57bb8a; color: white; width: 100%;",
                   icon = icon("heart-pulse"))
      ),
      column(
        width = 3,
        actionButton("goto_education", "Educación", 
                   class = "btn-lg btn-block", 
                   style = "background-color: #f6bf26; color: white; width: 100%;",
                   icon = icon("graduation-cap"))
      )
    )
  ),
  
  # Services Theme Panel
  nav_panel(
    title = "Servicios",
    icon = icon("wrench"),
    id = "services_panel",
    
    fluidRow(
      column(
        width = 12,
        div(class = "theme-header services-theme",
            icon("wrench", style = "font-size: 2rem; margin-right: 15px;"),
            div(
              h2("Servicios Públicos"),
              p("Evaluación de la satisfacción ciudadana con los servicios públicos esenciales")
            )
        )
      )
    ),
    
    # Tabs for different views of the Services theme
    navset_pill(
      nav_panel(
        title = "Panorama General",
        
        fluidRow(
          column(
            width = 4,
            div(style = "background-color: white; padding: 20px; border-radius: 5px; height: 100%;",
                h3("Calidad de los Servicios Públicos", class = "services-theme"),
                p("Los servicios públicos son fundamentales para la calidad de vida en Ciudad Juárez. La satisfacción de los ciudadanos con estos servicios refleja tanto la eficiencia gubernamental como el bienestar urbano."),
                p("Los resultados de la encuesta 2024 muestran variaciones importantes entre diferentes servicios, con mayor satisfacción en recolección de basura y electricidad, y menor satisfacción en pavimentación y transporte público."),
                
                h4("Servicio Mejor Evaluado"),
                div(class = "metric-card",
                    h3("9.1"),
                    p("Recolección de basura")
                ),
                
                h4("Servicio Peor Evaluado"),
                div(class = "metric-card",
                    h3("5.4"),
                    p("Calles y pavimentación")
                )
            )
          ),
          
          column(
            width = 8,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h4("Satisfacción con la Calidad de los Servicios Públicos"),
                plotlyOutput("services_overview_plot", height = "400px"),
                p(style = "font-size: 0.8rem; color: #777; text-align: right;", 
                 "Fuente: Encuesta de Percepción, Participación Ciudadana y Buen Gobierno, 2024.")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                h3("Distribución por Distrito", class = "services-theme"),
                selectInput("overview_service", "Seleccionar Servicio:",
                           choices = c("Agua potable" = "Q29", 
                                      "Electricidad" = "Q35", 
                                      "Recolección de basura" = "Q40",
                                      "Alumbrado público" = "Q46",
                                      "Calles y pavimentación" = "Q51"),
                           selected = "Q29"),
                leafletOutput("district_map", height = "500px")
            )
          )
        )
      ),
      
      nav_panel(
        title = "Explorador Detallado",
        
        fluidRow(
          column(
            width = 3,
            div(class = "filter-card",
                h4("Filtros"),
                
                selectInput("services_question", "Pregunta:",
                           choices = c("Agua potable" = "Q29", 
                                      "Electricidad" = "Q35", 
                                      "Recolección de basura" = "Q40",
                                      "Alumbrado público" = "Q46",
                                      "Calles y pavimentación" = "Q51")),
                
                selectInput("services_district", "Distrito:", 
                           choices = 1:9, multiple = TRUE),
                
                selectInput("services_gender", "Género:",
                           choices = c("Hombre", "Mujer"), multiple = TRUE),
                
                selectInput("services_age", "Grupo de Edad:",
                           choices = c("18-29", "30-44", "45-59", "60+"), multiple = TRUE),
                
                selectInput("services_viz", "Visualización:",
                           choices = c("Barras", "Mapa", "Tabla"))
            )
          ),
          
          column(
            width = 9,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                uiOutput("services_question_title"),
                uiOutput("services_visualization")
            )
          )
        )
      ),
      
      nav_panel(
        title = "Tendencias Históricas",
        
        fluidRow(
          column(
            width = 12,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h3("Evolución de la Satisfacción (2019-2024)", class = "services-theme"),
                plotlyOutput("services_trend_plot", height = "400px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                h3("Análisis de Tendencias", class = "services-theme"),
                p("La satisfacción con los servicios públicos ha mostrado distintas tendencias en los últimos años:"),
                tags$ul(
                  tags$li("El servicio de agua potable ha mantenido niveles estables, con un aumento significativo en 2023."),
                  tags$li("La recolección de basura ha mejorado consistentemente, alcanzando su punto máximo en 2024."),
                  tags$li("La satisfacción con las calles y pavimentación ha fluctuado, con una ligera tendencia a la baja.")
                )
            )
          )
        )
      ),
      
      nav_panel(
        title = "Datos y Metodología",
        
        fluidRow(
          column(
            width = 6,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h3("Preguntas Incluidas", class = "services-theme"),
                dataTableOutput("services_questions_table")
            )
          ),
          
          column(
            width = 6,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h3("Metodología", class = "services-theme"),
                p("La evaluación de servicios públicos se realizó mediante una escala del 1 al 10, donde:"),
                tags$ul(
                  tags$li("1 representa una satisfacción pésima"),
                  tags$li("10 representa una satisfacción excelente")
                ),
                p("Las preguntas fueron formuladas de la siguiente manera:"),
                p(em("\"En una escala del 1 al 10, ¿qué tan satisfecho está con [SERVICIO]?\""))
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                h3("Datos", class = "services-theme"),
                downloadButton("download_services_data", "Descargar Datos"),
                p(style = "margin-top: 10px;", "Descargue los datos completos de las preguntas sobre servicios públicos para realizar su propio análisis.")
            )
          )
        )
      )
    )
  ),
  
  # Mobility Theme Panel
  nav_panel(
    title = "Movilidad",
    icon = icon("car"),
    id = "mobility_panel",
    
    fluidRow(
      column(
        width = 12,
        div(class = "theme-header mobility-theme",
            icon("car", style = "font-size: 2rem; margin-right: 15px;"),
            div(
              h2("Movilidad"),
              p("Patrones de transporte y desplazamiento en Ciudad Juárez")
            )
        )
      )
    ),
    
    # Tabs for different views of the Mobility theme
    navset_pill(
      nav_panel(
        title = "Panorama General",
        
        fluidRow(
          column(
            width = 4,
            div(style = "background-color: white; padding: 20px; border-radius: 5px; height: 100%;",
                h3("Patrones de Movilidad", class = "mobility-theme"),
                p("Ciudad Juárez, como muchas ciudades fronterizas, enfrenta desafíos particulares en movilidad. El uso predominante del automóvil como principal medio de transporte genera impactos en la infraestructura vial y la calidad del aire."),
                p("Los resultados de la encuesta 2024 muestran una fuerte dependencia del vehículo privado, con oportunidades de mejora en el transporte público y la movilidad no motorizada."),
                
                h4("Modo Principal de Transporte"),
                div(class = "metric-card",
                    h3("52%"),
                    p("Vehículo propio")
                ),
                
                h4("Satisfacción con Transporte Público"),
                div(class = "metric-card",
                    h3("5.4/10"),
                    p("Calificación promedio")
                )
            )
          ),
          
          column(
            width = 8,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h4("Modos de Transporte Utilizados"),
                plotlyOutput("mobility_overview_plot", height = "400px"),
                p(style = "font-size: 0.8rem; color: #777; text-align: right;", 
                 "Fuente: Encuesta de Percepción, Participación Ciudadana y Buen Gobierno, 2024.")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                h3("Vehículos por Hogar", class = "mobility-theme"),
                plotlyOutput("vehicles_plot", height = "300px")
            )
          )
        )
      ),
      
      nav_panel(
        title = "Explorador Detallado",
        
        fluidRow(
          column(
            width = 3,
            div(class = "filter-card",
                h4("Filtros"),
                
                selectInput("mobility_question", "Aspecto:",
                           choices = c("Modos de transporte" = "transport_modes", 
                                      "Vehículos por hogar" = "Q66", 
                                      "Satisfacción con transporte público" = "Q75")),
                
                selectInput("mobility_district", "Distrito:", 
                           choices = 1:9, multiple = TRUE),
                
                selectInput("mobility_gender", "Género:",
                           choices = c("Hombre", "Mujer"), multiple = TRUE),
                
                selectInput("mobility_age", "Grupo de Edad:",
                           choices = c("18-29", "30-44", "45-59", "60+"), multiple = TRUE)
            )
          ),
          
          column(
            width = 9,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                uiOutput("mobility_question_title"),
                uiOutput("mobility_visualization")
            )
          )
        )
      ),
      
      nav_panel(
        title = "Análisis por Género",
        
        fluidRow(
          column(
            width = 12,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h3("Comparación de Patrones de Movilidad por Género", class = "mobility-theme"),
                plotlyOutput("mobility_gender_plot", height = "400px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                h3("Hallazgos", class = "mobility-theme"),
                p("El análisis por género revela diferencias significativas en los patrones de movilidad:"),
                tags$ul(
                  tags$li("Las mujeres utilizan más el transporte público que los hombres."),
                  tags$li("Los hombres reportan mayor uso de vehículo propio."),
                  tags$li("La percepción de seguridad en el transporte público varía significativamente por género.")
                )
            )
          )
        )
      ),
      
      nav_panel(
        title = "Datos y Metodología",
        
        fluidRow(
          column(
            width = 6,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h3("Preguntas Incluidas", class = "mobility-theme"),
                dataTableOutput("mobility_questions_table")
            )
          ),
          
          column(
            width = 6,
            div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                h3("Metodología", class = "mobility-theme"),
                p("Las preguntas sobre modos de transporte permiten selección múltiple, ya que muchos ciudadanos utilizan varios medios de transporte."),
                p("La satisfacción con el transporte público se evalúa en escala del 1 al 10, donde:"),
                tags$ul(
                  tags$li("1 representa una satisfacción pésima"),
                  tags$li("10 representa una satisfacción excelente")
                )
            )
          )
        )
      )
    )
  ),
  
  # Comparison Analysis Panel
  nav_panel(
    title = "Análisis Comparativo",
    icon = icon("chart-bar"),
    
    fluidRow(
      column(
        width = 3,
        div(class = "filter-card",
            h4("Selección de Variables"),
            
            selectInput("compare_theme1", "Tema 1:",
                       choices = c("Servicios Públicos" = "services", 
                                  "Movilidad" = "mobility")),
            
            selectInput("compare_question1", "Pregunta 1:",
                       choices = NULL),hr(),
            
                       selectInput("compare_theme2", "Tema 2:",
                                  choices = c("Servicios Públicos" = "services", 
                                             "Movilidad" = "mobility")),
                       
                       selectInput("compare_question2", "Pregunta 2:",
                                  choices = NULL),
                       
                       hr(),
                       
                       selectInput("compare_type", "Tipo de Comparación:",
                                  choices = c("Por Distrito" = "district", 
                                             "Por Género" = "gender", 
                                             "Por Edad" = "age")),
                       
                       actionButton("run_comparison", "Ejecutar Comparación", 
                                  class = "btn-primary", style = "width: 100%; margin-top: 15px;")
                   )
                 ),
                 
                 column(
                   width = 9,
                   div(style = "background-color: white; padding: 20px; border-radius: 5px;",
                       h3("Comparación de Variables"),
                       plotlyOutput("comparison_plot", height = "500px"),
                       
                       div(style = "background-color: #f5f9ff; padding: 15px; border-radius: 5px; margin-top: 20px; border-left: 4px solid #4285f4;",
                           h4("Hallazgos"),
                           htmlOutput("comparison_insights")
                       )
                   )
                 )
               )
             ),
             
             # Data Access Panel
             nav_panel(
               title = "Datos",
               icon = icon("database"),
               
               fluidRow(
                 column(
                   width = 12,
                   h2("Acceso a Datos"),
                   p("Explore y descargue los datos de la Encuesta de Percepción Ciudadana 2024."),
                   
                   div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                       h3("Explorador de Datos"),
                       
                       selectInput("data_theme", "Filtrar por Tema:",
                                  choices = c("Todos", "Servicios Públicos" = "services", 
                                             "Movilidad" = "mobility")),
                       
                       dataTableOutput("data_explorer")
                   )
                 )
               ),
               
               fluidRow(
                 column(
                   width = 4,
                   div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                       h3("Conjunto de Datos Completo"),
                       p("Descargue todos los datos de la encuesta en formato CSV:"),
                       downloadButton("download_all_data", "Descargar Datos Completos")
                   )
                 ),
                 
                 column(
                   width = 4,
                   div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                       h3("Datos por Tema"),
                       selectInput("download_theme", "Seleccionar Tema:",
                                  choices = c("Servicios Públicos" = "services", 
                                             "Movilidad" = "mobility")),
                       downloadButton("download_theme_data", "Descargar Datos del Tema")
                   )
                 ),
                 
                 column(
                   width = 4,
                   div(style = "background-color: white; padding: 20px; border-radius: 5px; margin-top: 20px;",
                       h3("Documentación"),
                       p("Descargue la documentación completa sobre la metodología y las variables:"),
                       downloadButton("download_documentation", "Descargar Documentación")
                   )
                 )
               )
             )
           )
           
           # ===== SERVER DEFINITION =====
           server <- function(input, output, session) {
             
             # === NAVIGATION HANDLERS ===
             # Handle navigation button clicks
             observeEvent(input$goto_services, {
               updateNavbarPage(session, "nav", selected = "services_panel")
             })
             
             observeEvent(input$goto_mobility, {
               updateNavbarPage(session, "nav", selected = "mobility_panel")
             })
             
             # === SERVICES THEME OUTPUTS ===
             # Services overview plot
             output$services_overview_plot <- renderPlotly({
               # Create dummy data for service ratings
               services <- data.frame(
                 Service = c("Recolección de basura", "Electricidad", "Agua potable", 
                            "Drenaje y alcantarillado", "Transporte público (BRT)",
                            "Alumbrado público", "Semáforos", "Disponibilidad de áreas verdes",
                            "Calles y pavimentación", "Transporte público (Ruteras)"),
                 Rating = c(9.1, 8.7, 8.5, 7.8, 7.8, 7.3, 7.1, 7.1, 5.6, 5.4),
                 Color = c("#e67c73", "#f6bf26", "#57bb8a", "#4285f4", "#b6cff5", 
                          "#e67c73", "#f6bf26", "#57bb8a", "#4285f4", "#b6cff5")
               )
               
               # Sort by rating
               services <- services[order(-services$Rating), ]
               
               # Create the plot
               plot_ly(
                 data = services,
                 x = ~Service,
                 y = ~Rating,
                 type = "bar",
                 marker = list(color = ~Color),
                 text = ~paste0(Service, ": ", Rating),
                 hoverinfo = "text"
               ) %>%
                 layout(
                   xaxis = list(title = "", tickangle = 45),
                   yaxis = list(title = "Calificación (1-10)", range = c(0, 10)),
                   margin = list(b = 120)
                 )
             })
             
             # District map for selected service
             output$district_map <- renderLeaflet({
               create_district_map(survey_data, input$overview_service)
             })
             
             # Services question title (explorer tab)
             output$services_question_title <- renderUI({
               question_id <- input$services_question
               question_meta <- metadata %>% filter(variable == question_id)
               
               h3(question_meta$label, class = "services-theme")
             })
             
             # Services visualization (explorer tab)
             output$services_visualization <- renderUI({
               viz_type <- input$services_viz
               question_id <- input$services_question
               
               if (viz_type == "Barras") {
                 plotlyOutput("services_bar_plot", height = "400px")
               } else if (viz_type == "Mapa") {
                 leafletOutput("services_map_plot", height = "400px")
               } else if (viz_type == "Tabla") {
                 dataTableOutput("services_data_table")
               }
             })
             
             # Render the selected visualization
             output$services_bar_plot <- renderPlotly({
               create_service_bar(survey_data, input$services_question, 
                                 input$services_district, input$services_gender, input$services_age)
             })
             
             output$services_map_plot <- renderLeaflet({
               create_district_map(survey_data, input$services_question, 
                                  input$services_district, input$services_gender, input$services_age)
             })
             
             output$services_data_table <- renderDataTable({
               # Filter data based on selections
               filtered_data <- survey_data
               
               if (length(input$services_district) > 0) {
                 filtered_data <- filtered_data %>% filter(DISTRICT %in% input$services_district)
               }
               
               if (length(input$services_gender) > 0) {
                 filtered_data <- filtered_data %>% filter(GENDER %in% input$services_gender)
               }
               
               if (length(input$services_age) > 0) {
                 filtered_data <- filtered_data %>% filter(AGE_GROUP %in% input$services_age)
               }
               
               # Get question ID and metadata
               question_id <- input$services_question
               question_meta <- metadata %>% filter(variable == question_id)
               
               # Create table
               result <- filtered_data %>%
                 group_by(DISTRICT) %>%
                 summarize(
                   "Promedio" = mean(get(question_id), na.rm = TRUE),
                   "Mínimo" = min(get(question_id), na.rm = TRUE),
                   "Máximo" = max(get(question_id), na.rm = TRUE),
                   "Respuestas" = n()
                 )
               
               datatable(
                 result,
                 options = list(
                   pageLength = 10,
                   language = list(
                     url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json'
                   )
                 )
               )
             })
             
             # Services historical trends plot
             output$services_trend_plot <- renderPlotly({
               # Create dummy historical data
               historical_data <- data.frame(
                 Year = rep(2019:2024, 3),
                 Service = rep(c("Agua potable", "Electricidad", "Recolección de basura"), each = 6),
                 Rating = c(
                   8.1, 8.2, 7.9, 7.9, 8.5, 8.5,  # Water
                   8.5, 8.6, 8.3, 8.4, 8.7, 8.7,  # Electricity
                   8.9, 8.8, 8.7, 8.8, 9.0, 9.1   # Waste collection
                 )
               )
               
               # Create the plot
               plot_ly() %>%
                 add_trace(
                   data = historical_data %>% filter(Service == "Agua potable"),
                   x = ~Year,
                   y = ~Rating,
                   type = "scatter",
                   mode = "lines+markers",
                   name = "Agua Potable",
                   line = list(color = "#57bb8a", width = 3)
                 ) %>%
                 add_trace(
                   data = historical_data %>% filter(Service == "Electricidad"),
                   x = ~Year,
                   y = ~Rating,
                   type = "scatter",
                   mode = "lines+markers",
                   name = "Electricidad",
                   line = list(color = "#f6bf26", width = 3)
                 ) %>%
                 add_trace(
                   data = historical_data %>% filter(Service == "Recolección de basura"),
                   x = ~Year,
                   y = ~Rating,
                   type = "scatter",
                   mode = "lines+markers",
                   name = "Recolección de Basura",
                   line = list(color = "#e67c73", width = 3)
                 ) %>%
                 layout(
                   xaxis = list(title = "Año"),
                   yaxis = list(title = "Calificación (1-10)", range = c(7.5, 9.5))
                 )
             })
             
             # Services questions table
             output$services_questions_table <- renderDataTable({
               services_meta <- metadata %>% 
                 filter(theme == "services") %>%
                 select(Variable = variable, Pregunta = label, Tipo = scale_type)
               
               datatable(
                 services_meta,
                 options = list(
                   pageLength = 10,
                   language = list(
                     url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json'
                   )
                 )
               )
             })
             
             # Services data download
             output$download_services_data <- downloadHandler(
               filename = function() {
                 paste("asi-estamos-juarez-2024-servicios-", Sys.Date(), ".csv", sep = "")
               },
               content = function(file) {
                 # Filter data for services theme
                 services_data <- survey_data %>%
                   select(DISTRICT, GENDER, AGE_GROUP, Q29, Q35, Q40, Q46, Q51, Q55)
                 
                 write.csv(services_data, file, row.names = FALSE)
               }
             )
             
             # === MOBILITY THEME OUTPUTS ===
             # Mobility overview plot
             output$mobility_overview_plot <- renderPlotly({
               create_transport_chart(survey_data)
             })
             
             # Vehicles per household plot
             output$vehicles_plot <- renderPlotly({
               # Calculate vehicle distribution
               vehicle_counts <- table(survey_data$Q66)
               vehicle_data <- data.frame(
                 Vehicles = c("0", "1", "2", "3+"),
                 Count = as.numeric(vehicle_counts),
                 Percentage = round(as.numeric(vehicle_counts) / sum(vehicle_counts) * 100, 1)
               )
               
               # Create the plot
               plot_ly(
                 data = vehicle_data,
                 x = ~Vehicles,
                 y = ~Percentage,
                 type = "bar",
                 marker = list(color = "#4285f4"),
                 text = ~paste0(Percentage, "%"),
                 textposition = "auto"
               ) %>%
                 layout(
                   title = "Vehículos por Hogar",
                   xaxis = list(title = "Número de Vehículos"),
                   yaxis = list(title = "Porcentaje (%)")
                 )
             })
             
             # Mobility question title (explorer tab)
             output$mobility_question_title <- renderUI({
               question_id <- input$mobility_question
               
               if (question_id == "transport_modes") {
                 title <- "Modos de Transporte"
               } else if (question_id == "Q66") {
                 title <- "Vehículos por Hogar"
               } else if (question_id == "Q75") {
                 title <- "Satisfacción con Transporte Público"
               }
               
               h3(title, class = "mobility-theme")
             })
             
             # Mobility visualization (explorer tab)
             output$mobility_visualization <- renderUI({
               question_id <- input$mobility_question
               
               if (question_id == "transport_modes") {
                 plotlyOutput("mobility_transport_plot", height = "400px")
               } else if (question_id == "Q66") {
                 plotlyOutput("mobility_vehicles_plot", height = "400px")
               } else if (question_id == "Q75") {
                 plotlyOutput("mobility_satisfaction_plot", height = "400px")
               }
             })
             
             # Render the selected mobility visualization
             output$mobility_transport_plot <- renderPlotly({
               create_transport_chart(survey_data, 
                                     input$mobility_district, 
                                     input$mobility_gender, 
                                     input$mobility_age)
             })
             
             output$mobility_vehicles_plot <- renderPlotly({
               # Filter data based on selections
               filtered_data <- survey_data
               
               if (length(input$mobility_district) > 0) {
                 filtered_data <- filtered_data %>% filter(DISTRICT %in% input$mobility_district)
               }
               
               if (length(input$mobility_gender) > 0) {
                 filtered_data <- filtered_data %>% filter(GENDER %in% input$mobility_gender)
               }
               
               if (length(input$mobility_age) > 0) {
                 filtered_data <- filtered_data %>% filter(AGE_GROUP %in% input$mobility_age)
               }
               
               # Calculate vehicle distribution
               vehicle_counts <- table(filtered_data$Q66)
               vehicle_data <- data.frame(
                 Vehicles = c("0", "1", "2", "3+"),
                 Count = as.numeric(vehicle_counts),
                 Percentage = round(as.numeric(vehicle_counts) / sum(vehicle_counts) * 100, 1)
               )
               
               # Create the plot
               plot_ly(
                 data = vehicle_data,
                 x = ~Vehicles,
                 y = ~Percentage,
                 type = "bar",
                 marker = list(color = "#4285f4"),
                 text = ~paste0(Percentage, "%"),
                 textposition = "auto"
               ) %>%
                 layout(
                   title = "Vehículos por Hogar",
                   xaxis = list(title = "Número de Vehículos"),
                   yaxis = list(title = "Porcentaje (%)")
                 )
             })
             
             output$mobility_satisfaction_plot <- renderPlotly({
               # Filter data based on selections
               filtered_data <- survey_data
               
               if (length(input$mobility_district) > 0) {
                 filtered_data <- filtered_data %>% filter(DISTRICT %in% input$mobility_district)
               }
               
               if (length(input$mobility_gender) > 0) {
                 filtered_data <- filtered_data %>% filter(GENDER %in% input$mobility_gender)
               }
               
               if (length(input$mobility_age) > 0) {
                 filtered_data <- filtered_data %>% filter(AGE_GROUP %in% input$mobility_age)
               }
               
               # Calculate satisfaction by district
               district_satisfaction <- filtered_data %>%
                 group_by(DISTRICT) %>%
                 summarize(
                   avg_satisfaction = mean(Q75, na.rm = TRUE),
                   count = n(),
                   .groups = 'drop'
                 )
               
               # Create the plot
               plot_ly(
                 data = district_satisfaction,
                 x = ~as.factor(DISTRICT),
                 y = ~avg_satisfaction,
                 type = "bar",
                 marker = list(color = "#4285f4"),
                 text = ~paste0("Distrito ", DISTRICT, ": ", round(avg_satisfaction, 1), "/10",
                               "<br>Respuestas: ", count),
                 hoverinfo = "text"
               ) %>%
                 layout(
                   title = "Satisfacción con Transporte Público por Distrito",
                   xaxis = list(title = "Distrito"),
                   yaxis = list(title = "Satisfacción Promedio (1-10)", range = c(0, 10))
                 )
             })
             
             # Mobility gender comparison plot
             output$mobility_gender_plot <- renderPlotly({
               # Calculate transport mode usage by gender
               gender_transport <- data.frame(
                 Mode = rep(c("Caminando", "Bicicleta", "Vehículo propio", "Transporte público"), 2),
                 Gender = rep(c("Hombre", "Mujer"), each = 4),
                 Percentage = c(
                   # Men
                   mean(survey_data$Q72_1[survey_data$GENDER == "Hombre"], na.rm = TRUE) * 100,
                   mean(survey_data$Q72_2[survey_data$GENDER == "Hombre"], na.rm = TRUE) * 100,
                   mean(survey_data$Q72_8[survey_data$GENDER == "Hombre"], na.rm = TRUE) * 100,
                   (mean(survey_data$Q72_9[survey_data$GENDER == "Hombre"], na.rm = TRUE) + 
                      mean(survey_data$Q72_10[survey_data$GENDER == "Hombre"], na.rm = TRUE)) * 100,
                   # Women
                   mean(survey_data$Q72_1[survey_data$GENDER == "Mujer"], na.rm = TRUE) * 100,
                   mean(survey_data$Q72_2[survey_data$GENDER == "Mujer"], na.rm = TRUE) * 100,
                   mean(survey_data$Q72_8[survey_data$GENDER == "Mujer"], na.rm = TRUE) * 100,
                   (mean(survey_data$Q72_9[survey_data$GENDER == "Mujer"], na.rm = TRUE) + 
                      mean(survey_data$Q72_10[survey_data$GENDER == "Mujer"], na.rm = TRUE)) * 100
                 )
               )
               
               # Create the plot
               plot_ly(
                 data = gender_transport,
                 x = ~Mode,
                 y = ~Percentage,
                 color = ~Gender,
                 colors = c("Hombre" = "#4285f4", "Mujer" = "#e67c73"),
                 type = "bar",
                 text = ~paste0(round(Percentage, 1), "%"),
                 textposition = "auto"
               ) %>%
                 layout(
                   title = "Modos de Transporte por Género",
                   xaxis = list(title = ""),
                   yaxis = list(title = "Porcentaje (%)"),
                   barmode = "group"
                 )
             })
             
             # Mobility questions table
             output$mobility_questions_table <- renderDataTable({
               mobility_meta <- metadata %>% 
                 filter(theme == "mobility") %>%
                 select(Variable = variable, Pregunta = label, Tipo = scale_type)
               
               datatable(
                 mobility_meta,
                 options = list(
                   pageLength = 10,
                   language = list(
                     url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json'
                   )
                 )
               )
             })
             
             # === COMPARISON ANALYSIS OUTPUTS ===
             # Update question choices based on selected theme
             observe({
               theme1 <- input$compare_theme1
               
               if (theme1 == "services") {
                 question_choices <- metadata %>%
                   filter(theme == "services") %>%
                   pull(label)
                 names(question_choices) <- metadata %>%
                   filter(theme == "services") %>%
                   pull(variable)
               } else if (theme1 == "mobility") {
                 question_choices <- c(
                   "Vehículos disponibles en el hogar" = "Q66",
                   "Satisfacción con transporte público" = "Q75"
                 )
               }
               
               updateSelectInput(session, "compare_question1", choices = question_choices)
             })
             
             observe({
               theme2 <- input$compare_theme2
               
               if (theme2 == "services") {
                 question_choices <- metadata %>%
                   filter(theme == "services") %>%
                   pull(label)
                 names(question_choices) <- metadata %>%
                   filter(theme == "services") %>%
                   pull(variable)
               } else if (theme2 == "mobility") {
                 question_choices <- c(
                   "Vehículos disponibles en el hogar" = "Q66",
                   "Satisfacción con transporte público" = "Q75"
                 )
               }
               
               updateSelectInput(session, "compare_question2", choices = question_choices)
             })
             
             # Generate comparison visualization and insights
             observeEvent(input$run_comparison, {
               # Get selected questions
               q1 <- input$compare_question1
               q2 <- input$compare_question2
               
               # Get question labels for titles
               q1_label <- metadata %>% filter(variable == q1) %>% pull(label)
               if (is.null(q1_label) || length(q1_label) == 0) {
                 if (q1 == "Q66") q1_label <- "Vehículos disponibles en el hogar"
                 if (q1 == "Q75") q1_label <- "Satisfacción con transporte público"
               }
               
               q2_label <- metadata %>% filter(variable == q2) %>% pull(label)
               if (is.null(q2_label) || length(q2_label) == 0) {
                 if (q2 == "Q66") q2_label <- "Vehículos disponibles en el hogar"
                 if (q2 == "Q75") q2_label <- "Satisfacción con transporte público"
               }
               
               # Create comparison based on selected type
               if (input$compare_type == "district") {
                 # District comparison
                 district_data <- survey_data %>%
                   group_by(DISTRICT) %>%
                   summarize(
                     q1_value = mean(get(q1), na.rm = TRUE),
                     q2_value = mean(get(q2), na.rm = TRUE),
                     .groups = 'drop'
                   )
                 
                 output$comparison_plot <- renderPlotly({
                   plot_ly() %>%
                     add_trace(
                       data = district_data,
                       x = ~as.factor(DISTRICT),
                       y = ~q1_value,
                       type = "bar",
                       name = q1_label,
                       marker = list(color = "#4285f4")
                     ) %>%
                     add_trace(
                       data = district_data,
                       x = ~as.factor(DISTRICT),
                       y = ~q2_value,
                       type = "bar",
                       name = q2_label,
                       marker = list(color = "#e67c73")
                     ) %>%
                     layout(
                       title = "Comparación por Distrito",
                       xaxis = list(title = "Distrito"),
                       yaxis = list(title = "Valor Promedio"),
                       barmode = "group"
                     )
                 })
                 
                 # Generate insights
                 output$comparison_insights <- renderUI({
                   # Calculate correlation
                   correlation <- cor(district_data$q1_value, district_data$q2_value, use = "complete.obs")
                   correlation_text <- if (abs(correlation) > 0.7) {
                     "fuerte"
                   } else if (abs(correlation) > 0.3) {
                     "moderada"
                   } else {
                     "débil"
                   }
                   
                   direction_text <- if (correlation > 0) {
                     "positiva"
                   } else {
                     "negativa"
                   }
                   
                   # Find districts with largest differences
                   district_data$diff <- abs(district_data$q1_value - district_data$q2_value)
                   max_diff_district <- district_data$DISTRICT[which.max(district_data$diff)]
                   
                   # Generate text
                   HTML(paste0(
                     "<p>Los datos muestran una correlación ", correlation_text, " ", direction_text, 
                     " (r = ", round(correlation, 2), ") entre estas variables a nivel de distrito.</p>",
                     
                     "<p>El Distrito ", max_diff_district, " muestra la mayor diferencia entre ambas variables, 
                     lo que podría indicar una relación particular en esta zona.</p>",
                     
                     "<p>Este análisis sugiere que ", 
                     if (correlation > 0.3) {
                       "existe una relación entre estas variables que merece mayor exploración."
                     } else {
                       "estas variables parecen comportarse de manera independiente."
                     },
                     "</p>"
                   ))
                 })
               } else if (input$compare_type == "gender") {
                 # Gender comparison
                 gender_data <- survey_data %>%
                   group_by(GENDER) %>%
                   summarize(
                     q1_value = mean(get(q1), na.rm = TRUE),
                     q2_value = mean(get(q2), na.rm = TRUE),
                     .groups = 'drop'
                   )
                 
                 output$comparison_plot <- renderPlotly({
                   plot_ly() %>%
                     add_trace(
                       data = gender_data,
                       x = ~GENDER,
                       y = ~q1_value,
                       type = "bar",
                       name = q1_label,
                       marker = list(color = "#4285f4")
                     ) %>%
                     add_trace(
                       data = gender_data,
                       x = ~GENDER,
                       y = ~q2_value,
                       type = "bar",
                       name = q2_label,
                       marker = list(color = "#e67c73")
                     ) %>%
                     layout(
                       title = "Comparación por Género",
                       xaxis = list(title = ""),
                       yaxis = list(title = "Valor Promedio"),
                       barmode = "group"
                     )
                 })
                 
                 # Generate gender-based insights
                 output$comparison_insights <- renderUI({
                   # Calculate differences
                   men_diff <- gender_data$q1_value[gender_data$GENDER == "Hombre"] - 
                              gender_data$q2_value[gender_data$GENDER == "Hombre"]
                   
                   women_diff <- gender_data$q1_value[gender_data$GENDER == "Mujer"] - 
                                gender_data$q2_value[gender_data$GENDER == "Mujer"]
                   
                   # Generate text
                   HTML(paste0(
                     "<p>Existen diferencias notables en cómo estas variables se comportan según el género:</p>",
                     
                     "<p><strong>Hombres:</strong> ", 
                     if (abs(men_diff) < 0.5) {
                       "Muestran valores similares en ambas variables."
                     } else if (men_diff > 0) {
                       paste0("Muestran valores más altos en ", q1_label, " que en ", q2_label, ".")
                     } else {
                       paste0("Muestran valores más altos en ", q2_label, " que en ", q1_label, ".")
                     },
                     "</p>",
                     
                     "<p><strong>Mujeres:</strong> ", 
                     if (abs(women_diff) < 0.5) {
                       "Muestran valores similares en ambas variables."
                     } else if (women_diff > 0) {
                       paste0("Muestran valores más altos en ", q1_label, " que en ", q2_label, ".")
                     } else {
                       paste0("Muestran valores más altos en ", q2_label, " que en ", q1_label, ".")
                     },
                     "</p>",
                     
                     "<p>Esta diferencia podría indicar distintas perspectivas o necesidades según el género.</p>"
                   ))
                 })
               } else if (input$compare_type == "age") {
                 # Age comparison
                 age_data <- survey_data %>%
                   group_by(AGE_GROUP) %>%
                   summarize(
                     q1_value = mean(get(q1), na.rm = TRUE),
                     q2_value = mean(get(q2), na.rm = TRUE),
                     .groups = 'drop'
                   )
                 
                 output$comparison_plot <- renderPlotly({
                   plot_ly() %>%
                     add_trace(
                       data = age_data,
                       x = ~AGE_GROUP,
                       y = ~q1_value,
                       type = "bar",
                       name = q1_label,
                       marker = list(color = "#4285f4")
                     ) %>%
                     add_trace(
                       data = age_data,
                       x = ~AGE_GROUP,
                       y = ~q2_value,
                       type = "bar",
                       name = q2_label,
                       marker = list(color = "#e67c73")
                     ) %>%
                     layout(
                       title = "Comparación por Grupo de Edad",
                       xaxis = list(title = ""),
                       yaxis = list(title = "Valor Promedio"),
                       barmode = "group"
                     )
                 })
                 
                 # Generate age-based insights
                 output$comparison_insights <- renderUI({
                   # Find age group with largest difference
                   age_data$diff <- abs(age_data$q1_value - age_data$q2_value)
                   max_diff_age <- age_data$AGE_GROUP[which.max(age_diff)]
        
                   # Identify trends across age groups
                   trend_direction <- if (all(diff(age_data$q1_value) > 0) || all(diff(age_data$q1_value) < 0)) {
                     "clara"
                   } else {
                     "no clara"
                   }
                   
                   # Generate text
                   HTML(paste0(
                     "<p>Al analizar las diferencias por grupo de edad:</p>",
                     
                     "<p>El grupo de ", max_diff_age, " muestra la mayor diferencia entre ambas variables, 
                     lo que podría indicar cómo las necesidades y percepciones cambian con la edad.</p>",
                     
                     "<p>Se observa una tendencia ", trend_direction, " en la relación de estas variables a través de los grupos de edad, ", 
                     if (trend_direction == "clara") {
                       "lo que sugiere un efecto consistente de la edad en estas percepciones."
                     } else {
                       "lo que sugiere que la edad tiene efectos complejos en estas percepciones."
                     },
                     "</p>"
                   ))
                 })
               }
             })
             
             # === DATA ACCESS OUTPUTS ===
             # Data explorer
             output$data_explorer <- renderDataTable({
               if (input$data_theme == "Todos") {
                 display_data <- survey_data
               } else if (input$data_theme == "services") {
                 display_data <- survey_data %>%
                   select(DISTRICT, GENDER, AGE_GROUP, Q29, Q35, Q40, Q46, Q51, Q55)
               } else if (input$data_theme == "mobility") {
                 display_data <- survey_data %>%
                   select(DISTRICT, GENDER, AGE_GROUP, Q66, Q72_1, Q72_2, Q72_8, Q72_9, Q72_10, Q75)
               }
               
               datatable(
                 display_data,
                 options = list(
                   pageLength = 10,
                   scrollX = TRUE,
                   language = list(
                     url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json'
                   )
                 )
               )
             })
             
             # Download handlers
             output$download_all_data <- downloadHandler(
               filename = function() {
                 paste("asi-estamos-juarez-2024-completo-", Sys.Date(), ".csv", sep = "")
               },
               content = function(file) {
                 write.csv(survey_data, file, row.names = FALSE)
               }
             )
             
             output$download_theme_data <- downloadHandler(
               filename = function() {
                 paste("asi-estamos-juarez-2024-", input$download_theme, "-", Sys.Date(), ".csv", sep = "")
               },
               content = function(file) {
                 if (input$download_theme == "services") {
                   theme_data <- survey_data %>%
                     select(DISTRICT, GENDER, AGE_GROUP, Q29, Q35, Q40, Q46, Q51, Q55)
                 } else if (input$download_theme == "mobility") {
                   theme_data <- survey_data %>%
                     select(DISTRICT, GENDER, AGE_GROUP, Q66, Q72_1, Q72_2, Q72_8, Q72_9, Q72_10, Q75)
                 }
                 
                 write.csv(theme_data, file, row.names = FALSE)
               }
             )
             
             output$download_documentation <- downloadHandler(
               filename = function() {
                 paste("asi-estamos-juarez-2024-documentacion-", Sys.Date(), ".csv", sep = "")
               },
               content = function(file) {
                 write.csv(metadata, file, row.names = FALSE)
               }
             )
           }
           
           # Run the application
           shinyApp(ui = ui, server = server)