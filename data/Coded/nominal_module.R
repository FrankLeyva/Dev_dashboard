
library(tidytext)
library(wordcloud2)

process_nominal_data <- function(data, question_id, metadata) {
  processed <- data %>%
    select(response = !!sym(question_id), 
           district = Q2, 
           gender = Q101, 
           age = Q103) %>%
    mutate(
      # Clean text responses
      response_clean = str_trim(tolower(response)),
      # Create tokens for word analysis
      response_tokens = map(response_clean, ~ unlist(str_split(., "\\s+")))
    )
  
  return(processed)
}

create_nominal_summary <- function(data) {
  # Word frequency analysis
  word_freq <- data %>%
    unnest(response_tokens) %>%
    count(word = response_tokens) %>%
    arrange(desc(n))
  
  # Response length statistics
  length_stats <- data %>%
    mutate(word_count = map_int(response_tokens, length)) %>%
    summarise(
      avg_length = mean(word_count),
      median_length = median(word_count),
      max_length = max(word_count)
    )
  
  paste0(
    "Análisis de Respuestas:\n",
    "Total de respuestas: ", nrow(data), "\n",
    "Promedio de palabras: ", round(length_stats$avg_length, 1), "\n",
    "Mediana de palabras: ", length_stats$median_length, "\n\n",
    "Palabras más frecuentes:\n",
    paste(
      head(word_freq$word, 10),
      head(word_freq$n, 10),
      sep = ": ",
      collapse = "\n"
    )
  )
}

create_nominal_viz <- function(data, viz_type, title = "") {
  switch(viz_type,
    "word_freq" = {
      word_freq <- data %>%
        unnest(response_tokens) %>%
        count(word = response_tokens) %>%
        arrange(desc(n))
      
      plot_ly(head(word_freq, 20)) %>%
        add_bars(x = ~reorder(word, n), y = ~n) %>%
        layout(
          title = title,
          xaxis = list(title = "Palabra", tickangle = 45),
          yaxis = list(title = "Frecuencia")
        )
    },
    "response_length" = {
      plot_data <- data %>%
        mutate(word_count = map_int(response_tokens, length))
      
      plot_ly(plot_data) %>%
        add_histogram(x = ~word_count, nbinsx = 30) %>%
        layout(
          title = title,
          xaxis = list(title = "Número de Palabras"),
          yaxis = list(title = "Frecuencia")
        )
    },
    "word_by_demo" = {
      plot_data <- data %>%
        unnest(response_tokens) %>%
        count(gender, age, word = response_tokens) %>%
        group_by(gender, age) %>%
        slice_max(n, n = 5) %>%
        ungroup()
      
      plot_ly(plot_data) %>%
        add_bars(x = ~word, y = ~n, color = ~gender,
                facet = ~age) %>%
        layout(
          title = title,
          xaxis = list(title = "Palabra", tickangle = 45),
          yaxis = list(title = "Frecuencia"),
          barmode = "group"
        )
    },
    "district_themes" = {
      # Get top word per district
      plot_data <- data %>%
        unnest(response_tokens) %>%
        count(district, word = response_tokens) %>%
        group_by(district) %>%
        slice_max(n, n = 1)
      
      map_data <- districts_sf %>%
        left_join(plot_data, by = c("No_Distrit" = "district"))
      
      pal <- colorFactor("Set3", domain = unique(plot_data$word))
      
      leaflet(map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(word),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = ~paste0(
            "Distrito: ", No_Distrit,
            "\nPalabra más común: ", word,
            "\nFrecuencia: ", n
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~word,
          title = "Palabra más común",
          position = "bottomright"
        )
    }
  )
}

nominalUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
        card(
          card_header("Filtros"),
          checkboxGroupInput(ns("district_filter"), "Distrito:", choices = NULL),
          checkboxGroupInput(ns("gender_filter"), "Género:", choices = NULL),
          checkboxGroupInput(ns("age_filter"), "Grupo de Edad:", choices = NULL)
        )
      ),
      column(9,
        card(
          card_header("Visualización"),
          radioButtons(ns("viz_type"), "Tipo de Análisis:",
                      choices = c(
                        "Frecuencia de Palabras" = "word_freq",
                        "Longitud de Respuestas" = "response_length",
                        "Palabras por Demografía" = "word_by_demo",
                        "Temas por Distrito" = "district_themes"
                      )),
          plotlyOutput(ns("plot"))
        ),
        card(
          card_header("Resumen del Análisis"),
          verbatimTextOutput(ns("summary"))
        )
      )
    )
  )
}

nominalServer <- function(id, data, question_id, metadata) {
  moduleServer(id, function(input, output, session) {
    
    processed_data <- reactive({
      process_nominal_data(data(), question_id, metadata)
    })
    
    filtered_data <- reactive({
      req(processed_data())
      current_data <- processed_data()
      
      if (!is.null(input$district_filter)) {
        current_data <- current_data %>% filter(district %in% input$district_filter)
      }
      if (!is.null(input$gender_filter)) {
        current_data <- current_data %>% filter(gender %in% input$gender_filter)
      }
      if (!is.null(input$age_filter)) {
        current_data <- current_data %>% filter(age %in% input$age_filter)
      }
      
      current_data
    })
    
    output$plot <- renderPlotly({
      req(filtered_data())
      question_label <- metadata %>%
        filter(variable == question_id) %>%
        pull(label)
      
      create_nominal_viz(filtered_data(), input$viz_type, title = question_label)
    })
    
    output$summary <- renderText({
      req(filtered_data())
      create_nominal_summary(filtered_data())
    })
    
    observe({
      updateCheckboxGroupInput(session, "district_filter",
                             choices = sort(unique(processed_data()$district)),
                             selected = sort(unique(processed_data()$district)))
      updateCheckboxGroupInput(session, "gender_filter",
                             choices = sort(unique(processed_data()$gender)),
                             selected = sort(unique(processed_data()$gender)))
      updateCheckboxGroupInput(session, "age_filter",
                             choices = sort(unique(processed_data()$age)),
                             selected = sort(unique(processed_data()$age)))
    })
  })
}