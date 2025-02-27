prepare_razon_data <- function(data, question_id, metadata) {
  # Ensure required columns exist
  required_cols <- c(question_id, "Q2", "Q101", "Q103")
  
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns: ", 
         paste(required_cols[!required_cols %in% names(data)], collapse = ", "))
  }
  
  # Create dataset with only required columns
  subset_data <- data %>%
    select(
      value = all_of(question_id),
      district = Q2,
      gender = Q101,
      age_group = Q103
    ) %>%
    # Remove NA values
    filter(!is.na(value)) %>%
    # Convert to numeric if not already
    mutate(
      value = as.numeric(value),
      district = as.factor(district),
      gender = as.factor(gender),
      age_group = as.factor(age_group)
    )
    attr(subset_data, "question_label") <- get_question_label(question_id, metadata)

  return(subset_data)
}
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Add these helper functions at the top
calculate_district_means <- function(data) {
  data %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      .groups = 'drop'
    )
}

calculate_age_distribution <- function(data) {
  data %>%
    group_by(age_group) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(mean_value))
}

calculate_gender_district_stats <- function(data) {
  data %>%
    group_by(district, gender) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = gender,
      values_from = mean_value
    )
}
create_histogram <- function(data, bins = 30, title = NULL) {
  question_label <- attr(data, "question_label")
  # If no title provided, use the question label
  if (is.null(title)) {
    title <- paste("Distribución de", question_label)
  }
  plot_ly(
    data = data,
    x = ~value,
    type = "histogram",
    nbinsx = bins,
    marker = list(
      color = theme_config$colors$primary,
      line = list(
        color = theme_config$colors$neutral,
        width = 1
      )
    )
  )%>%
    apply_plotly_theme(
      title = title,
      xlab = "Valor",
      ylab = "Frecuencia"
    )
}
create_district_map <- function(data, geo_data) {
  req(data, geo_data)

  district_stats <- data %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Create color palette for districts
  pal <- colorNumeric(
    palette = theme_config$palettes$district,
    domain = district_stats$mean_value
  )
  
  # Create map
  leaflet(geo_data) %>%
    addTiles() %>% 
    addPolygons(,
      fillOpacity = 0.7,
      weight = 1,
      color = theme_config$palette$district[match(geo_data$No_Distrit, district_stats$district)],
      dashArray = "3",
      highlight = highlightOptions(
        weight = 2,
        color = theme_config$palette$district,
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "Distrito: %s<br>Promedio: %.2f<br>N: %d",
        district_stats$district[match(geo_data$No_Distrit, district_stats$district)],
        district_stats$mean_value[match(geo_data$No_Distrit, district_stats$district)],
        district_stats$n[match(geo_data$No_Distrit, district_stats$district)]
      ) %>% lapply(HTML)
    )
}




razonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4,
        accordion(
          accordion_panel("Controles de Visualización",
          selectInput(
            ns("plot_type"),
            "Tipo de Visualización",
            choices = c(
              "Resumen Estadístico" = "summary",
              "Histograma" = "histogram",
              "Mapa de Distritos" = "map",
              "Barras por Edad" = "age_bars",
              "Comparación por Género" = "gender_dumbbell",
              "Gráfico de Barras" = "bars"
            )
          )
        ),
          # Add filter controls
          accordion_panel("Filtros",
          selectInput(
            ns("district_filter"), 
            "Distritos",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            ns("gender_filter"),
            "Género",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            ns("age_filter"),
            "Grupo de Edad",
            choices = NULL,
            multiple = TRUE
          )
        ),
          accordion_panel(
            "Opciones Adicionales",
          conditionalPanel(
            condition = "input.plot_type == 'bars'",
            ns = ns,
            radioButtons(
              ns("bar_orientation"),
              "Orientación",
              choices = c(
                "Vertical" = "v",
                "Horizontal" = "h"
              )
            )
          ), 
        )
      )
    ),
      column(8,
        card(
          card_header("Visualización"),
          uiOutput(ns("plot_output"))
        )
      )
    )
  )
}

# R/razon_module.R

razonServer <- function(id, data, selected_question, geo_data, metadata) {
  moduleServer(id, function(input, output, session) {
     
    # Reactive dataset preparation
    prepared_data <- reactive({
      tryCatch({
        req(data(), selected_question(), metadata())
        
        # Add validation
        if (is.null(selected_question()) || selected_question() == "") {
          return(NULL)
        }
        
        prepare_razon_data(data(), selected_question(), metadata())
      }, error = function(e) {
        warning(paste("Error in prepared_data:", e$message))
        return(NULL)
      })
    })

    observe({
      req(prepared_data())
      
      updateSelectInput(session, "district_filter",
        choices = unique(prepared_data()$district),
        selected = character(0)
      )
      
      updateSelectInput(session, "gender_filter",
        choices = unique(prepared_data()$gender),
        selected = character(0)
      )
      
      updateSelectInput(session, "age_filter",
        choices = unique(prepared_data()$age_group),
        selected = character(0)
      )
    })
    filtered_data <- reactive({
      data <- prepared_data()
      
      if (length(input$district_filter) > 0) {
        data <- data %>% filter(district %in% input$district_filter)
      }
      
      if (length(input$gender_filter) > 0) {
        data <- data %>% filter(gender %in% input$gender_filter)
      }
      
      if (length(input$age_filter) > 0) {
        data <- data %>% filter(age_group %in% input$age_filter)
      }
      
      data
    })

    output$plot_output <- renderUI({
      plot_type <- input$plot_type
      
      switch(plot_type,
        "summary" = verbatimTextOutput(session$ns("summary_stats")),
        "histogram" = plotlyOutput(session$ns("histogram_plot")),
        "map" = leafletOutput(session$ns("district_map")),
        "age_bars" = plotlyOutput(session$ns("age_bars_plot")),
        "gender_dumbbell" = plotlyOutput(session$ns("gender_dumbbell_plot")),
        "bars" = plotlyOutput(session$ns("bar_plot"))
      )
    })
    
    # Statistical Summary
    output$summary_stats <- renderPrint({
      stats <- list(
        total_responses = length(filtered_data()$value),
        mode = find_mode(filtered_data()$value),
        mean = mean(filtered_data()$value, na.rm = TRUE),
        median = median(filtered_data()$value, na.rm = TRUE),
        sd = sd(filtered_data()$value, na.rm = TRUE),
        unique_categories = length(unique(filtered_data()$value)),
        missing = sum(is.na(filtered_data()$value))
      )
      
      cat("Estadísticas:\n")
      cat("Total de respuestas:", stats$total_responses, "\n")
      cat("Moda:", stats$mode, "\n")
      cat("Media:", round(stats$mean, 2), "\n")
      cat("Mediana:", stats$median, "\n")
      cat("Desviación Estándar:", round(stats$sd, 2), "\n")
      cat("Categorías únicas:", stats$unique_categories, "\n")
      cat("Datos faltantes:", stats$missing, "\n")
    })
    
    # Histogram
    output$histogram_plot <- renderPlotly({
      req(filtered_data())
      create_histogram(filtered_data(), bins = input$bins)
    })


     output$age_bars_plot <- renderPlotly({
      age_stats <- calculate_age_distribution(filtered_data())
      
      plot_ly(
        data = age_stats,
        x = ~age_group,
        y = ~mean_value,
        type = "bar",
        marker = list(
          color = get_color_palette("age_group")
        )
      ) %>%
        apply_plotly_theme(
          title = "Promedio por Grupo de Edad",
          xlab = "Grupo de Edad",
          ylab = "Valor Promedio"
        )
    })
    
    # Gender dumbbell plot
    output$gender_dumbbell_plot <- renderPlotly({
      gender_stats <- calculate_gender_district_stats(filtered_data())
      
      # Create traces for each gender
      p <- plot_ly() %>%
        add_trace(
          data = gender_stats,
          x = ~`Hombre`,
          y = ~district,
          name = "Hombre",
          type = "scatter",
          mode = "markers",
          marker = list(color = get_color_palette("gender")[2])
        ) %>%
        add_trace(
          data = gender_stats,
          x = ~`Mujer`,
          y = ~district,
          name = "Mujer",
          type = "scatter",
          mode = "markers",
          marker = list(color = get_color_palette("gender")[1])
        )
      
      # Add connecting lines
      for(i in 1:nrow(gender_stats)) {
        p <- add_segments(p,
          x = gender_stats$Hombre[i],
          xend = gender_stats$Mujer[i],
          y = gender_stats$district[i],
          yend = gender_stats$district[i],
          line = list(color = theme_config$colors$neutral),
          showlegend = FALSE
        )
      }
      
      p %>% apply_plotly_theme(
        title = "Comparación por Género y Distrito",
        xlab = "Valor Promedio",
        ylab = "Distrito"
      ) %>%
        layout(showlegend = TRUE)
    })
    
    # Bar plot using the plot_functions from global_theme
    output$bar_plot <- renderPlotly({
      district_means <- calculate_district_means(filtered_data())
      
      plot_functions$bar(
        data = district_means,
        x = "district",
        y = "mean_value",
        title = "Promedio por Distrito",
        xlab = "Distrito",
        ylab = "Valor Promedio",
        orientation = input$bar_orientation,
        color_by = 'district'
      )
    })
    output$district_map <- renderLeaflet({
      req(filtered_data(), geo_data())
      create_district_map(filtered_data(), geo_data())
    })
  })
}