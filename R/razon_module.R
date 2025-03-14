prepare_razon_data <- function(data, question_id, metadata) {
  # Get the column mapping from attributes
  col_mapping <- attr(data, "col_mapping")
  
  # Function to get the actual column name for a given question ID
  get_col_name <- function(q_id) {
    if (!is.null(col_mapping) && q_id %in% names(col_mapping)) {
      return(col_mapping[[q_id]])
    } else if (q_id %in% names(data)) {
      return(q_id)
    } else {
      message(paste("Warning: Column", q_id, "not found in the dataset"))
      return(NULL)
    }
  }
  
  # Get the actual column name for the question
  actual_q_col <- get_col_name(question_id)
  
  if (is.null(actual_q_col)) {
    stop(paste("Question column", question_id, "not found in the dataset"))
  }
  
  # Check if required standardized columns exist
  if (!all(c("DISTRICT", "GENDER", "AGE_GROUP") %in% names(data))) {
    stop("Missing required standardized columns: DISTRICT, GENDER, AGE_GROUP")
  }
  
  # Create dataset with only required columns
  subset_data <- data %>%
    select(
      value = all_of(actual_q_col),
      district = DISTRICT,  
      gender = GENDER,      
      age_group = AGE_GROUP 
    ) %>%
    # Convert to numeric safely
    mutate(
      value = as.numeric(as.character(value))
    ) %>%
    # Remove NA values after conversion
    filter(!is.na(value)) %>%
    # Convert categorical variables to factors
    mutate(
      district = as.factor(district),
      gender = as.factor(gender),
      age_group = as.factor(age_group)
    )
    
  attr(subset_data, "question_label") <- get_question_label(question_id, metadata)
  
  return(subset_data)
}

find_mode <- function(x) {
  # Safely handle empty or all-NA input
  if(length(x) == 0 || all(is.na(x))) {
    return(NA)
  }
  
  # Remove NA values
  x <- x[!is.na(x)]
  
  # Return mode or NA if still empty
  if(length(x) == 0) {
    return(NA)
  }
  
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Add these helper functions at the top
calculate_district_means <- function(data) {
  # Handle empty dataframe
  if(nrow(data) == 0) {
    return(data.frame(
      district = character(),
      mean_value = numeric(),
      sd_value = numeric()
    ))
  }
  
  data %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      .groups = 'drop'
    )
}

calculate_age_distribution <- function(data) {
  # Handle empty dataframe
  if(nrow(data) == 0) {
    return(data.frame(
      age_group = character(),
      mean_value = numeric(),
      count = integer()
    ))
  }
  
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
  # Handle empty dataframe
  if(nrow(data) == 0) {
    return(data.frame(
      district = character(),
      Hombre = numeric(),
      Mujer = numeric()
    ))
  }
  
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
  # Check for empty data
  if(nrow(data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
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
  # Check inputs
  if(is.null(data) || nrow(data) == 0 || is.null(geo_data)) {
    return(leaflet() %>% 
             addTiles() %>%
             addControl("No hay datos suficientes para visualizar", position = "topright"))
  }

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
    addPolygons(
      fillOpacity = 0.7,
      weight = 1,
      color = ~pal(district_stats$mean_value[match(No_Distrit, district_stats$district)]),
      dashArray = "3",
      highlight = highlightOptions(
        weight = 2,
        color = "#666666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "Distrito: %s<br>Promedio: %.2f<br>N: %d",
        district_stats$district[match(No_Distrit, district_stats$district)],
        district_stats$mean_value[match(No_Distrit, district_stats$district)],
        district_stats$n[match(No_Distrit, district_stats$district)]
      ) %>% lapply(HTML)
    )
}


create_ridge_plot <- function(data, title = NULL) {
  # Check for empty data
  if(nrow(data) == 0) {
    return(ggplot() + 
             ggtitle("No hay datos suficientes para visualizar") +
             theme_minimal())
  }
  
  # Check if we have ggridges
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    return(ggplot() + 
             ggtitle("Package 'ggridges' is required for this visualization") +
             theme_minimal())
  }
  
  question_label <- attr(data, "question_label")
  # If no title provided, use the question label
  if (is.null(title)) {
    title <- paste("Distribución de", question_label, "por distrito")
  }
  
  # Create the plot
  p <- ggplot(data, aes(x = value, y = district, fill = district)) +
    ggridges::geom_density_ridges(
      quantile_lines = TRUE, 
      quantiles = 2,
      alpha = 0.7,
      scale = 0.9
    ) +
    scale_fill_manual(values = get_color_palette("district")) +
    theme_minimal() +
    labs(
      title = title,
      x = "Valor",
      y = "Distrito"
    ) +
    theme(legend.position = "none")
  
  return(p)
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
              "Gráfico de Barras" = "bars",
              "Gráfico de Crestas" = "ridge_plot"  
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


razonServer <- function(id, data, metadata, selected_question, geo_data) {
  moduleServer(id, function(input, output, session) {
     
    # Reactive dataset preparation
    prepared_data <- reactive({
      tryCatch({
        req(data(), selected_question(), metadata())
        
        # Add validation
        if (is.null(selected_question()) || selected_question() == "") {
          return(NULL)
        }
        
        result <- prepare_razon_data(data(), selected_question(), metadata())
        return(result)
      }, error = function(e) {
        warning(paste("Error in prepared_data:", e$message))
        return(NULL)
      })
    })

    observe({
      req(prepared_data())
      
      if (is.null(prepared_data()) || nrow(prepared_data()) == 0) {
        # If no data, just set empty choices
        updateSelectInput(session, "district_filter", choices = character(0))
        updateSelectInput(session, "gender_filter", choices = character(0))
        updateSelectInput(session, "age_filter", choices = character(0))
        return()
      }
      
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
      
      if (is.null(data) || nrow(data) == 0) {
        return(data)
      }
      
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
        "bars" = plotlyOutput(session$ns("bar_plot")),
        "ridge_plot" = plotOutput(session$ns("ridge_plot"), height = "600px")
      )
    })
    
    # Statistical Summary
    output$summary_stats <- renderPrint({
      data <- filtered_data()
      
      if (is.null(data) || nrow(data) == 0) {
        cat("No hay datos disponibles para visualizar.\n")
        return()
      }
      
      stats <- list(
        total_responses = length(data$value),
        mode = find_mode(data$value),
        mean = mean(data$value, na.rm = TRUE),
        median = median(data$value, na.rm = TRUE),
        sd = sd(data$value, na.rm = TRUE),
        unique_categories = length(unique(data$value)),
        missing = sum(is.na(data$value))
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
      create_histogram(filtered_data(), bins = 30)
    })
    
    output$ridge_plot <- renderPlot({
      req(filtered_data())
      create_ridge_plot(filtered_data())
    })

    output$age_bars_plot <- renderPlotly({
      req(filtered_data())
      
      if (nrow(filtered_data()) == 0) {
        return(plotly_empty() %>% 
                layout(title = "No hay datos suficientes para visualizar"))
      }
      
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
      req(filtered_data())
      
      if (nrow(filtered_data()) == 0) {
        return(plotly_empty() %>% 
                layout(title = "No hay datos suficientes para visualizar"))
      }
      
      gender_stats <- calculate_gender_district_stats(filtered_data())
      
      # Check if we have necessary columns
      if (!"Hombre" %in% names(gender_stats) || !"Mujer" %in% names(gender_stats)) {
        return(plotly_empty() %>% 
                layout(title = "Faltan datos de género para visualizar"))
      }
      
      # Create traces for each gender
      p <- plot_ly() %>%
        add_trace(
          data = gender_stats,
          x = ~Hombre,
          y = ~district,
          name = "Hombre",
          type = "scatter",
          mode = "markers",
          marker = list(color = get_color_palette("gender")[2])
        ) %>%
        add_trace(
          data = gender_stats,
          x = ~Mujer,
          y = ~district,
          name = "Mujer",
          type = "scatter",
          mode = "markers",
          marker = list(color = get_color_palette("gender")[1])
        )
      
      # Add connecting lines
      for(i in 1:nrow(gender_stats)) {
        if (!is.na(gender_stats$Hombre[i]) && !is.na(gender_stats$Mujer[i])) {
          p <- add_segments(p,
            x = gender_stats$Hombre[i],
            xend = gender_stats$Mujer[i],
            y = gender_stats$district[i],
            yend = gender_stats$district[i],
            line = list(color = theme_config$colors$neutral),
            showlegend = FALSE
          )
        }
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
      req(filtered_data())
      
      if (nrow(filtered_data()) == 0) {
        return(plotly_empty() %>% 
                layout(title = "No hay datos suficientes para visualizar"))
      }
      
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