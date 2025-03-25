# Modified razon_module.R with theme customization support

prepare_razon_data <- function(data, question_id, metadata) {
  # This function remains unchanged
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
  # Unchanged helper function
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

# Helper functions - modified to accept custom_theme
calculate_district_means <- function(data) {
  # Unchanged calculation function
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
  # Unchanged calculation function
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
  # Unchanged calculation function
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

# Visualization functions - modified to accept custom_theme
create_histogram <- function(data, bins = 30, title = NULL, custom_theme = NULL) {
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
  
  # Use colors from custom theme if provided
  bar_color <- ifelse(!is.null(custom_theme), 
                      custom_theme$colors$primary, 
                      theme_config$colors$primary)
  
  line_color <- ifelse(!is.null(custom_theme), 
                       custom_theme$colors$neutral, 
                       theme_config$colors$neutral)
  
  plot_ly(
    data = data,
    x = ~value,
    type = "histogram",
    nbinsx = bins,
    marker = list(
      color = bar_color,
      line = list(
        color = line_color,
        width = 1
      )
    )
  ) %>%
    apply_plotly_theme(
      title = title,
      xlab = "Valor",
      ylab = "Frecuencia",
      custom_theme = custom_theme
    )
}

create_district_map <- function(data, geo_data, custom_theme = NULL) {
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
  
  # Create color palette for districts - use custom theme if provided
  district_palette <- if (!is.null(custom_theme)) {
    custom_theme$palettes$district
  } else {
    theme_config$palettes$district
  }
  
  pal <- colorNumeric(
    palette = district_palette,
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

create_ridge_plot <- function(data, title = NULL, custom_theme = NULL) {
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
  
  # Get numeric values for the plot
  plot_data <- data %>%
    mutate(
      numeric_value = get_numeric_values(.),
      district = factor(district, levels = rev(sort(unique(as.character(district)))))
    )
  
  # Use district palette from custom theme if provided
  district_colors <- if (!is.null(custom_theme)) {
    custom_theme$palettes$district
  } else {
    get_color_palette("district")
  }
  
  # Get distinct colors for median and mean
  median_color <- "#000000"  
  
  
  mean_color <-  "#000000"  
  
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = numeric_value, y = district, fill = district)) +
    # Base layer with density ridges - NO OUTLINE
    ggridges::geom_density_ridges(
      alpha = 0.7,
      scale = 0.9,
      rel_min_height = 0.01,
      color = NA,  # No outline color
      show.legend = FALSE
    ) +
    # Add median line on top (solid)
    ggridges::geom_density_ridges(
      aes(x = numeric_value, y = district),
      alpha = 0,  # Transparent fill
      scale = 0.9,
      rel_min_height = 0.01,
      quantile_lines = TRUE,
      quantiles = 2,  # 2 is the code for median
      color = median_color,
      size = 1.0,
      fill = NA,
      show.legend = FALSE
    ) +
    # Add mean line on top (dashed)
    ggridges::geom_density_ridges(
      aes(x = numeric_value, y = district),
      alpha = 0,  # Transparent fill
      scale = 0.9,
      rel_min_height = 0.01,
      quantile_lines = TRUE,
      quantile_fun = function(x,...) mean(x, na.rm = TRUE),
      linetype = "dashed",
      color = mean_color,
      size = 1.0,
      fill = NA,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = district_colors) +
    theme_minimal() +
    labs(
      title = title,
      x = "Valor",
      y = "Distrito"
    ) +
    theme(legend.position = "none")
  
  # Apply custom typography if available
  if (!is.null(custom_theme)) {
    p <- p + 
      theme(
        text = element_text(family = custom_theme$typography$font_family),
        plot.title = element_text(size = custom_theme$typography$sizes$title),
        axis.title = element_text(size = custom_theme$typography$sizes$axis),
        axis.text = element_text(size = custom_theme$typography$sizes$text)
      )
  }
  
  # Add a legend explaining the lines
  p <- p + 
    annotate("segment", x = min(plot_data$numeric_value, na.rm=TRUE), 
             xend = min(plot_data$numeric_value, na.rm=TRUE) + (max(plot_data$numeric_value, na.rm=TRUE) - min(plot_data$numeric_value, na.rm=TRUE))*0.1,
             y = min(as.numeric(plot_data$district), na.rm=TRUE) - 1.0, 
             yend = min(as.numeric(plot_data$district), na.rm=TRUE) - 1.0,
             color = median_color, size = 1.0) +
    annotate("text", x = min(plot_data$numeric_value, na.rm=TRUE) + (max(plot_data$numeric_value, na.rm=TRUE) - min(plot_data$numeric_value, na.rm=TRUE))*0.12, 
             y = min(as.numeric(plot_data$district), na.rm=TRUE) - 1.0, 
             label = "Mediana", hjust = 0, size = 3.5) +
    annotate("segment", x = min(plot_data$numeric_value, na.rm=TRUE), 
             xend = min(plot_data$numeric_value, na.rm=TRUE) + (max(plot_data$numeric_value, na.rm=TRUE) - min(plot_data$numeric_value, na.rm=TRUE))*0.1,
             y = min(as.numeric(plot_data$district), na.rm=TRUE) - 1.5, 
             yend = min(as.numeric(plot_data$district), na.rm=TRUE) - 1.5,
             color = mean_color, linetype = "dashed", size = 1.0) +
    annotate("text", x = min(plot_data$numeric_value, na.rm=TRUE) + (max(plot_data$numeric_value, na.rm=TRUE) - min(plot_data$numeric_value, na.rm=TRUE))*0.12, 
             y = min(as.numeric(plot_data$district), na.rm=TRUE) - 1.5, 
             label = "Media", hjust = 0, size = 3.5) +
    coord_cartesian(clip = 'off') +  # Allow drawing outside the plot area
    theme(plot.margin = margin(b = 40))  # Add extra margin at the bottom
  
  return(p)
}

# Modified UI function to accept the custom theme
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
          
          # Add download buttons for summary statistics
          conditionalPanel(
            condition = sprintf("input['%s'] == 'summary'", ns("plot_type")),
            div(
              style = "margin-top: 15px;",
              downloadButton(ns("download_summary_csv"), "Descargar Resumen (CSV)"),
              br(),
              br(),
              downloadButton(ns("download_summary_excel"), "Descargar Resumen (Excel)")
            )
          )
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

# Modified server function to accept the custom theme
razonServer <- function(id, data, selected_question, geo_data, metadata, current_theme = NULL) {
  moduleServer(id, function(input, output, session) {
     
    # Get the active theme (custom or default)
    active_theme <- reactive({
      if (is.function(current_theme)) {
        # If current_theme is a reactive function, call it to get the value
        current_theme()
      } else if (!is.null(current_theme)) {
        # If it's a direct value, use it
        current_theme
      } else {
        # Default to theme_config if nothing provided
        theme_config
      }
    })
    
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
        "bars" = plotlyOutput(session$ns("bar_plot")),
        "ridge_plot" = plotOutput(session$ns("ridge_plot"), height = "600px")
      )
    })
    
    # Enhanced Statistical Summary - unchanged
    output$summary_stats <- renderPrint({
      data <- filtered_data()
      
      # Create initial stats
      total_responses <- nrow(data)
      missing_values <- sum(is.na(data$value))
      valid_responses <- total_responses - missing_values
      
      cat("Estadísticas para Datos de Razón:\n")
      cat("\nDistribución de Respuestas:\n")
      cat("Total de respuestas:", total_responses, "\n")
      cat("Respuestas válidas:", valid_responses, "\n")
      cat("Datos faltantes:", missing_values, 
          sprintf("(%.1f%%)", 100 * missing_values/total_responses), "\n")
      
      # Overall statistics
      cat("\nEstadísticas Generales:\n")
      cat("Media:", round(mean(data$value, na.rm = TRUE), 2), "\n")
      cat("Mediana:", median(data$value, na.rm = TRUE), "\n")
      cat("Desviación Estándar:", round(sd(data$value, na.rm = TRUE), 2), "\n")
      cat("Mínimo:", min(data$value, na.rm = TRUE), "\n")
      cat("Máximo:", max(data$value, na.rm = TRUE), "\n")
      
      # Find mode
      mode_values <- find_mode(data$value)
      if (length(mode_values) > 0) {
        cat("Moda:", paste(mode_values, collapse = ", "), "\n")
      } else {
        cat("Moda: No definida\n")
      }
      
      # Frequency distribution
      freq_table <- table(data$value)
      if (length(freq_table) <= 30) {  # Only display if not too many values
        cat("\nDistribución de Frecuencias:\n")
        freq_df <- data.frame(
          Valor = names(freq_table),
          Frecuencia = as.vector(freq_table),
          Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2)
        )
        print(freq_df[order(-freq_df$Frecuencia), ])
      } else {
        cat("\nDistribución de Frecuencias: Demasiados valores únicos (", length(freq_table), ") para mostrar tabla.\n")
      }
      
      # Statistics by district
      cat("\nEstadísticas por Distrito:\n")
      district_stats <- data %>%
        group_by(district) %>%
        summarise(
          n = n(),
          Media = round(mean(value, na.rm = TRUE), 2),
          Mediana = median(value, na.rm = TRUE),
          DE = round(sd(value, na.rm = TRUE), 2),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE),
          .groups = 'drop'
        )
      print(district_stats)
      
      # Statistics by gender
      cat("\nEstadísticas por Género:\n")
      gender_stats <- data %>%
        group_by(gender) %>%
        summarise(
          n = n(),
          Media = round(mean(value, na.rm = TRUE), 2),
          Mediana = median(value, na.rm = TRUE),
          DE = round(sd(value, na.rm = TRUE), 2),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE),
          .groups = 'drop'
        )
      print(gender_stats)
      
      # Statistics by age group
      cat("\nEstadísticas por Grupo de Edad:\n")
      age_stats <- data %>%
        group_by(age_group) %>%
        summarise(
          n = n(),
          Media = round(mean(value, na.rm = TRUE), 2),
          Mediana = median(value, na.rm = TRUE),
          DE = round(sd(value, na.rm = TRUE), 2),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE),
          .groups = 'drop'
        )
      print(age_stats)
    })
    
    # Generate summary tables for download - unchanged
    summary_tables <- reactive({
      data <- filtered_data()
      
      # Create overall statistics table
      overall_stats <- data.frame(
        Statistic = c("Media", "Mediana", "Desviación Estándar", "Mínimo", "Máximo", "N"),
        Value = c(
          round(mean(data$value, na.rm = TRUE), 2),
          median(data$value, na.rm = TRUE),
          round(sd(data$value, na.rm = TRUE), 2),
          min(data$value, na.rm = TRUE),
          max(data$value, na.rm = TRUE),
          nrow(data)
        )
      )
      
      # Calculate district statistics
      district_stats <- data %>%
        group_by(district) %>%
        summarise(
          n = n(),
          Media = round(mean(value, na.rm = TRUE), 2),
          Mediana = median(value, na.rm = TRUE),
          DE = round(sd(value, na.rm = TRUE), 2),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Calculate gender statistics
      gender_stats <- data %>%
        group_by(gender) %>%
        summarise(
          n = n(),
          Media = round(mean(value, na.rm = TRUE), 2),
          Mediana = median(value, na.rm = TRUE),
          DE = round(sd(value, na.rm = TRUE), 2),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Calculate age group statistics
      age_stats <- data %>%
        group_by(age_group) %>%
        summarise(
          n = n(),
          Media = round(mean(value, na.rm = TRUE), 2),
          Mediana = median(value, na.rm = TRUE),
          DE = round(sd(value, na.rm = TRUE), 2),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Create frequency table
      freq_table <- as.data.frame(table(data$value))
      names(freq_table) <- c("Valor", "Frecuencia")
      freq_table$Porcentaje <- round(100 * freq_table$Frecuencia / sum(freq_table$Frecuencia), 2)
      
      return(list(
        overall = overall_stats,
        district = district_stats,
        gender = gender_stats,
        age = age_stats,
        frequency = freq_table
      ))
    })
    
    # CSV download handler - unchanged
    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("resumen_", selected_question(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        summaries <- summary_tables()
        
        # Create temporary directory for files
        temp_dir <- tempdir()
        
        # Write each summary to a CSV file
        write.csv(summaries$overall, file.path(temp_dir, "estadisticas_generales.csv"), row.names = FALSE)
        write.csv(summaries$district, file.path(temp_dir, "estadisticas_por_distrito.csv"), row.names = FALSE)
        write.csv(summaries$gender, file.path(temp_dir, "estadisticas_por_genero.csv"), row.names = FALSE)
        write.csv(summaries$age, file.path(temp_dir, "estadisticas_por_edad.csv"), row.names = FALSE)
        write.csv(summaries$frequency, file.path(temp_dir, "tabla_frecuencias.csv"), row.names = FALSE)
        
        # Create zip file with all CSVs
        files_to_zip <- c(
          file.path(temp_dir, "estadisticas_generales.csv"),
          file.path(temp_dir, "estadisticas_por_distrito.csv"),
          file.path(temp_dir, "estadisticas_por_genero.csv"),
          file.path(temp_dir, "estadisticas_por_edad.csv"),
          file.path(temp_dir, "tabla_frecuencias.csv")
        )
        
        zip(file, files_to_zip)
      }
    )
    
    # Excel download handler - unchanged
    output$download_summary_excel <- downloadHandler(
      filename = function() {
        paste0("resumen_", selected_question(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        summaries <- summary_tables()
        
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          # Fall back to csv if openxlsx is not available
          write.csv(summaries$overall, file)
          return()
        }
        
        # Create workbook and add worksheets
        wb <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(wb, "Estadísticas Generales")
        openxlsx::writeData(wb, "Estadísticas Generales", summaries$overall)
        
        openxlsx::addWorksheet(wb, "Por Distrito")
        openxlsx::writeData(wb, "Por Distrito", summaries$district)
        
        openxlsx::addWorksheet(wb, "Por Género")
        openxlsx::writeData(wb, "Por Género", summaries$gender)
        
        openxlsx::addWorksheet(wb, "Por Grupo de Edad")
        openxlsx::writeData(wb, "Por Grupo de Edad", summaries$age)
        
        openxlsx::addWorksheet(wb, "Tabla de Frecuencias")
        openxlsx::writeData(wb, "Tabla de Frecuencias", summaries$frequency)
        
        # Save workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Modified plot outputs that use custom theme
    output$histogram_plot <- renderPlotly({
      req(filtered_data())
      create_histogram(filtered_data(), custom_theme = active_theme())
    })
    
    output$ridge_plot <- renderPlot({
      req(filtered_data())
      create_ridge_plot(filtered_data(), custom_theme = active_theme())
    })

    output$age_bars_plot <- renderPlotly({
      age_stats <- calculate_age_distribution(filtered_data())
      
      # Use colors from active theme
      age_colors <- if (!is.null(active_theme())) {
        active_theme()$palettes$age_group
      } else {
        get_color_palette("age_group")
      }
      
      plot_ly(
        data = age_stats,
        x = ~age_group,
        y = ~mean_value,
        type = "bar",
        marker = list(
          color = age_colors
        )
      ) %>%
        apply_plotly_theme(
          title = "Promedio por Grupo de Edad",
          xlab = "Grupo de Edad",
          ylab = "Valor Promedio",
          custom_theme = active_theme()
        )
    })
    
    # Gender dumbbell plot - with custom theme
    output$gender_dumbbell_plot <- renderPlotly({
      gender_stats <- calculate_gender_district_stats(filtered_data())
      
      # Get custom gender colors if available
      gender_colors <- if (!is.null(active_theme())) {
        active_theme()$palettes$gender
      } else {
        get_color_palette("gender")
      }
      
      # Create traces for each gender
      p <- plot_ly() %>%
        add_trace(
          data = gender_stats,
          x = ~`Hombre`,
          y = ~district,
          name = "Hombre",
          type = "scatter",
          mode = "markers",
          marker = list(color = gender_colors[2])
        ) %>%
        add_trace(
          data = gender_stats,
          x = ~`Mujer`,
          y = ~district,
          name = "Mujer",
          type = "scatter",
          mode = "markers",
          marker = list(color = gender_colors[1])
        )
      
      # Get neutral color from theme
      neutral_color <- if (!is.null(active_theme())) {
        active_theme()$colors$neutral
      } else {
        theme_config$colors$neutral
      }
      
      # Add connecting lines
      for(i in 1:nrow(gender_stats)) {
        p <- add_segments(p,
          x = gender_stats$Hombre[i],
          xend = gender_stats$Mujer[i],
          y = gender_stats$district[i],
          yend = gender_stats$district[i],
          line = list(color = neutral_color),
          showlegend = FALSE
        )
      }
      
      p %>% apply_plotly_theme(
        title = "Comparación por Género y Distrito",
        xlab = "Valor Promedio",
        ylab = "Distrito",
        custom_theme = active_theme()
      ) %>%
        layout(showlegend = TRUE)
    })
    
    # Bar plot using the plot_functions from global_theme
    output$bar_plot <- renderPlotly({
      district_means <- calculate_district_means(filtered_data())
      
      # Use district colors from custom theme
      district_colors <- if (!is.null(active_theme())) {
        active_theme()$palettes$district
      } else {
        get_color_palette("district")
      }
      
      # Create plot directly instead of using plot_functions to handle custom theme
      if (input$bar_orientation == "h") {
        plot_ly(
          data = district_means,
          y = ~district,
          x = ~mean_value,
          type = "bar",
          orientation = 'h',
          marker = list(color = district_colors)
        ) %>%
          apply_plotly_theme(
            title = "Promedio por Distrito",
            xlab = "Valor Promedio",
            ylab = "Distrito",
            custom_theme = active_theme()
          )
      } else {
        plot_ly(
          data = district_means,
          x = ~district,
          y = ~mean_value,
          type = "bar",
          marker = list(color = district_colors)
        ) %>%
          apply_plotly_theme(
            title = "Promedio por Distrito",
            xlab = "Distrito",
            ylab = "Valor Promedio",
            custom_theme = active_theme()
          )
      }
    })
    
    output$district_map <- renderLeaflet({
      req(filtered_data(), geo_data())
      create_district_map(
        filtered_data(), 
        geo_data(), 
        custom_theme = active_theme()
      )
    })
  })
}