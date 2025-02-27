prepare_interval_data <- function(data, question_id, metadata) {
  # Validate inputs
  if (is.null(question_id) || question_id == "") {
    return(NULL)
  }
  
  # Get metadata for this question
  question_metadata <- metadata %>%
    filter(variable == question_id) %>%
    first()
  
  if (is.null(question_metadata)) {
    return(NULL)
  }
  
  # Initialize value_labels and ns_nc_codes
  value_labels <- NULL
  ns_nc_codes <- NULL
  label_to_value_map <- NULL
  special_cases <- FALSE
  
  # Process value labels if they exist
  if (question_metadata$has_value_labels && !is.na(question_metadata$value_labels)) {
    tryCatch({
      # Split the labels string by semicolon
      label_pairs <- strsplit(question_metadata$value_labels, ";")[[1]]
      
      # Create mapping vectors
      values <- numeric()
      labels <- character()
      ns_nc_values <- numeric()
      
      # Check for special mixed case (numeric + text)
      if (any(grepl("No hay|No existe", label_pairs, ignore.case = TRUE))) {
        special_cases <- TRUE
      }
      
      # Process each pair
      for(pair in label_pairs) {
        # Split by equals and clean up
        parts <- strsplit(trimws(pair), "=")[[1]]
        if(length(parts) == 2) {
          value <- trimws(parts[1])
          label <- trimws(parts[2])
          
          # Check if this is a NS/NC type response
          if(grepl("NS/NC|No sabe|No responde|No contesta|No hay|No existe", label, ignore.case = TRUE)) {
            ns_nc_values <- c(ns_nc_values, as.numeric(value))
          }
          
          # For special case questions, if label is just a number, store it as is
          if (special_cases && label == value) {
            # Just a number like "1 = 1"
            values <- c(values, as.numeric(value))
            labels <- c(labels, label)
          } else {
            # Normal label like "1 = Muy satisfecho"
            values <- c(values, as.numeric(value))
            labels <- c(labels, label)
          }
        }
      }
      
      # Create mappings
      value_labels <- setNames(labels, values)
      label_to_value_map <- setNames(values, labels)
      ns_nc_codes <- ns_nc_values
      
    }, error = function(e) {
      warning(paste("Error processing value labels for", question_id, ":", e$message))
    })
  }
  
  # Select relevant columns
  subset_data <- data %>%
    select(
      value = all_of(question_id),
      district = Q2,
      gender = Q101,
      age_group = Q103
    )
  
  # Handle NA values
  subset_data$value_original <- subset_data$value
  subset_data$is_na <- is.na(subset_data$value)
  
  # For special case questions (like Q56), convert data
  if (special_cases) {
    # Try to convert values to numeric, NAs will be for text responses
    subset_data$value_num <- suppressWarnings(as.numeric(subset_data$value))
    
    # Mark NS/NC values based on metadata
    subset_data$response_type <- case_when(
      subset_data$is_na ~ "MISSING",
      is.na(subset_data$value_num) | subset_data$value_num %in% ns_nc_codes ~ "NS/NC",
      TRUE ~ "VALID"
    )
  } else {
    # Try to determine if values are numeric or text
    is_numeric_data <- tryCatch({
      # Try converting first non-NA value to numeric
      values <- subset_data$value[!is.na(subset_data$value)]
      if (length(values) > 0) {
        !is.na(as.numeric(values[1]))
      } else {
        TRUE  # Default to numeric if no data
      }
    }, warning = function(w) {
      FALSE  # If warning, assume text data
    }, error = function(e) {
      TRUE   # Default to numeric on other errors
    })
    
    if (!is_numeric_data && !is.null(label_to_value_map)) {
      # For text values that need mapping to numeric
      subset_data$response_type <- case_when(
        subset_data$is_na ~ "MISSING",
        subset_data$value %in% names(label_to_value_map)[label_to_value_map %in% ns_nc_codes] ~ "NS/NC",
        !subset_data$value %in% names(label_to_value_map) ~ "INVALID",
        TRUE ~ "VALID"
      )
      
      # Map to numeric values
      subset_data$value_num <- sapply(1:nrow(subset_data), function(i) {
        if (subset_data$response_type[i] == "VALID") {
          as.numeric(label_to_value_map[subset_data$value[i]])
        } else {
          NA_real_
        }
      })
    } else {
      # For already numeric values
      subset_data$value_num <- suppressWarnings(as.numeric(subset_data$value))
      
      # Mark NS/NC values based on metadata
      subset_data$response_type <- case_when(
        subset_data$is_na ~ "MISSING",
        !is.na(subset_data$value_num) & subset_data$value_num %in% ns_nc_codes ~ "NS/NC",
        TRUE ~ "VALID"
      )
    }
  }
  
  # Count responses by type
  ns_nc_count <- sum(subset_data$response_type == "NS/NC", na.rm = TRUE)
  missing_count <- sum(subset_data$response_type == "MISSING", na.rm = TRUE)
  total_responses <- nrow(subset_data)
  
  # Filter to valid responses for analysis
  valid_data <- subset_data %>%
    filter(response_type == "VALID") %>%
    mutate(
      district = as.factor(district),
      gender = as.factor(gender),
      age_group = as.factor(age_group)
    )
  
  # If no valid data, return empty structure with attributes
  if (nrow(valid_data) == 0) {
    warning(paste("No valid data for question", question_id))
    empty_data <- data.frame(
      value = character(),
      district = character(),
      gender = character(),
      age_group = character(),
      value_num = numeric()
    )
    attr(empty_data, "has_labels") <- !is.null(value_labels)
    attr(empty_data, "value_labels") <- value_labels
    attr(empty_data, "ns_nc_count") <- ns_nc_count
    attr(empty_data, "missing_count") <- missing_count
    attr(empty_data, "total_responses") <- total_responses
    attr(empty_data, "numeric_values") <- numeric()
    return(empty_data)
  }
  
  # Create factor levels for display
  if (special_cases || (!is.null(value_labels) && any(names(value_labels) %in% c(1:10)))) {
    # For scale questions like Q56 (1-10 plus No hay)
    # Extract numeric values for factor levels
    valid_numeric_values <- sort(as.numeric(names(value_labels)[!names(value_labels) %in% ns_nc_codes]))
    
    # Create factor with numeric labels first
    valid_data$value <- factor(
      valid_data$value_num,
      levels = valid_numeric_values,
      ordered = TRUE
    )
  } else if (!is.null(value_labels)) {
    # For standard labeled questions
    ordered_values <- as.character(sort(as.numeric(names(value_labels)[!names(value_labels) %in% ns_nc_codes])))
    ordered_labels <- value_labels[ordered_values]
    
    valid_data$value <- factor(
      valid_data$value_num,
      levels = as.numeric(ordered_values),
      labels = ordered_labels,
      ordered = TRUE
    )
  } else {
    # For unlabeled numeric data
    max_val <- max(valid_data$value_num, na.rm = TRUE)
    
    # Fix for "result would be too long a vector" error
    if (is.finite(max_val) && max_val <= 1000) {  # Add a reasonable limit
      valid_data$value <- factor(
        valid_data$value_num,
        levels = 1:max_val,
        ordered = TRUE
      )
    } else {
      # Just use the unique values as levels instead of 1:max_val
      unique_values <- sort(unique(valid_data$value_num))
      valid_data$value <- factor(
        valid_data$value_num,
        levels = unique_values,
        ordered = TRUE
      )
    }
  }
  
  # Add attributes about the processing
  attr(valid_data, "has_labels") <- !is.null(value_labels)
  attr(valid_data, "value_labels") <- value_labels
  attr(valid_data, "ns_nc_count") <- ns_nc_count
  attr(valid_data, "missing_count") <- missing_count
  attr(valid_data, "total_responses") <- total_responses
  attr(valid_data, "numeric_values") <- valid_data$value_num
  attr(valid_data, "question_label") <- get_question_label(question_id, metadata)

  # Simplify the data frame to include only necessary columns
  valid_data <- valid_data %>%
    select(value, district, gender, age_group, value_num)
  
  return(valid_data)
}

get_value_label <- function(numeric_value, data) {
  # Get value labels from the data's attributes
  value_labels <- attr(data, "value_labels")
  
  if (!is.null(value_labels) && as.character(numeric_value) %in% names(value_labels)) {
    return(value_labels[as.character(numeric_value)])
  } else {
    return(as.character(numeric_value))
  }
}

# Function to get the most common value with proper label
get_mode_with_label <- function(data) {
  if (is.factor(data$value)) {
    # For factor data, get the most common level
    mode_value <- names(which.max(table(data$value)))
    return(mode_value)
  } else {
    # For numeric data, find most common value
    numeric_values <- get_numeric_values(data)
    mode_table <- table(numeric_values)
    if (length(mode_table) == 0) return("No data")
    
    mode_numeric <- as.numeric(names(which.max(mode_table)))
    
    # Try to get label from value_labels attribute
    value_labels <- attr(data, "value_labels")
    if (!is.null(value_labels) && as.character(mode_numeric) %in% names(value_labels)) {
      return(value_labels[as.character(mode_numeric)])
    }
    
    return(as.character(mode_numeric))
  }
}

# Create a label lookup table for a dataset
create_label_lookup <- function(data) {
  value_labels <- attr(data, "value_labels")
  if (is.null(value_labels)) {
    # If no labels, create a simple identity mapping
    unique_values <- unique(get_numeric_values(data))
    unique_values <- unique_values[!is.na(unique_values)]
    return(setNames(as.character(unique_values), unique_values))
  }
  return(value_labels)
}

# Helper function to get numeric values
get_numeric_values <- function(data) {
  if("value_num" %in% names(data)) {
    return(data$value_num)
  } else if(!is.null(attr(data, "numeric_values"))) {
    return(attr(data, "numeric_values"))
  } else {
    # As a fallback, try direct conversion
    return(suppressWarnings(as.numeric(as.character(data$value))))
  }
}


# Visualization functions
create_interval_histogram <- function(data, bins = 30, title = "Distribución") {
  # Check if we have labels
  has_labels <- attr(data, "has_labels")
  
  plot <- plot_ly(
    data = data,
    x = ~value,
    type = "histogram",
    nbinsx = if(has_labels) length(levels(data$value)) else bins,
    marker = list(
      color = theme_config$colors$primary,
      line = list(
        color = theme_config$colors$neutral,
        width = 1
      )
    )
  ) %>%
    apply_plotly_theme(
      title = title,
      xlab = "Valor",
      ylab = "Frecuencia"
    )
  
  if(!has_labels) {
    plot <- plot %>%
      layout(
        xaxis = list(
          tickmode = "linear",
          tick0 = 1,
          dtick = 1
        )
      )
  }
  
  return(plot)
}

create_interval_district_map <- function(data, geo_data) {
  # Create label lookup
  labels_lookup <- create_label_lookup(data)
  
  # Calculate district statistics
  district_stats <- data %>%
    mutate(numeric_value = get_numeric_values(.)) %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(numeric_value, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Get mode values for each district
  district_modes <- data %>%
    group_by(district) %>%
    summarise(
      mode_numeric = as.numeric(names(which.max(table(get_numeric_values(.))))),
      .groups = 'drop'
    ) %>%
    # Add text labels for modes
    mutate(
      mode_label = sapply(mode_numeric, function(val) {
        if (as.character(val) %in% names(labels_lookup)) {
          return(labels_lookup[as.character(val)])
        } else {
          return(as.character(val))
        }
      })
    )
  
  # Join the mode values
  district_stats <- district_stats %>%
    left_join(district_modes, by = "district")
  
  # Create map
  leaflet(geo_data) %>%
    addTiles() %>% 
    addPolygons(
      fillOpacity = 0.7,
      weight = 1,
      color = theme_config$palette$district[match(geo_data$No_Distrit, district_stats$district)],
      dashArray = "3",
      highlight = highlightOptions(
        weight = 2,
        color = theme_config$colors$highlight,
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "Distrito: %s<br>Respuesta más común: %s<br>Promedio: %.2f<br>N: %d",
        district_stats$district[match(No_Distrit, district_stats$district)],
        district_stats$mode_label[match(No_Distrit, district_stats$district)],
        district_stats$mean_value[match(No_Distrit, district_stats$district)],
        district_stats$n[match(No_Distrit, district_stats$district)]
      ) %>% lapply(HTML)
    )
}

create_interval_age_bars <- function(data) {
  # Create label lookup
  labels_lookup <- create_label_lookup(data)
  
  # Calculate age group statistics 
  age_stats <- data %>%
    group_by(age_group) %>%
    summarise(
      mean_value = mean(get_numeric_values(.), na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Get mode values for each age group with labels
  age_modes <- data %>%
    group_by(age_group) %>%
    summarise(
      mode_numeric = as.numeric(names(which.max(table(get_numeric_values(.))))),
      .groups = 'drop'
    ) %>%
    # Add text labels for modes
    mutate(
      mode_label = sapply(mode_numeric, function(val) {
        if (as.character(val) %in% names(labels_lookup)) {
          return(labels_lookup[as.character(val)])
        } else {
          return(as.character(val))
        }
      })
    )
  
  # Join the mode values
  age_stats <- age_stats %>%
    left_join(age_modes, by = "age_group")
  
  # Create the plot
  plot_ly(
    data = age_stats,
    x = ~age_group,
    y = ~mean_value,
    type = "bar",
    text = ~sprintf(
      "Respuesta más común: %s<br>N: %d",
      mode_label, n
    ),
    hoverinfo = "text+y",
    marker = list(
      color = get_color_palette("age_group")
    )
  ) %>%
    apply_plotly_theme(
      title = "Distribución por Grupo de Edad",
      xlab = "Grupo de Edad",
      ylab = "Valor Promedio"
    )
}

create_interval_gender_dumbbell <- function(data) {
  # Create label lookup
  labels_lookup <- create_label_lookup(data)
  
  # Calculate gender statistics by district
  gender_stats <- data %>%
    group_by(district, gender) %>%
    summarise(
      mean_value = mean(get_numeric_values(.), na.rm = TRUE),
      mode_numeric = as.numeric(names(which.max(table(get_numeric_values(.))))),
      n = n(),
      .groups = 'drop'
    ) %>%
    # Add text labels for modes
    mutate(
      mode_label = sapply(mode_numeric, function(val) {
        if (as.character(val) %in% names(labels_lookup)) {
          return(labels_lookup[as.character(val)])
        } else {
          return(as.character(val))
        }
      })
    )
  
  # Pivot wider for the dumbbell plot
  gender_stats_wide <- gender_stats %>%
    pivot_wider(
      id_cols = district,
      names_from = gender,
      values_from = c(mean_value, mode_label, n)
    )
  
  # Create the plot
  p <- plot_ly() %>%
    add_trace(
      data = gender_stats_wide,
      x = ~mean_value_Hombre,
      y = ~district,
      name = "Hombre",
      type = "scatter",
      mode = "markers",
      text = ~sprintf(
        "Respuesta más común: %s<br>N: %d",
        mode_label_Hombre, n_Hombre
      ),
      hoverinfo = "text+x",
      marker = list(color = get_color_palette("gender")[2])
    ) %>%
    add_trace(
      data = gender_stats_wide,
      x = ~mean_value_Mujer,
      y = ~district,
      name = "Mujer",
      type = "scatter",
      mode = "markers",
      text = ~sprintf(
        "Respuesta más común: %s<br>N: %d",
        mode_label_Mujer, n_Mujer
      ),
      hoverinfo = "text+x",
      marker = list(color = get_color_palette("gender")[1])
    )
  
  # Add connecting lines
  for(i in 1:nrow(gender_stats_wide)) {
    p <- add_segments(p,
      x = gender_stats_wide$mean_value_Hombre[i],
      xend = gender_stats_wide$mean_value_Mujer[i],
      y = gender_stats_wide$district[i],
      yend = gender_stats_wide$district[i],
      line = list(color = theme_config$colors$neutral),
      showlegend = FALSE
    )
  }
  
  p %>% 
    apply_plotly_theme(
      title = "Comparación por Género y Distrito",
      xlab = "Valor Promedio",
      ylab = "Distrito"
    ) %>%
    layout(showlegend = TRUE)
}

create_interval_bars <- function(data, orientation = "v") {
  # Check if we have valid data
  if (nrow(data) == 0) {
    return(plot_ly() %>% 
             layout(title = "No hay datos válidos para visualizar",
                    xaxis = list(title = ""),
                    yaxis = list(title = "")))
  }
  
  # Create label lookup
  labels_lookup <- create_label_lookup(data)
  
  # Calculate district statistics
  district_stats <- data %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(get_numeric_values(.), na.rm = TRUE),
      mode_numeric = as.numeric(names(which.max(table(get_numeric_values(.))))),
      n = n(),
      .groups = 'drop'
    ) %>%
    # Add text labels for modes
    mutate(
      mode_label = sapply(mode_numeric, function(val) {
        if (as.character(val) %in% names(labels_lookup)) {
          return(labels_lookup[as.character(val)])
        } else {
          return(as.character(val))
        }
      })
    )
  
  # Check if we have valid statistics
  if (nrow(district_stats) == 0 || all(is.na(district_stats$mean_value))) {
    return(plot_ly() %>% 
             layout(title = "No se pueden calcular estadísticas para esta pregunta",
                    xaxis = list(title = ""),
                    yaxis = list(title = "")))
  }
  
  # Add hover text
  district_stats$hover_text <- sprintf(
    "Respuesta más común: %s<br>N: %d",
    district_stats$mode_label,
    district_stats$n
  )
  
  plot_functions$bar(
    data = district_stats,
    x = "district",
    y = "mean_value",
    title = "Distribución por Distrito",
    xlab = "Distrito",
    ylab = "Valor Promedio",
    orientation = orientation,
    color_by = 'district'
  )
}
# UI Definition
intervalUI <- function(id) {
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
            condition = sprintf("input['%s'] == 'histogram'", ns("plot_type")),
            sliderInput(
              ns("bins"),
              "Número de Bins",
              min = 10,
              max = 50,
              value = 30
            )
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bars'", ns("plot_type")),
            radioButtons(
              ns("bar_orientation"),
              "Orientación",
              choices = c(
                "Vertical" = "v",
                "Horizontal" = "h"
              )
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

intervalServer <- function(id, data, metadata, selected_question, geo_data) {
  moduleServer(id, function(input, output, session) {
    # Initial data preparation with metadata
    prepared_data <- reactive({
      tryCatch({
        req(data(), selected_question(), metadata())
        
        # Add validation
        if (is.null(selected_question()) || selected_question() == "") {
          return(NULL)
        }
        
        prepare_interval_data(data(), selected_question(), metadata())
      }, error = function(e) {
        warning(paste("Error in prepared_data:", e$message))
        return(NULL)
      })
    })
    
    # Update filter choices
    observe({
      req(prepared_data())
      if (is.null(prepared_data())) {
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
    
    # Filtered data reactive
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
    
    # Dynamic plot output based on selection
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

    output$summary_stats <- renderPrint({
      data <- filtered_data()
      
      # Get the response counts from attributes
      ns_nc_count <- attr(data, "ns_nc_count")
      missing_count <- attr(data, "missing_count")
      total_responses <- attr(data, "total_responses")
      valid_responses <- nrow(data)
      value_labels <- attr(data, "value_labels")
      
      cat("Estadísticas para Datos de Intervalo:\n")
      cat("\nDistribución de Respuestas:\n")
      cat("Total de respuestas:", total_responses, "\n")
      cat("Respuestas válidas:", valid_responses, "\n")
      cat("No sabe/No contesta:", ns_nc_count, 
          sprintf("(%.1f%%)", 100 * ns_nc_count/total_responses), "\n")
      cat("Datos faltantes:", missing_count,
          sprintf("(%.1f%%)", 100 * missing_count/total_responses), "\n")
      
      if(is.factor(data$value)) {
        # For labeled data, show frequency distribution with factor levels
        cat("\nDistribución de valores (con etiquetas):\n")
        print(table(data$value))
        
        # Calculate and display mode as factor level
        mode_value <- names(which.max(table(data$value)))
        
        # Calculate statistics on numeric values
        numeric_values <- get_numeric_values(data)
        
        cat("\nEstadísticas:\n")
        cat("Moda (respuesta más común):", mode_value, "\n")
        cat("Media:", round(mean(numeric_values, na.rm = TRUE), 2), "\n")
        cat("Mediana:", median(numeric_values, na.rm = TRUE), "\n")
        cat("Desviación Estándar:", round(sd(numeric_values, na.rm = TRUE), 2), "\n")
        cat("Rango:", min(numeric_values, na.rm = TRUE), "a", max(numeric_values, na.rm = TRUE), "\n")
        
        # Show the value label mapping for reference
        if (!is.null(value_labels) && length(value_labels) > 0) {
          cat("\nCorrespondencia entre valores numéricos y etiquetas:\n")
          for (i in 1:length(value_labels)) {
            cat(paste0(names(value_labels)[i], " = ", value_labels[i]), "\n")
          }
        }
      } else {
        # Get numeric values and create a lookup table
        numeric_values <- get_numeric_values(data)
        
        # Display distribution with labels if available
        if (!is.null(value_labels) && length(value_labels) > 0) {
          cat("\nDistribución de valores (con etiquetas):\n")
          value_counts <- table(numeric_values)
          
          # Create a labeled table
          labeled_counts <- data.frame(
            Valor_Numérico = names(value_counts),
            Frecuencia = as.vector(value_counts),
            Etiqueta = sapply(names(value_counts), function(val) {
              if (val %in% names(value_labels)) value_labels[val] else val
            })
          )
          print(labeled_counts)
        } else {
          cat("\nEstadísticas descriptivas:\n")
          print(summary(numeric_values))
        }
        
        # Calculate and show mode with label
        mode_numeric <- as.numeric(names(which.max(table(numeric_values))))
        mode_label <- if (!is.null(value_labels) && as.character(mode_numeric) %in% names(value_labels)) {
          value_labels[as.character(mode_numeric)]
        } else {
          as.character(mode_numeric)
        }
        
        cat("\nEstadísticas adicionales:\n")
        cat("Moda (respuesta más común):", mode_label, "\n")
        cat("Desviación Estándar:", round(sd(numeric_values, na.rm = TRUE), 2), "\n")
        cat("Coeficiente de Variación:", round(sd(numeric_values, na.rm = TRUE) / mean(numeric_values, na.rm = TRUE) * 100, 2), "%\n")
        
        # Show the value label mapping for reference
        if (!is.null(value_labels) && length(value_labels) > 0) {
          cat("\nCorrespondencia entre valores numéricos y etiquetas:\n")
          for (i in 1:length(value_labels)) {
            cat(paste0(names(value_labels)[i], " = ", value_labels[i]), "\n")
          }
        }
      }
    })
    
 # Update plot outputs
 output$histogram_plot <- renderPlotly({
  req(filtered_data())
  create_interval_histogram(
    filtered_data(), 
    bins = input$bins,
    title = paste("Distribución de", selected_question())
  )
})

output$district_map <- renderLeaflet({
  req(filtered_data(), geo_data())
  create_interval_district_map(filtered_data(), geo_data())
})

output$age_bars_plot <- renderPlotly({
  req(filtered_data())
  create_interval_age_bars(filtered_data())
})

output$gender_dumbbell_plot <- renderPlotly({
  req(filtered_data())
  create_interval_gender_dumbbell(filtered_data())
})

output$bar_plot <- renderPlotly({
  req(filtered_data())
  create_interval_bars(
    filtered_data(),
    orientation = input$bar_orientation
  )
})
})
}