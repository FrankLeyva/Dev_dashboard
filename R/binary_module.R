prepare_binary_data <- function(data, question_id, metadata) {
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
  
  # Initialize value_labels and response mapping
  value_labels <- NULL
  positive_values <- c("1", "Sí", "Si", "Yes", "Selected", "TRUE", "True", "true")
  negative_values <- c("0", "No", "Not Selected", "FALSE", "False", "false")
  
  # Process value labels if they exist
  if (question_metadata$has_value_labels && !is.na(question_metadata$value_labels)) {
    tryCatch({
      # Split the labels string by semicolon
      label_pairs <- strsplit(question_metadata$value_labels, ";")[[1]]
      
      # Create mapping vectors
      values <- character()
      labels <- character()
      
      # Process each pair
      for(pair in label_pairs) {
        # Split by equals and clean up
        parts <- strsplit(trimws(pair), "=")[[1]]
        if(length(parts) == 2) {
          value <- trimws(parts[1])
          label <- trimws(parts[2])
          
          values <- c(values, value)
          labels <- c(labels, label)
        }
      }
      
      # Create mappings
      value_labels <- setNames(labels, values)
      
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
  
  # Standardize binary values (convert to TRUE/FALSE)
  subset_data <- subset_data %>%
    mutate(
      binary_value = case_when(
        is_na ~ NA,
        value %in% positive_values ~ TRUE,
        value %in% negative_values ~ FALSE,
        as.character(value) == "1" ~ TRUE,
        as.character(value) == "0" ~ FALSE,
        toupper(value) == "TRUE" ~ TRUE,
        toupper(value) == "FALSE" ~ FALSE,
        toupper(value) == "YES" ~ TRUE,
        toupper(value) == "NO" ~ FALSE,
        toupper(value) == "SELECTED" ~ TRUE,
        TRUE ~ NA
      )
    )
  
  # Count responses by type
  missing_count <- sum(is.na(subset_data$binary_value))
  total_responses <- nrow(subset_data)
  
  # Filter to valid responses for analysis
  valid_data <- subset_data %>%
    filter(!is.na(binary_value)) %>%
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
      binary_value = logical()
    )
    attr(empty_data, "has_labels") <- !is.null(value_labels)
    attr(empty_data, "value_labels") <- value_labels
    attr(empty_data, "missing_count") <- missing_count
    attr(empty_data, "total_responses") <- total_responses
    attr(empty_data, "question_id") <- question_id
    attr(empty_data, "question_label") <- question_metadata$label
    return(empty_data)
  }
  
  # Add attributes about the processing
  attr(valid_data, "has_labels") <- !is.null(value_labels)
  attr(valid_data, "value_labels") <- value_labels
  attr(valid_data, "missing_count") <- missing_count
  attr(valid_data, "total_responses") <- total_responses
  attr(valid_data, "question_id") <- question_id
  attr(valid_data, "question_label") <- question_metadata$label
  attr(valid_data, "question_label") <- get_question_label(question_id, metadata)

  return(valid_data)
}

# Function to prepare multiple binary questions for comparison
prepare_multiple_binary <- function(data, question_ids, metadata) {
  if (length(question_ids) == 0) {
    return(NULL)
  }
  
  # Get metadata for these questions
  questions_metadata <- metadata %>%
    filter(variable %in% question_ids)
  
  # Prepare a list to store individual question data
  question_data_list <- list()
  
  # Process each question
  for (qid in question_ids) {
    question_data <- prepare_binary_data(data, qid, metadata)
    if (!is.null(question_data) && nrow(question_data) > 0) {
      question_data_list[[qid]] <- question_data
    }
  }
  
  # Return the list of processed data
  return(question_data_list)
}

# Visualization functions
create_binary_bar <- function(data, title = "Distribución de Respuestas") {
  # Calculate proportions
  response_counts <- table(data$binary_value)
  
  # Create data frame for plotting
  if (length(response_counts) == 0) {
    return(plotly_empty() %>% 
           layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get value labels if available
  value_labels <- attr(data, "value_labels")
  
  # Determine appropriate labels
  if (!is.null(value_labels) && length(value_labels) >= 2) {
    # Use the value labels from metadata
    true_label <- value_labels[names(value_labels)[1]]
    false_label <- value_labels[names(value_labels)[2]]
  } else {
    # Default labels
    true_label <- "Sí"
    false_label <- "No"
  }
  
  plot_df <- data.frame(
    Response = c(true_label, false_label),
    Count = c(
      sum(data$binary_value, na.rm = TRUE), 
      sum(!data$binary_value, na.rm = TRUE)
    ),
    Percentage = c(
      round(100 * mean(data$binary_value, na.rm = TRUE), 1),
      round(100 * (1 - mean(data$binary_value, na.rm = TRUE)), 1)
    )
  )
  
  # Create bar chart
  plot_ly(
    data = plot_df,
    x = ~Response,
    y = ~Count,
    type = "bar",
    text = ~paste0(Response, ": ", Count, " (", Percentage, "%)"),
    hoverinfo = "text",
    marker = list(
      color = c(theme_config$colors$primary, theme_config$colors$secondary)
    )
  ) %>%
    apply_plotly_theme(
      title = title,
      xlab = "Respuesta",
      ylab = "Frecuencia"
    )
}

create_binary_pie <- function(data, title = "Distribución de Respuestas") {
  # Calculate proportions
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
           layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get value labels if available
  value_labels <- attr(data, "value_labels")
  
  # Determine appropriate labels
  if (!is.null(value_labels) && length(value_labels) >= 2) {
    # Use the value labels from metadata
    true_label <- value_labels[names(value_labels)[1]]
    false_label <- value_labels[names(value_labels)[2]]
  } else {
    # Default labels
    true_label <- "Sí"
    false_label <- "No"
  }
  
  values <- c(
    sum(data$binary_value, na.rm = TRUE), 
    sum(!data$binary_value, na.rm = TRUE)
  )
  
  labels <- c(true_label, false_label)
  
  percentages <- c(
    round(100 * mean(data$binary_value, na.rm = TRUE), 1),
    round(100 * (1 - mean(data$binary_value, na.rm = TRUE)), 1)
  )
  
  # Create pie chart
  plot_ly(
    labels = ~labels,
    values = ~values,
    type = "pie",
    textinfo = "label+percent",
    insidetextorientation = "radial",
    marker = list(
      colors = c(theme_config$colors$primary, theme_config$colors$secondary),
      line = list(color = '#FFFFFF', width = 1)
    ),
    hoverinfo = "text",
    text = ~paste0(labels, ": ", values, " respuestas (", percentages, "%)")
  ) %>%
    layout(
      title = title,
      showlegend = FALSE
    )
}

create_binary_district_bars <- function(data, orientation = "v") {
  # Check if we have data
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get value labels if available
  value_labels <- attr(data, "value_labels")
  
  # Determine appropriate labels
  if (!is.null(value_labels) && length(value_labels) >= 2) {
    # Use the value labels from metadata
    true_label <- value_labels[names(value_labels)[1]]
  } else {
    # Default label
    true_label <- "Sí"
  }
  
  # Calculate percentages by district
  district_stats <- data %>%
    group_by(district) %>%
    summarise(
      percentage_true = 100 * mean(binary_value, na.rm = TRUE),
      count_true = sum(binary_value, na.rm = TRUE),
      count_false = sum(!binary_value, na.rm = TRUE),
      count_total = n(),
      .groups = 'drop'
    )
  
  # Create bar chart
  if (orientation == "h") {
    plot_ly(
      data = district_stats,
      y = ~reorder(district, percentage_true),
      x = ~percentage_true,
      type = "bar",
      orientation = 'h',
      text = ~paste0(
        "Distrito: ", district, "<br>",
        true_label, ": ", count_true, " (", round(percentage_true, 1), "%)<br>",
        "Total: ", count_total
      ),
      hoverinfo = "text",
      marker = list(color = theme_config$colors$primary)
    ) %>%
      apply_plotly_theme(
        title = paste0("Porcentaje de ", true_label, " por Distrito"),
        xlab = paste0("% de ", true_label),
        ylab = "Distrito"
      )
  } else {
    plot_ly(
      data = district_stats,
      x = ~district,
      y = ~percentage_true,
      type = "bar",
      text = ~paste0(
        "Distrito: ", district, "<br>",
        true_label, ": ", count_true, " (", round(percentage_true, 1), "%)<br>",
        "Total: ", count_total
      ),
      hoverinfo = "text",
      marker = list(color = theme_config$colors$primary)
    ) %>%
      apply_plotly_theme(
        title = paste0("Porcentaje de ", true_label, " por Distrito"),
        xlab = "Distrito",
        ylab = paste0("% de ", true_label)
      ) %>%
      layout(
        xaxis = list(tickangle = 45)
      )
  }
}

create_binary_district_map <- function(data, geo_data) {
  # Check if we have data
  if (nrow(data) == 0 || is.null(geo_data)) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get value labels if available
  value_labels <- attr(data, "value_labels")
  
  # Determine appropriate labels
  if (!is.null(value_labels) && length(value_labels) >= 2) {
    # Use the value labels from metadata
    true_label <- value_labels[names(value_labels)[1]]
  } else {
    # Default label
    true_label <- "Sí"
  }
  
  # Calculate percentages by district
  district_stats <- data %>%
    group_by(district) %>%
    summarise(
      percentage_true = 100 * mean(binary_value, na.rm = TRUE),
      count_true = sum(binary_value, na.rm = TRUE),
      count_false = sum(!binary_value, na.rm = TRUE),
      count_total = n(),
      .groups = 'drop'
    )
  
  # Create color palette
  pal <- colorNumeric(
    palette = "Blues",
    domain = district_stats$percentage_true
  )
  
  # Create map
  leaflet(geo_data) %>%
    addTiles() %>% 
    addPolygons(
      fillColor = ~pal(district_stats$percentage_true[match(No_Distrit, district_stats$district)]),
      fillOpacity = 0.7,
      weight = 1,
      color = "#666666",
      dashArray = "3",
      highlight = highlightOptions(
        weight = 2,
        color = "#666666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "Distrito: %s<br>%s: %d (%.1f%%)<br>Total: %d",
        district_stats$district[match(No_Distrit, district_stats$district)],
        true_label,
        district_stats$count_true[match(No_Distrit, district_stats$district)],
        district_stats$percentage_true[match(No_Distrit, district_stats$district)],
        district_stats$count_total[match(No_Distrit, district_stats$district)]
      ) %>% lapply(HTML)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = district_stats$percentage_true,
      title = paste0("% de ", true_label),
      opacity = 0.7
    )
}

create_binary_demographics_bars <- function(data, group_var = "gender") {
  # Check if we have data
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get value labels if available
  value_labels <- attr(data, "value_labels")
  
  # Determine appropriate labels
  if (!is.null(value_labels) && length(value_labels) >= 2) {
    # Use the value labels from metadata
    true_label <- value_labels[names(value_labels)[1]]
    false_label <- value_labels[names(value_labels)[2]]
  } else {
    # Default labels
    true_label <- "Sí"
    false_label <- "No"
  }
  
  # Calculate percentages by demographic group
  demo_stats <- data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      percentage_true = 100 * mean(binary_value, na.rm = TRUE),
      count_true = sum(binary_value, na.rm = TRUE),
      count_false = sum(!binary_value, na.rm = TRUE),
      count_total = n(),
      .groups = 'drop'
    )
  
  # Create grouped bar chart
  plot_ly(
    data = demo_stats,
    x = ~get(group_var),
    y = ~percentage_true,
    type = "bar",
    text = ~paste0(
      get(group_var), "<br>",
      true_label, ": ", count_true, " (", round(percentage_true, 1), "%)<br>",
      false_label, ": ", count_false, " (", round(100 - percentage_true, 1), "%)<br>",
      "Total: ", count_total
    ),
    hoverinfo = "text",
    marker = list(
      color = if(group_var == "gender") get_color_palette("gender") else 
             if(group_var == "age_group") get_color_palette("age_group") else 
             theme_config$colors$primary
    )
  ) %>%
    apply_plotly_theme(
      title = paste("Distribución por", ifelse(group_var == "gender", "Género", "Grupo de Edad")),
      xlab = ifelse(group_var == "gender", "Género", "Grupo de Edad"),
      ylab = paste0("Porcentaje de ", true_label)
    )
}

# New function for gender dumbbell plot
create_binary_gender_dumbbell <- function(data) {
  # Check if we have data
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get value labels if available
  value_labels <- attr(data, "value_labels")
  
  # Determine appropriate label
  if (!is.null(value_labels) && length(value_labels) >= 2) {
    # Use the value labels from metadata
    true_label <- value_labels[names(value_labels)[1]]
  } else {
    # Default label
    true_label <- "Sí"
  }
  
  # Calculate gender statistics by district
  gender_stats <- data %>%
    group_by(district, gender) %>%
    summarise(
      percentage_true = 100 * mean(binary_value, na.rm = TRUE),
      count_true = sum(binary_value, na.rm = TRUE),
      count_total = n(),
      .groups = 'drop'
    )
  
  # Pivot wider for the dumbbell plot
  gender_stats_wide <- gender_stats %>%
    pivot_wider(
      id_cols = district,
      names_from = gender,
      values_from = c(percentage_true, count_true, count_total)
    )
  
  # Create the plot
  p <- plot_ly() %>%
    add_trace(
      data = gender_stats_wide,
      x = ~percentage_true_Hombre,
      y = ~district,
      name = "Hombre",
      type = "scatter",
      mode = "markers",
      text = ~paste0(
        "Distrito: ", district, "<br>",
        "Hombre - ", true_label, ": ", count_true_Hombre, " (", round(percentage_true_Hombre, 1), "%)<br>",
        "Total: ", count_total_Hombre
      ),
      hoverinfo = "text",
      marker = list(color = get_color_palette("gender")[2])
    ) %>%
    add_trace(
      data = gender_stats_wide,
      x = ~percentage_true_Mujer,
      y = ~district,
      name = "Mujer",
      type = "scatter",
      mode = "markers",
      text = ~paste0(
        "Distrito: ", district, "<br>",
        "Mujer - ", true_label, ": ", count_true_Mujer, " (", round(percentage_true_Mujer, 1), "%)<br>",
        "Total: ", count_total_Mujer
      ),
      hoverinfo = "text",
      marker = list(color = get_color_palette("gender")[1])
    )
  
  # Add connecting lines
  for(i in 1:nrow(gender_stats_wide)) {
    p <- add_segments(p,
      x = gender_stats_wide$percentage_true_Hombre[i],
      xend = gender_stats_wide$percentage_true_Mujer[i],
      y = gender_stats_wide$district[i],
      yend = gender_stats_wide$district[i],
      line = list(color = theme_config$colors$neutral),
      showlegend = FALSE
    )
  }
  
  p %>% 
    apply_plotly_theme(
      title = paste0("Comparación por Género: % de ", true_label, " por Distrito"),
      xlab = paste0("% de ", true_label),
      ylab = "Distrito"
    ) %>%
    layout(showlegend = TRUE)
}

create_multiple_binary_comparison <- function(data_list, comparison_type = "bars", top_n = 5) {
  # Check if we have data
  if (length(data_list) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay preguntas seleccionadas para comparar"))
  }
  
  # Prepare comparison data
  comparison_data <- data.frame(
    QuestionID = character(),
    QuestionLabel = character(),
    PercentageTrue = numeric(),
    CountTrue = integer(),
    CountTotal = integer(),
    stringsAsFactors = FALSE
  )
  
  for (qid in names(data_list)) {
    data <- data_list[[qid]]
    
    # Skip if no valid data
    if (nrow(data) == 0) next
    
    # Get question label
    question_label <- attr(data, "question_label")
    if (is.null(question_label)) question_label <- qid
    
    # Calculate statistics
    percentage_true <- 100 * mean(data$binary_value, na.rm = TRUE)
    count_true <- sum(data$binary_value, na.rm = TRUE)
    count_total <- nrow(data)
    
    # Add to comparison data
    comparison_data <- rbind(
      comparison_data,
      data.frame(
        QuestionID = qid,
        QuestionLabel = question_label,
        PercentageTrue = percentage_true,
        CountTrue = count_true,
        CountTotal = count_total,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # If no valid data, return empty plot
  if (nrow(comparison_data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos válidos para comparar"))
  }
  
  # Sort by percentage true and take top N
  comparison_data <- comparison_data %>%
    arrange(desc(PercentageTrue)) %>%
    mutate(
      ShortLabel = ifelse(
        nchar(QuestionLabel) > 50,
        paste0(substr(QuestionLabel, 1, 47), "..."),
        QuestionLabel
      )
    )
  
  if (nrow(comparison_data) > top_n) {
    comparison_data <- comparison_data[1:top_n, ]
  }
  
  # Create visualization based on type
  if (comparison_type == "bars") {
    # Horizontal bar chart
    plot_ly(
      data = comparison_data,
      y = ~reorder(ShortLabel, PercentageTrue),
      x = ~PercentageTrue,
      type = "bar",
      orientation = 'h',
      text = ~paste0(
        QuestionLabel, "<br>",
        "Sí: ", CountTrue, " (", round(PercentageTrue, 1), "%)<br>",
        "Total: ", CountTotal
      ),
      hoverinfo = "text",
      marker = list(
        color = colorRampPalette(c(theme_config$colors$primary, theme_config$colors$highlight))(nrow(comparison_data))
      )
    ) %>%
      apply_plotly_theme(
        title = "Comparación de Preguntas Binarias",
        xlab = "Porcentaje de Respuestas Positivas",
        ylab = ""
      )
  } else if (comparison_type == "pie") {
    # Create pie chart data
    pie_data <- data.frame(
      category = comparison_data$ShortLabel,
      value = comparison_data$CountTrue
    )
    
    # Create pie chart
    plot_ly(
      labels = ~pie_data$category,
      values = ~pie_data$value,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hoverinfo = "text",
      text = ~paste0(
        comparison_data$QuestionLabel, "<br>",
        "Sí: ", comparison_data$CountTrue, " (", round(comparison_data$PercentageTrue, 1), "%)<br>",
        "Total: ", comparison_data$CountTotal
      )
    ) %>%
      layout(
        title = "Distribución de Respuestas Positivas por Pregunta"
      )
  } else if (comparison_type == "bubbles") {
    # Create bubble chart
    plot_ly(
      data = comparison_data,
      x = ~reorder(ShortLabel, -PercentageTrue),
      y = ~PercentageTrue,
      size = ~CountTotal,
      type = "scatter",
      mode = "markers",
      marker = list(
        sizemode = "area",
        sizeref = 2 * max(comparison_data$CountTotal) / (40^2),
        sizemin = 5,
        color = ~PercentageTrue,
        colorscale = "Blues",
        colorbar = list(title = "% Sí")
      ),
      text = ~paste0(
        QuestionLabel, "<br>",
        "Sí: ", CountTrue, " (", round(PercentageTrue, 1), "%)<br>",
        "Total: ", CountTotal
      ),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Comparación de Preguntas Binarias",
        xlab = "",
        ylab = "Porcentaje de Respuestas Positivas"
      ) %>%
      layout(
        xaxis = list(tickangle = 45)
      )
  } else {
    # Default to horizontal bar chart
    plot_ly(
      data = comparison_data,
      y = ~reorder(ShortLabel, PercentageTrue),
      x = ~PercentageTrue,
      type = "bar",
      orientation = 'h',
      text = ~paste0(
        QuestionLabel, "<br>",
        "Sí: ", CountTrue, " (", round(PercentageTrue, 1), "%)<br>",
        "Total: ", CountTotal
      ),
      hoverinfo = "text",
      marker = list(
        color = colorRampPalette(c(theme_config$colors$primary, theme_config$colors$highlight))(nrow(comparison_data))
      )
    ) %>%
      apply_plotly_theme(
        title = "Comparación de Preguntas Binarias",
        xlab = "Porcentaje de Respuestas Positivas",
        ylab = ""
      )
  }
}

# UI Definition for the binary module
binaryUI <- function(id) {
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
              "Gráfico de Barras" = "bars",
              "Gráfico Circular" = "pie",
              "Mapa por Distrito" = "district_map",
              "Barras por Distrito" = "district_bars",
              "Comparación por Género" = "gender_dumbbell",
              "Barras por Género" = "gender_bars",
              "Barras por Edad" = "age_bars",
              "Comparación Múltiple" = "multiple_comparison"
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
            condition = sprintf("input['%s'] == 'district_bars'", ns("plot_type")),
            radioButtons(
              ns("bar_orientation"),
              "Orientación",
              choices = c(
                "Vertical" = "v",
                "Horizontal" = "h"
              ),
              selected = "v"
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'multiple_comparison'", ns("plot_type")),
            selectInput(
              ns("compare_questions"),
              "Preguntas a Comparar",
              choices = NULL,
              multiple = TRUE
            ),
            sliderInput(
              ns("top_n"),
              "Mostrar Top N",
              min = 3,
              max = 20,
              value = 5
            ),
            radioButtons(
              ns("comparison_type"),
              "Tipo de Visualización",
              choices = c(
                "Barras Horizontales" = "bars",
                "Gráfico Circular" = "pie",
                "Gráfico de Burbujas" = "bubbles"
              ),
              selected = "bars"
            )
          )
        ), 
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

# Server Definition for the binary module
binaryServer <- function(id, data, metadata, selected_question, geo_data, all_binary_questions = NULL) {
  moduleServer(id, function(input, output, session) {
    # Initial data preparation with metadata
    prepared_data <- reactive({
      tryCatch({
        req(data(), selected_question(), metadata())
        
        # Add validation
        if (is.null(selected_question()) || selected_question() == "") {
          return(NULL)
        }
        
        prepare_binary_data(data(), selected_question(), metadata())
      }, error = function(e) {
        warning(paste("Error in prepared_data:", e$message))
        return(NULL)
      })
    })
    
    # Update compare_questions choices
    observe({
      if (!is.null(all_binary_questions)) {
        # Get labels for questions if available
        question_labels <- sapply(all_binary_questions, function(qid) {
          # Try to get label from metadata
          q_meta <- metadata() %>% filter(variable == qid) %>% first()
          if (!is.null(q_meta) && !is.na(q_meta$label)) {
            # Truncate long labels
            label <- q_meta$label
            if (nchar(label) > 50) {
              label <- paste0(substr(label, 1, 47), "...")
            }
            return(paste0(qid, " - ", label))
          } else {
            return(qid)
          }
        })
        
        updateSelectInput(session, "compare_questions",
          choices = question_labels,
          selected = character(0)
        )
      }
    })
    
    # Update filter choices
    observe({
      tryCatch({
        data <- prepared_data()
        if (is.null(data) || nrow(data) == 0) {
          return()
        }
        
        updateSelectInput(session, "district_filter",
          choices = unique(data$district),
          selected = character(0)
        )
        
        updateSelectInput(session, "gender_filter",
          choices = unique(data$gender),
          selected = character(0)
        )
        
        updateSelectInput(session, "age_filter",
          choices = unique(data$age_group),
          selected = character(0)
        )
      }, error = function(e) {
        warning(paste("Error updating filters:", e$message))
      })
    })
    
    # Filtered data reactive
    filtered_data <- reactive({
      tryCatch({
        data <- prepared_data()
        if (is.null(data) || nrow(data) == 0) return(data)
        
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
      }, error = function(e) {
        warning(paste("Error in filtered_data:", e$message))
        return(NULL)
      })
    })
    
    # Prepare multiple questions data
    multiple_questions_data <- reactive({
      tryCatch({
        req(input$plot_type == "multiple_comparison")
        
        if (length(input$compare_questions) == 0) {
          return(NULL)
        }
        
        # Extract question IDs from selections (they have format "QXX - Label")
        question_ids <- sapply(input$compare_questions, function(q) {
          strsplit(q, " - ")[[1]][1]
        })
        
        # Prepare data for all selected questions
        prepare_multiple_binary(data(), question_ids, metadata())
        
      }, error = function(e) {
        warning(paste("Error in multiple_questions_data:", e$message))
        return(NULL)
      })
    })
    
    # Dynamic plot output based on selection
    output$plot_output <- renderUI({
      plot_type <- input$plot_type
      
      switch(plot_type,
        "summary" = verbatimTextOutput(session$ns("summary_stats")),
        "bars" = plotlyOutput(session$ns("bars_plot"), height = "600px"),
        "pie" = plotlyOutput(session$ns("pie_plot"), height = "600px"),
        "district_map" = leafletOutput(session$ns("district_map_plot"), height = "600px"),
        "district_bars" = plotlyOutput(session$ns("district_bars_plot"), height = "600px"),
        "gender_dumbbell" = plotlyOutput(session$ns("gender_dumbbell_plot"), height = "600px"),
        "gender_bars" = plotlyOutput(session$ns("gender_bars_plot"), height = "600px"),
        "age_bars" = plotlyOutput(session$ns("age_bars_plot"), height = "600px"),
        "multiple_comparison" = plotlyOutput(session$ns("multiple_comparison_plot"), height = "600px")
      )
    })

    # Summary statistics
    output$summary_stats <- renderPrint({
      tryCatch({
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) {
          cat("No hay datos disponibles para visualizar")
          return()
        }
        
        # Get the response counts
        total_responses <- nrow(data)
        missing_count <- attr(data, "missing_count")
        valid_responses <- total_responses - missing_count
        
        # Calculate binary proportions
        true_count <- sum(data$binary_value, na.rm = TRUE)
        false_count <- sum(!data$binary_value, na.rm = TRUE)
        true_percent <- round(100 * true_count / (true_count + false_count), 2)
        false_percent <- round(100 * false_count / (true_count + false_count), 2)
        
        # Get value labels if available
        value_labels <- attr(data, "value_labels")
        
        # Determine appropriate labels
        if (!is.null(value_labels) && length(value_labels) >= 2) {
          # Use the value labels from metadata
          true_label <- value_labels[names(value_labels)[1]]
          false_label <- value_labels[names(value_labels)[2]]
        } else {
          # Default labels
          true_label <- "Sí"
          false_label <- "No"
        }
        
        cat("Estadísticas para Datos Binarios:\n")
        cat("\nDistribución de Respuestas:\n")
        cat("Total de respuestas:", attr(data, "total_responses"), "\n")
        cat("Respuestas válidas:", valid_responses, "\n")
        cat("Datos faltantes:", missing_count,
            sprintf("(%.1f%%)", 100 * missing_count/attr(data, "total_responses")), "\n")
        
        cat("\nDistribución de valores:\n")
        cat(true_label, ":", true_count, sprintf("(%.2f%%)", true_percent), "\n")
        cat(false_label, ":", false_count, sprintf("(%.2f%%)", false_percent), "\n")
        
        # District breakdown
        cat("\nDistribución por Distrito:\n")
        district_breakdown <- data %>%
          group_by(district) %>%
          summarise(
            Total = n(),
            `Sí` = sum(binary_value, na.rm = TRUE),
            `%Sí` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            `No` = sum(!binary_value, na.rm = TRUE),
            `%No` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
            .groups = 'drop'
          )
        
        print(district_breakdown)
        
        # Gender breakdown
        cat("\nDistribución por Género:\n")
        gender_breakdown <- data %>%
          group_by(gender) %>%
          summarise(
            Total = n(),
            `Sí` = sum(binary_value, na.rm = TRUE),
            `%Sí` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            `No` = sum(!binary_value, na.rm = TRUE),
            `%No` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
            .groups = 'drop'
          )
        
        print(gender_breakdown)
        
        # Age breakdown
        cat("\nDistribución por Grupo de Edad:\n")
        age_breakdown <- data %>%
          group_by(age_group) %>%
          summarise(
            Total = n(),
            `Sí` = sum(binary_value, na.rm = TRUE),
            `%Sí` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            `No` = sum(!binary_value, na.rm = TRUE),
            `%No` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
            .groups = 'drop'
          )
        
        print(age_breakdown)
        
      }, error = function(e) {
        cat("Error al generar estadísticas:", e$message)
      })
    })
    
    # Bar plot
    output$bars_plot <- renderPlotly({
      req(filtered_data())
      question_label <- attr(filtered_data(), "question_label")
      if (is.null(question_label)) question_label <- selected_question()
      
      create_binary_bar(
        filtered_data(), 
        title = paste("Distribución de", question_label)
      )
    })
    
    # Pie chart
    output$pie_plot <- renderPlotly({
      req(filtered_data())
      question_label <- attr(filtered_data(), "question_label")
      if (is.null(question_label)) question_label <- selected_question()
      
      create_binary_pie(
        filtered_data(),
        title = paste("Distribución de", question_label)
      )
    })
    
    # District map
    output$district_map_plot <- renderLeaflet({
      req(filtered_data(), geo_data())
      create_binary_district_map(filtered_data(), geo_data())
    })
    
    # District bars
    output$district_bars_plot <- renderPlotly({
      req(filtered_data())
      create_binary_district_bars(
        filtered_data(),
        orientation = input$bar_orientation
      )
    })
    
    # Gender dumbbell plot
    output$gender_dumbbell_plot <- renderPlotly({
      req(filtered_data())
      create_binary_gender_dumbbell(filtered_data())
    })
    
    # Gender bars
    output$gender_bars_plot <- renderPlotly({
      req(filtered_data())
      create_binary_demographics_bars(filtered_data(), "gender")
    })
    
    # Age bars
    output$age_bars_plot <- renderPlotly({
      req(filtered_data())
      create_binary_demographics_bars(filtered_data(), "age_group")
    })
    
    # Multiple comparison
    output$multiple_comparison_plot <- renderPlotly({
      req(multiple_questions_data())
      create_multiple_binary_comparison(
        multiple_questions_data(),
        comparison_type = input$comparison_type,
        top_n = input$top_n
      )
    })
  })
}