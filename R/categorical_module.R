prepare_categorical_data <- function(data, question_id, metadata) {
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
  
  # Process value labels if they exist
  if (question_metadata$has_value_labels && !is.na(question_metadata$value_labels)) {
    tryCatch({
      # Split the labels string by semicolon
      label_pairs <- strsplit(question_metadata$value_labels, ";")[[1]]
      
      # Create mapping vectors
      values <- numeric()
      labels <- character()
      ns_nc_values <- numeric()
      
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
          
          values <- c(values, as.numeric(value))
          labels <- c(labels, label)
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
  
  # Process response types - Fixed version for vector comparison
  subset_data$response_type <- "VALID"  # Default value
  
  # Mark missing values
  subset_data$response_type[subset_data$is_na] <- "MISSING"
  
  # Mark NS/NC values if applicable
  if (!is.null(ns_nc_codes) && length(ns_nc_codes) > 0) {
    # Try to convert to numeric
    value_num <- suppressWarnings(as.numeric(subset_data$value))
    
    # Check which ones are NS/NC, but only for non-NA values
    for (i in 1:nrow(subset_data)) {
      if (!subset_data$is_na[i] && !is.na(value_num[i]) && value_num[i] %in% ns_nc_codes) {
        subset_data$response_type[i] <- "NS/NC"
      }
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
      age_group = character()
    )
    attr(empty_data, "has_labels") <- !is.null(value_labels)
    attr(empty_data, "value_labels") <- value_labels
    attr(empty_data, "ns_nc_count") <- ns_nc_count
    attr(empty_data, "missing_count") <- missing_count
    attr(empty_data, "total_responses") <- total_responses
    return(empty_data)
  }
  
  # Create factor for the categorical variable
  if (!is.null(value_labels)) {
    # For labeled data, map values to labels
    value_num <- suppressWarnings(as.numeric(valid_data$value))
    
    # Check if values are numeric and can be mapped
    if (!all(is.na(value_num))) {
      # Create a new factor variable with labeled levels
      valid_data$value <- factor(
        value_num,
        levels = as.numeric(names(value_labels)),
        labels = value_labels
      )
    } else {
      # For non-numeric values, just convert to factor
      valid_data$value <- as.factor(valid_data$value)
    }
  } else {
    # If no labels, just convert to factor as is
    valid_data$value <- as.factor(valid_data$value)
  }
  
  # Add attributes about the processing
  attr(valid_data, "has_labels") <- !is.null(value_labels)
  attr(valid_data, "value_labels") <- value_labels
  attr(valid_data, "ns_nc_count") <- ns_nc_count
  attr(valid_data, "missing_count") <- missing_count
  attr(valid_data, "total_responses") <- total_responses
  
  # Simplify the data frame to include only necessary columns
  valid_data <- valid_data %>%
    select(value, district, gender, age_group)
  
  return(valid_data)
}
create_category_bars <- function(data, max_categories = 15, title = "Distribución de Frecuencias") {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || !("value" %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Calculate frequency table
    freq_table <- table(data$value)
    if (length(freq_table) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay categorías para visualizar"))
    }
    
    freq_data <- data.frame(
      Category = names(freq_table),
      Frequency = as.vector(freq_table),
      Percentage = round(100 * as.vector(freq_table) / sum(freq_table), 2)
    )
    freq_data <- freq_data[order(-freq_data$Frequency), ]
    
    # Handle case with too many categories
    if (nrow(freq_data) > max_categories) {
      # Keep top categories and group others
      top_categories <- freq_data[1:max_categories, ]
      other_sum <- sum(freq_data$Frequency[(max_categories+1):nrow(freq_data)])
      other_percent <- sum(freq_data$Percentage[(max_categories+1):nrow(freq_data)])
      
      freq_data <- rbind(
        top_categories,
        data.frame(
          Category = "Otras categorías",
          Frequency = other_sum,
          Percentage = other_percent
        )
      )
    }
    
    # Create plot with horizontal layout for many categories
    if (nrow(freq_data) > 8) {
      p <- plot_ly(
        data = freq_data,
        y = ~reorder(Category, Frequency),
        x = ~Frequency,
        type = "bar",
        orientation = 'h',
        text = ~paste0(Category, ": ", Frequency, " (", Percentage, "%)"),
        hoverinfo = "text",
        marker = list(
          color = theme_config$colors$primary
        )
      ) %>%
        apply_plotly_theme(
          title = title,
          xlab = "Frecuencia",
          ylab = ""
        )
    } else {
      p <- plot_ly(
        data = freq_data,
        x = ~Category,
        y = ~Frequency,
        type = "bar",
        text = ~paste0(Category, ": ", Frequency, " (", Percentage, "%)"),
        hoverinfo = "text",
        marker = list(
          color = theme_config$colors$primary
        )
      ) %>%
        apply_plotly_theme(
          title = title,
          xlab = "Categoría",
          ylab = "Frecuencia"
        )
    }
    
    return(p)
  }, error = function(e) {
    warning(paste("Error in create_category_bars:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create pie chart
# Create pie chart (improved version)
create_category_pie <- function(data, max_categories = 8) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || !("value" %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Calculate frequency table
    freq_table <- table(data$value)
    if (length(freq_table) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay categorías para visualizar"))
    }
    
    freq_data <- data.frame(
      Category = names(freq_table),
      Frequency = as.vector(freq_table),
      Percentage = round(100 * as.vector(freq_table) / sum(freq_table), 2)
    )
    freq_data <- freq_data[order(-freq_data$Frequency), ]
    
    # Check if pie chart is appropriate (not too many categories)
    if (nrow(freq_data) > max_categories) {
      # Keep top categories and group others
      top_categories <- freq_data[1:max_categories, ]
      other_sum <- sum(freq_data$Frequency[(max_categories+1):nrow(freq_data)])
      other_percent <- sum(freq_data$Percentage[(max_categories+1):nrow(freq_data)])
      
      freq_data <- rbind(
        top_categories,
        data.frame(
          Category = "Otras categorías",
          Frequency = other_sum,
          Percentage = other_percent
        )
      )
    }
    
    # Create pie chart with custom colors
    colors <- colorRampPalette(c(theme_config$colors$primary, theme_config$colors$highlight))(nrow(freq_data))
    
    plot_ly(
      data = freq_data,
      labels = ~Category,
      values = ~Frequency,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      hoverinfo = "text",
      text = ~paste0(Category, ": ", Frequency, " respuestas")
    ) %>%
      layout(
        title = "Distribución de Categorías",
        showlegend = FALSE
      )
  }, error = function(e) {
    warning(paste("Error in create_category_pie:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create heatmap for district distribution
create_category_district_heatmap <- function(data, max_categories = 10) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || 
        !all(c("value", "district") %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Get top categories based on frequency
    freq_table <- table(data$value)
    if (length(freq_table) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay categorías para visualizar"))
    }
    
    top_categories <- names(sort(freq_table, decreasing = TRUE)[1:min(max_categories, length(freq_table))])
    
    # Filter data to include only top categories
    filtered_data <- data %>%
      filter(value %in% top_categories)
    
    # Calculate counts and percentages by district and category
    district_data <- filtered_data %>%
      count(district, value) %>%
      group_by(district) %>%
      mutate(percentage = round(100 * n / sum(n), 1)) %>%
      ungroup()
    
    # If no data after filtering, return error message
    if (nrow(district_data) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos para visualizar después de filtrar"))
    }
    
    # Create heatmap
    plot_ly(
      data = district_data,
      x = ~district,
      y = ~value,
      z = ~percentage,
      type = "heatmap",
      colorscale = "Blues",
      text = ~paste0(district, " - ", value, ": ", n, " (", percentage, "%)"),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Distribución por Distrito",
        xlab = "Distrito",
        ylab = "Categoría"
      ) %>%
      layout(
        xaxis = list(tickangle = 45),
        yaxis = list(
          categoryorder = "array",
          categoryarray = rev(top_categories)
        )
      )
  }, error = function(e) {
    warning(paste("Error in create_category_district_heatmap:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create stacked bar chart by district
create_category_stacked_bars <- function(data, max_categories = 7) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || 
        !all(c("value", "district") %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Get top categories based on frequency
    freq_table <- table(data$value)
    if (length(freq_table) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay categorías para visualizar"))
    }
    
    top_categories <- names(sort(freq_table, decreasing = TRUE)[1:min(max_categories, length(freq_table))])
    
    # Filter data to include only top categories
    filtered_data <- data %>%
      filter(value %in% top_categories)
    
    # For remaining data not in top categories, group as "Otros"
    if (length(freq_table) > max_categories) {
      other_data <- data %>%
        filter(!value %in% top_categories) %>%
        mutate(value = factor("Otras categorías"))
      
      # Combine with top categories
      filtered_data <- rbind(
        filtered_data,
        other_data
      )
    }
    
    # Calculate percentages by district
    district_data <- filtered_data %>%
      count(district, value) %>%
      group_by(district) %>%
      mutate(percentage = 100 * n / sum(n)) %>%
      ungroup()
    
    # If no data after filtering, return error message
    if (nrow(district_data) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos para visualizar después de filtrar"))
    }
    
    # Create stacked bar chart
    plot_ly(
      data = district_data,
      x = ~district,
      y = ~percentage,
      color = ~value,
      type = "bar",
      text = ~paste0(value, ": ", round(percentage, 1), "%"),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Distribución por Distrito",
        xlab = "Distrito",
        ylab = "Porcentaje"
      ) %>%
      layout(
        barmode = "stack",
        xaxis = list(tickangle = 45),
        legend = list(title = list(text = "Categoría"))
      )
  }, error = function(e) {
    warning(paste("Error in create_category_stacked_bars:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create treemap visualization
create_category_treemap <- function(data, include_demographics = FALSE) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || !("value" %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Prepare basic treemap data
    if (!include_demographics) {
      # Simple treemap by category
      freq_table <- table(data$value)
      
      # Prepare treemap data
      treemap_data <- data.frame(
        ids = names(freq_table),
        labels = names(freq_table),
        parents = rep("", length(freq_table)),
        values = as.numeric(freq_table)
      )
    } else {
      # More complex treemap with demographic hierarchy
      # Level 1: Overall
      treemap_data <- data.frame(
        ids = "Total",
        labels = "Total",
        parents = "",
        values = nrow(data)
      )
      
      # Level 2: By gender
      gender_counts <- data %>%
        count(gender) %>%
        mutate(
          ids = paste0("gender_", gender),
          labels = gender,
          parents = "Total"
        )
      
      treemap_data <- rbind(
        treemap_data,
        gender_counts %>% select(ids, labels, parents, values = n)
      )
      
      # Level 3: By gender and category
      gender_category_counts <- data %>%
        count(gender, value) %>%
        mutate(
          ids = paste0("gc_", gender, "_", value),
          labels = as.character(value),
          parents = paste0("gender_", gender)
        )
      
      treemap_data <- rbind(
        treemap_data,
        gender_category_counts %>% select(ids, labels, parents, values = n)
      )
    }
    
    # Create treemap
    plot_ly(
      data = treemap_data,
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      type = "treemap",
      branchvalues = "total",
      textinfo = "label+value+percent parent",
      hoverinfo = "label+value+percent parent",
      marker = list(
        colorscale = "Blues",
        line = list(width = 1)
      )
    ) %>%
      layout(
        title = ifelse(include_demographics, 
                     "Jerarquía de Categorías por Demografía", 
                     "Distribución de Categorías")
      )
  }, error = function(e) {
    warning(paste("Error in create_category_treemap:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create bar chart by gender
create_category_gender_bars <- function(data, max_categories = 5) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || 
        !all(c("value", "gender") %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Get top categories
    freq_table <- table(data$value)
    if (length(freq_table) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay categorías para visualizar"))
    }
    
    top_categories <- names(sort(freq_table, decreasing = TRUE)[1:min(max_categories, length(freq_table))])
    
    # Filter data to include only top categories
    filtered_data <- data %>%
      filter(value %in% top_categories)
    
    # Calculate percentages by gender
    gender_data <- filtered_data %>%
      count(gender, value) %>%
      group_by(gender) %>%
      mutate(percentage = round(100 * n / sum(n), 1)) %>%
      ungroup()
    
    if (nrow(gender_data) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos para visualizar después de filtrar"))
    }
    
    # Create grouped bar chart
    plot_ly(
      data = gender_data,
      x = ~value,
      y = ~percentage,
      color = ~gender,
      type = "bar",
      colors = get_color_palette("gender"),
      text = ~paste0(gender, " - ", value, ": ", n, " (", percentage, "%)"),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Distribución por Género",
        xlab = "Categoría",
        ylab = "Porcentaje"
      ) %>%
      layout(
        barmode = "group",
        legend = list(title = list(text = "Género"))
      )
  }, error = function(e) {
    warning(paste("Error in create_category_gender_bars:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create bar chart by age group
create_category_age_bars <- function(data, max_categories = 5) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || 
        !all(c("value", "age_group") %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Get top categories
    freq_table <- table(data$value)
    if (length(freq_table) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay categorías para visualizar"))
    }
    
    top_categories <- names(sort(freq_table, decreasing = TRUE)[1:min(max_categories, length(freq_table))])
    
    # Filter data to include only top categories
    filtered_data <- data %>%
      filter(value %in% top_categories)
    
    # Calculate percentages by age group
    age_data <- filtered_data %>%
      count(age_group, value) %>%
      group_by(age_group) %>%
      mutate(percentage = round(100 * n / sum(n), 1)) %>%
      ungroup()
    
    if (nrow(age_data) == 0) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos para visualizar después de filtrar"))
    }
    
    # Create grouped bar chart
    plot_ly(
      data = age_data,
      x = ~value,
      y = ~percentage,
      color = ~age_group,
      type = "bar",
      colors = get_color_palette("age_group"),
      text = ~paste0(age_group, " - ", value, ": ", n, " (", percentage, "%)"),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Distribución por Grupo de Edad",
        xlab = "Categoría",
        ylab = "Porcentaje"
      ) %>%
      layout(
        barmode = "group",
        legend = list(title = list(text = "Grupo de Edad"))
      )
  }, error = function(e) {
    warning(paste("Error in create_category_age_bars:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create chord diagram for category relationships
create_category_relationships <- function(data, cat_var1, cat_var2) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0 || 
        !all(c(cat_var1, cat_var2) %in% names(data))) {
      return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Create cross-tabulation
    cross_tab <- table(data[[cat_var1]], data[[cat_var2]])
    
    # Convert to matrix format for chord diagram
    matrix_data <- as.matrix(cross_tab)
    
    # Generate colors
    n_colors <- nrow(matrix_data) + ncol(matrix_data)
    colors <- colorRampPalette(c(theme_config$colors$primary, theme_config$colors$highlight))(n_colors)
    
    # Create labels
    labels <- c(rownames(matrix_data), colnames(matrix_data))
    
    # Create sankey diagram (as an alternative to chord diagram, which isn't directly available in plotly)
    # Prepare source-target pairs
    source <- vector()
    target <- vector()
    value <- vector()
    
    for (i in 1:nrow(matrix_data)) {
      for (j in 1:ncol(matrix_data)) {
        if (matrix_data[i, j] > 0) {
          source <- c(source, i-1)  # 0-indexed
          target <- c(target, j + nrow(matrix_data) - 1)  # offset by number of rows
          value <- c(value, matrix_data[i, j])
        }
      }
    }
    
    # Create sankey diagram
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = labels,
        color = colors,
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = source,
        target = target,
        value = value
      )
    ) %>%
      layout(
        title = paste("Relación entre", cat_var1, "y", cat_var2),
        font = list(size = 10)
      )
  }, error = function(e) {
    warning(paste("Error in create_category_relationships:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# UI Definition
categoricoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4,
        card(
          card_header("Controles de Visualización"),
          selectInput(
            ns("plot_type"),
            "Tipo de Visualización",
            choices = c(
              "Resumen Estadístico" = "summary",
              "Gráfico de Barras" = "bars",
              "Gráfico Circular" = "pie",
              "Mapa de Calor por Distrito" = "district_heatmap",
              "Barras Apiladas por Distrito" = "stacked_bars",
              "Barras por Género" = "gender_bars",
              "Barras por Edad" = "age_bars",
              "Gráfico de Árbol" = "treemap",
              "Gráfico de Árbol Jerárquico" = "hierarchical_treemap",
              "Relaciones entre Categorías" = "relationship"
            )
          ),
          
          # Add filter controls
          card_header("Filtros"),
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
          ),
          
          # Conditional panels for specific plot types
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bars'", ns("plot_type")),
            sliderInput(
              ns("max_categories"),
              "Número máximo de categorías",
              min = 5,
              max = 30,
              value = 15
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'pie'", ns("plot_type")),
            sliderInput(
              ns("pie_max_categories"),
              "Número máximo de categorías",
              min = 3,
              max = 15,
              value = 8
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'district_heatmap'", ns("plot_type")),
            sliderInput(
              ns("heatmap_max_categories"),
              "Número máximo de categorías",
              min = 3,
              max = 20,
              value = 10
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'stacked_bars'", ns("plot_type")),
            sliderInput(
              ns("stacked_max_categories"),
              "Número máximo de categorías",
              min = 3,
              max = 15,
              value = 7
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'relationship'", ns("plot_type")),
            selectInput(
              ns("rel_cat1"),
              "Primera variable",
              choices = c("district" = "district", 
                          "gender" = "gender", 
                          "age_group" = "age_group", 
                          "value" = "value"),
              selected = "value"
            ),
            selectInput(
              ns("rel_cat2"),
              "Segunda variable",
              choices = c("district" = "district", 
                          "gender" = "gender", 
                          "age_group" = "age_group", 
                          "value" = "value"),
              selected = "district"
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
# Server Definition
categoricoServer <- function(id, data, metadata, selected_question, geo_data) {
  moduleServer(id, function(input, output, session) {
    # Initial data preparation with metadata
    prepared_data <- reactive({
      tryCatch({
        req(data(), selected_question(), metadata())
        
        # Add validation
        if (is.null(selected_question()) || selected_question() == "") {
          return(NULL)
        }
        
        prepare_categorical_data(data(), selected_question(), metadata())
      }, error = function(e) {
        warning(paste("Error in prepared_data:", e$message))
        return(NULL)
      })
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
    
    # Dynamic plot output based on selection
    output$plot_output <- renderUI({
      plot_type <- input$plot_type
      
      switch(plot_type,
        "summary" = verbatimTextOutput(session$ns("summary_stats")),
        "bars" = plotlyOutput(session$ns("bars_plot"), height = "600px"),
        "pie" = plotlyOutput(session$ns("pie_plot"), height = "600px"),
        "district_heatmap" = plotlyOutput(session$ns("district_heatmap_plot"), height = "600px"),
        "stacked_bars" = plotlyOutput(session$ns("stacked_bars_plot"), height = "600px"),
        "gender_bars" = plotlyOutput(session$ns("gender_bars_plot"), height = "600px"),
        "age_bars" = plotlyOutput(session$ns("age_bars_plot"), height = "600px"),
        "treemap" = plotlyOutput(session$ns("treemap_plot"), height = "600px"),
        "hierarchical_treemap" = plotlyOutput(session$ns("hierarchical_treemap_plot"), height = "600px"),
        "relationship" = plotlyOutput(session$ns("relationship_plot"), height = "600px")
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
        
        # Get the response counts from attributes
        ns_nc_count <- attr(data, "ns_nc_count")
        missing_count <- attr(data, "missing_count")
        total_responses <- attr(data, "total_responses")
        valid_responses <- nrow(data)
        
        cat("Estadísticas para Datos Categóricos:\n")
        cat("\nDistribución de Respuestas:\n")
        cat("Total de respuestas:", total_responses, "\n")
        cat("Respuestas válidas:", valid_responses, "\n")
        cat("No sabe/No contesta:", ns_nc_count, 
            sprintf("(%.1f%%)", 100 * ns_nc_count/total_responses), "\n")
        cat("Datos faltantes:", missing_count,
            sprintf("(%.1f%%)", 100 * missing_count/total_responses), "\n")
        
        # Show frequency distribution
        freq_table <- table(data$value)
        cat("\nDistribución de Frecuencias:\n")
        freq_df <- data.frame(
          Categoría = names(freq_table),
          Frecuencia = as.vector(freq_table),
          Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2)
        )
        print(freq_df[order(-freq_df$Frecuencia), ])
        
        # Show mode
        if (length(freq_table) > 0) {
          mode_category <- names(freq_table)[which.max(freq_table)]
          mode_count <- max(freq_table)
          mode_percent <- round(100 * mode_count / sum(freq_table), 2)
          
          cat("\nEstadísticas Descriptivas:\n")
          cat("Categoría más frecuente:", mode_category, "\n")
          cat("Frecuencia:", mode_count, "\n")
          cat("Porcentaje:", mode_percent, "%\n")
          cat("Total de categorías únicas:", length(freq_table), "\n")
        }
        
        # Show district breakdown
        cat("\nDistribución por Distrito (Top 3 respuestas para cada distrito):\n")
        district_breakdown <- filtered_data() %>%
          group_by(district, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(district) %>%
          mutate(percentage = round(100 * count / sum(count), 1)) %>%
          slice_max(order_by = count, n = 3) %>%
          arrange(district, desc(count))
          
        print(district_breakdown)
        
        # Show gender breakdown
        cat("\nDistribución por Género:\n")
        gender_breakdown <- filtered_data() %>%
          group_by(gender, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(gender) %>%
          mutate(percentage = round(100 * count / sum(count), 1)) %>%
          slice_max(order_by = count, n = 3) %>%
          arrange(gender, desc(count))
          
        print(gender_breakdown)
        
      }, error = function(e) {
        cat("Error al generar estadísticas:", e$message)
      })
    })
    
    # Bar plot
    output$bars_plot <- renderPlotly({
      create_category_bars(
        filtered_data(), 
        max_categories = input$max_categories,
        title = paste("Distribución de", selected_question())
      )
    })
    
    # Pie chart
    output$pie_plot <- renderPlotly({
      create_category_pie(
        filtered_data(),
        max_categories = input$pie_max_categories
      )
    })
    
    # District heatmap
    output$district_heatmap_plot <- renderPlotly({
      create_category_district_heatmap(
        filtered_data(),
        max_categories = input$heatmap_max_categories
      )
    })
    
    # Stacked bars
    output$stacked_bars_plot <- renderPlotly({
      create_category_stacked_bars(
        filtered_data(),
        max_categories = input$stacked_max_categories
      )
    })
    
    # Gender bars
    output$gender_bars_plot <- renderPlotly({
      create_category_gender_bars(filtered_data())
    })
    
    # Age bars
    output$age_bars_plot <- renderPlotly({
      create_category_age_bars(filtered_data())
    })
    
    # Treemap
    output$treemap_plot <- renderPlotly({
      create_category_treemap(filtered_data(), include_demographics = FALSE)
    })
    
    # Hierarchical treemap
    output$hierarchical_treemap_plot <- renderPlotly({
      create_category_treemap(filtered_data(), include_demographics = TRUE)
    })
    
    # Relationship plot
    output$relationship_plot <- renderPlotly({
      create_category_relationships(
        filtered_data(),
        cat_var1 = input$rel_cat1,
        cat_var2 = input$rel_cat2
      )
    })
  })
}