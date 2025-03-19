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
      district = DISTRICT,  
      gender = GENDER,      
      age_group = AGE_GROUP 
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
  attr(valid_data, "question_label") <- get_question_label(question_id, metadata)

  # Simplify the data frame to include only necessary columns
  valid_data <- valid_data %>%
    select(value, district, gender, age_group)
  
  return(valid_data)
}

# Updated visualization functions with theme support
create_category_bars <- function(data, max_categories = 15, title = "Distribución de Frecuencias", custom_theme = NULL) {
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
    
    # Get bar color from theme
    bar_color <- if (!is.null(custom_theme)) {
      custom_theme$colors$primary
    } else {
      theme_config$colors$primary
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
          color = bar_color
        )
      ) %>%
        apply_plotly_theme(
          title = title,
          xlab = "Frecuencia",
          ylab = "",
          custom_theme = custom_theme
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
          color = bar_color
        )
      ) %>%
        apply_plotly_theme(
          title = title,
          xlab = "Categoría",
          ylab = "Frecuencia",
          custom_theme = custom_theme
        )
    }
    
    return(p)
  }, error = function(e) {
    warning(paste("Error in create_category_bars:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create pie chart (improved version with theme support)
create_category_pie <- function(data, max_categories = 8, custom_theme = NULL) {
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
    
    # Create custom colors if theme is provided
    colors <- if (!is.null(custom_theme)) {
      colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))(nrow(freq_data))
    } else {
      colorRampPalette(c(theme_config$colors$primary, theme_config$colors$highlight))(nrow(freq_data))
    }
    
    # Create pie chart with custom colors
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
        title = list(
          text = "Distribución de Categorías",
          font = if (!is.null(custom_theme)) {
            list(
              family = custom_theme$typography$font_family,
              size = custom_theme$typography$sizes$title,
              color = custom_theme$colors$text
            )
          } else {
            list(
              family = theme_config$typography$font_family,
              size = theme_config$typography$sizes$title,
              color = theme_config$colors$text
            )
          }
        ),
        showlegend = FALSE
      )
  }, error = function(e) {
    warning(paste("Error in create_category_pie:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create heatmap for district distribution with theme support
create_category_district_heatmap <- function(data, max_categories = 10, custom_theme = NULL) {
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
        ylab = "Categoría",
        custom_theme = custom_theme
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

# Create stacked bar chart by district with theme support
create_category_stacked_bars <- function(data, max_categories = 7, custom_theme = NULL) {
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
        ylab = "Porcentaje",
        custom_theme = custom_theme
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

# Create treemap visualization with theme support
create_category_treemap <- function(data, include_demographics = FALSE, custom_theme = NULL) {
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
    
    # Use colors from custom theme if provided
    colorscale <- if (!is.null(custom_theme)) {
      colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))(9)
    } else {
      "Blues"
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
        colorscale = colorscale,
        line = list(width = 1)
      )
    ) %>%
      layout(
        title = list(
          text = ifelse(include_demographics, 
                      "Jerarquía de Categorías por Demografía", 
                      "Distribución de Categorías"),
          font = if (!is.null(custom_theme)) {
            list(
              family = custom_theme$typography$font_family,
              size = custom_theme$typography$sizes$title,
              color = custom_theme$colors$text
            )
          } else {
            list(
              family = theme_config$typography$font_family,
              size = theme_config$typography$sizes$title,
              color = theme_config$colors$text
            )
          }
        )
      )
  }, error = function(e) {
    warning(paste("Error in create_category_treemap:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create bar chart by gender with theme support
create_category_gender_bars <- function(data, max_categories = 5, custom_theme = NULL) {
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
    
    # Get gender colors from theme if provided
    gender_colors <- if (!is.null(custom_theme)) {
      custom_theme$palettes$gender
    } else {
      get_color_palette("gender")
    }
    
    # Create grouped bar chart
    plot_ly(
      data = gender_data,
      x = ~value,
      y = ~percentage,
      color = ~gender,
      type = "bar",
      colors = gender_colors,
      text = ~paste0(gender, " - ", value, ": ", n, " (", percentage, "%)"),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Distribución por Género",
        xlab = "Categoría",
        ylab = "Porcentaje",
        custom_theme = custom_theme
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

# Create bar chart by age group with theme support
create_category_age_bars <- function(data, max_categories = 5, custom_theme = NULL) {
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
    
    # Get age group colors from theme if provided
    age_colors <- if (!is.null(custom_theme)) {
      custom_theme$palettes$age_group
    } else {
      get_color_palette("age_group")
    }
    
    # Create grouped bar chart
    plot_ly(
      data = age_data,
      x = ~value,
      y = ~percentage,
      color = ~age_group,
      type = "bar",
      colors = age_colors,
      text = ~paste0(age_group, " - ", value, ": ", n, " (", percentage, "%)"),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Distribución por Grupo de Edad",
        xlab = "Categoría",
        ylab = "Porcentaje",
        custom_theme = custom_theme
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

# Create chord diagram for category relationships with theme support
create_category_relationships <- function(data, cat_var1, cat_var2, custom_theme = NULL) {
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
    
    # Generate colors from theme
    n_colors <- nrow(matrix_data) + ncol(matrix_data)
    colors <- if (!is.null(custom_theme)) {
      colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))(n_colors)
    } else {
      colorRampPalette(c(theme_config$colors$primary, theme_config$colors$highlight))(n_colors)
    }
    
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
        title = list(
          text = paste("Relación entre", cat_var1, "y", cat_var2),
          font = if (!is.null(custom_theme)) {
            list(
              family = custom_theme$typography$font_family,
              size = custom_theme$typography$sizes$title,
              color = custom_theme$colors$text
            )
          } else {
            list(
              family = theme_config$typography$font_family,
              size = theme_config$typography$sizes$title,
              color = theme_config$colors$text
            )
          }
        ),
        font = list(size = 10)
      )
  }, error = function(e) {
    warning(paste("Error in create_category_relationships:", e$message))
    return(plotly_empty() %>% 
           layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create map for categories with theme support
create_category_district_map <- function(data, geo_data, selected_categories = NULL, highlight_extremes = TRUE, custom_theme = NULL) {
  # Check if we have data
  if (is.null(data) || nrow(data) == 0 || is.null(geo_data)) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get district colors from custom theme if provided
  district_colors <- if (!is.null(custom_theme)) {
    custom_theme$palettes$district
  } else {
    theme_config$palettes$district
  }
  
  # Handle case when no categories are selected
  if (is.null(selected_categories) || length(selected_categories) == 0) {
    # Default mode: Show counts of each category by district
    district_counts <- data %>%
      count(district, value) %>%
      group_by(district) %>%
      mutate(percentage = round(100 * n / sum(n), 1)) %>%
      ungroup()
    
    # Get most common category for each district
    district_modes <- district_counts %>%
      group_by(district) %>%
      slice_max(order_by = n, n = 1) %>%
      ungroup() %>%
      select(district, value, count = n, percentage)
    
    # Create map
    return(leaflet(geo_data) %>%
      addTiles() %>% 
      addPolygons(
        fillOpacity = 0.7,
        weight = 1,
        color = district_colors[match(geo_data$No_Distrit, as.numeric(district_modes$district))],
        dashArray = "3",
        highlight = highlightOptions(
          weight = 2,
          color = "#666666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~sprintf(
          "Distrito: %s<br>Categoría más común: %s<br>Frecuencia: %d (%.1f%%)",
          district_modes$district[match(No_Distrit, district_modes$district)],
          district_modes$value[match(No_Distrit, district_modes$district)],
          district_modes$count[match(No_Distrit, district_modes$district)],
          district_modes$percentage[match(No_Distrit, district_modes$district)]
        ) %>% lapply(HTML)
      ))
  }
  
  # For selected categories, calculate percentage
  district_stats <- data %>%
    mutate(
      is_selected = value %in% selected_categories
    ) %>%
    group_by(district) %>%
    summarise(
      total_responses = n(),
      selected_count = sum(is_selected, na.rm = TRUE),
      selected_percent = round(100 * sum(is_selected, na.rm = TRUE) / n(), 1),
      .groups = 'drop'
    )
  
  # Calculate centroids for label placement
  geo_data$centroid <- sf::st_centroid(geo_data$geometry)
  centroids <- sf::st_coordinates(geo_data$centroid)
  geo_data$lng <- centroids[,1]
  geo_data$lat <- centroids[,2]
  
  # Find district with highest and lowest percentages if highlighting extremes
  highest_district <- lowest_district <- NULL
  if (highlight_extremes) {
    highest_district <- district_stats %>% arrange(desc(selected_percent)) %>% slice(1)
    lowest_district <- district_stats %>% arrange(selected_percent) %>% slice(1)
  }
  
  # Create list of selected categories for title
  categories_text <- paste(selected_categories, collapse = ", ")
  if (nchar(categories_text) > 100) {
    categories_text <- paste0(substr(categories_text, 1, 97), "...")
  }
  
  # Create map
  map <- leaflet(geo_data) %>%
    addTiles() %>% 
    addPolygons(
      fillOpacity = 0.7,
      weight = 1,
      color = district_colors[match(geo_data$No_Distrit, as.numeric(district_stats$district))],
      dashArray = "3",
      highlight = highlightOptions(
        weight = 2,
        color = "#666666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "Distrito: %s<br>Porcentaje: %s%%<br>Respuestas: %d/%d",
        district_stats$district[match(No_Distrit, district_stats$district)],
        district_stats$selected_percent[match(No_Distrit, district_stats$district)],
        district_stats$selected_count[match(No_Distrit, district_stats$district)],
        district_stats$total_responses[match(No_Distrit, district_stats$district)]
      ) %>% lapply(HTML)
    )
  
  # Add labels for each district
  for(i in 1:nrow(geo_data)) {
    district_num <- geo_data$No_Distrit[i]
    percent <- district_stats$selected_percent[match(district_num, district_stats$district)]
    
    # Skip if no percentage data
    if(is.na(percent)) next
    
    # Determine background color based on whether this is highest or lowest district
    bg_color <- "#FFFFFF"  # Default white background
    text_color <- "#000000"  # Default black text
    
    if(highlight_extremes) {
      if(!is.null(highest_district) && !is.na(highest_district$district) && district_num == highest_district$district) {
        bg_color <- "#87CEEB"  # Light blue background for highest
        text_color <- "#000000"  # Black text
      } else if(!is.null(lowest_district) && !is.na(lowest_district$district) && district_num == lowest_district$district) {
        bg_color <- "#012A4A"  # Dark blue background for lowest
        text_color <- "#FFFFFF"  # White text
      }
    }
    
    # Create label HTML
    label_html <- sprintf(
      '<div style="background-color: %s; color: %s; padding: 5px; border-radius: 3px; font-weight: bold;">Distrito %s<br>%s%%</div>',
      bg_color, text_color, district_num, percent
    )
    
    # Add label
    map <- map %>% addLabelOnlyMarkers(
      lng = geo_data$lng[i],
      lat = geo_data$lat[i],
      label = lapply(list(label_html), HTML),
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE
      )
    )
  }
  
  # Add overall average label
  overall_percent <- round(mean(district_stats$selected_percent, na.rm = TRUE), 1)
  
  map <- map %>% addControl(
    html = sprintf(
      '<div style="background-color: #333333; color: white; padding: 5px; border-radius: 3px;"><strong>Porcentaje general: %s%%</strong></div>',
      overall_percent
    ),
    position = "topright"
  )
  
  return(map)
}

# UI Definition
categoricoUI <- function(id) {
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
              "Mapa de Calor por Distrito" = "district_heatmap",
              "Mapa Geográfico por Distrito" = "district_map", # Add new option here
              "Barras Apiladas por Distrito" = "stacked_bars",
              "Barras por Género" = "gender_bars",
              "Barras por Edad" = "age_bars",
              "Gráfico de Árbol" = "treemap",
              "Gráfico de Árbol Jerárquico" = "hierarchical_treemap",
              "Relaciones entre Categorías" = "relationship"
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
   condition = sprintf("input['%s'] == 'summary'", ns("plot_type")),
   div(
     style = "margin-top: 15px;",
     downloadButton(ns("download_summary_csv"), "Descargar Resumen (CSV)"),
     br(),
     br(),
     downloadButton(ns("download_summary_excel"), "Descargar Resumen (Excel)")
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
          )
          ),
            accordion_panel(
              "Opciones Adicionales",
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
          ),
          
          # Add new conditional panel for district map options
          conditionalPanel(
            condition = sprintf("input['%s'] == 'district_map'", ns("plot_type")),
            checkboxGroupInput(
              ns("map_categories"),
              "Categorías a incluir en el porcentaje:",
              choices = NULL # Will be populated dynamically
            ),
            checkboxInput(
              ns("highlight_extremes"),
              "Resaltar valores extremos",
              value = TRUE
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

# Server Definition - Updated to accept custom theme
categoricoServer <- function(id, data, metadata, selected_question, geo_data, current_theme = NULL) {
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
    observe({
      req(filtered_data())
      data <- filtered_data()
      
      if(is.null(data) || nrow(data) == 0) {
        return()
      }
      
      # Get unique categories
      unique_categories <- unique(data$value)
      
      # Sort alphabetically
      unique_categories <- sort(as.character(unique_categories))
      
      # Limit to top 15 most common categories if there are too many
      if(length(unique_categories) > 15) {
        category_counts <- table(data$value)
        top_categories <- names(sort(category_counts, decreasing = TRUE)[1:15])
        unique_categories <- sort(top_categories)
      }
      
      # Update the choices
      updateCheckboxGroupInput(session, "map_categories", 
                               choices = unique_categories,
                               selected = NULL)
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
        "district_map" = leafletOutput(session$ns("district_map_plot"), height = "600px"),
        "stacked_bars" = plotlyOutput(session$ns("stacked_bars_plot"), height = "600px"),
        "gender_bars" = plotlyOutput(session$ns("gender_bars_plot"), height = "600px"),
        "age_bars" = plotlyOutput(session$ns("age_bars_plot"), height = "600px"),
        "treemap" = plotlyOutput(session$ns("treemap_plot"), height = "600px"),
        "hierarchical_treemap" = plotlyOutput(session$ns("hierarchical_treemap_plot"), height = "600px"),
        "relationship" = plotlyOutput(session$ns("relationship_plot"), height = "600px")
      )
    })
    
    # The statistical summary doesn't use theme elements, so it remains unchanged
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
        
        # Show overall frequency distribution
        freq_table <- table(data$value)
        cat("\nDistribución de Frecuencias (General):\n")
        freq_df <- data.frame(
          Categoría = names(freq_table),
          Frecuencia = as.vector(freq_table),
          Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2)
        )
        
        # Sort by frequency and print all rows
        freq_df <- freq_df[order(-freq_df$Frecuencia), ]
        # Force print all categories - no limit
        print(freq_df, n = nrow(freq_df), na.print = "NA")
        
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
        
        # Show distribution by district - most common category per district
        cat("\nDistribución por Distrito (categoría más frecuente para cada distrito):\n")
        district_breakdown <- data %>%
          group_by(district, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(district) %>%
          mutate(percentage = round(100 * count / sum(count), 1)) %>%
          slice_max(order_by = count, n = 1) %>%
          arrange(district)
          
        print(district_breakdown %>% select(district, Categoría = value, Frecuencia = count, Porcentaje = percentage), n = nrow(district_breakdown))
        
        # Instead of printing all district-category combinations directly, 
        # show district by district with top categories for better readability
        cat("\nDistribución Detallada por Distrito (Top 3 categorías por distrito):\n")
        
        # Process each district separately and print with cat() to avoid tibble shortening
        district_list <- unique(data$district)
        for (d in district_list) {
          cat("\n--- Distrito", d, "---\n")
          
          dist_data <- data %>%
            filter(district == d) %>%
            group_by(value) %>%
            summarise(
              count = n(),
              percentage = round(100 * count / sum(nrow(data[data$district == d,])), 1),
              .groups = 'drop'
            ) %>%
            arrange(desc(count)) %>%
            head(5)  # Show top 5 categories
          
          # Create formatted output
          for (i in 1:nrow(dist_data)) {
            cat(sprintf("%s: %d (%0.1f%%)\n", 
                        dist_data$value[i], 
                        dist_data$count[i], 
                        dist_data$percentage[i]))
          }
        }
        
        # Show gender breakdown
        cat("\nDistribución por Género (categoría más frecuente para cada género):\n")
        gender_breakdown <- data %>%
          group_by(gender, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(gender) %>%
          mutate(percentage = round(100 * count / sum(count), 1)) %>%
          slice_max(order_by = count, n = 1) %>%
          arrange(gender)
          
        print(gender_breakdown %>% select(gender, Categoría = value, Frecuencia = count, Porcentaje = percentage), n = nrow(gender_breakdown))
        
        # Show detailed gender breakdown
        cat("\nDistribución Detallada por Género:\n")
        for (g in unique(data$gender)) {
          cat("\n--- Género:", g, "---\n")
          
          gender_data <- data %>%
            filter(gender == g) %>%
            group_by(value) %>%
            summarise(
              count = n(),
              percentage = round(100 * count / sum(nrow(data[data$gender == g,])), 1),
              .groups = 'drop'
            ) %>%
            arrange(desc(count)) %>%
            head(5)  # Show top 5 categories
          
          # Create formatted output
          for (i in 1:nrow(gender_data)) {
            cat(sprintf("%s: %d (%0.1f%%)\n", 
                       gender_data$value[i], 
                       gender_data$count[i], 
                       gender_data$percentage[i]))
          }
        }
        
        # Show age group breakdown
        cat("\nDistribución por Grupo de Edad (categoría más frecuente para cada grupo):\n")
        age_breakdown <- data %>%
          group_by(age_group, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(age_group) %>%
          mutate(percentage = round(100 * count / sum(count), 1)) %>%
          slice_max(order_by = count, n = 1) %>%
          arrange(age_group)
          
        print(age_breakdown %>% select(age_group, Categoría = value, Frecuencia = count, Porcentaje = percentage), n = nrow(age_breakdown))
        
        # Show detailed age breakdown
        cat("\nDistribución Detallada por Grupo de Edad:\n")
        for (a in unique(data$age_group)) {
          cat("\n--- Grupo de Edad:", a, "---\n")
          
          age_data <- data %>%
            filter(age_group == a) %>%
            group_by(value) %>%
            summarise(
              count = n(),
              percentage = round(100 * count / sum(nrow(data[data$age_group == a,])), 1),
              .groups = 'drop'
            ) %>%
            arrange(desc(count)) %>%
            head(5)  # Show top 5 categories
          
          # Create formatted output
          for (i in 1:nrow(age_data)) {
            cat(sprintf("%s: %d (%0.1f%%)\n", 
                       age_data$value[i], 
                       age_data$count[i], 
                       age_data$percentage[i]))
          }
        }
        
      }, error = function(e) {
        cat("Error al generar estadísticas:", e$message)
      })
    })
    
    # Generate summary tables for download - these don't use theme elements either
    summary_tables <- reactive({
      data <- filtered_data()
      
      # Calculate overall frequencies
      freq_table <- as.data.frame(table(data$value))
      names(freq_table) <- c("Categoría", "Frecuencia")
      freq_table$Porcentaje <- round(100 * freq_table$Frecuencia / sum(freq_table$Frecuencia), 2)
      freq_table <- freq_table[order(-freq_table$Frecuencia), ]
      
      # Get the most frequent category
      if (nrow(freq_table) > 0) {
        mode_category <- freq_table$Categoría[1]
        mode_count <- freq_table$Frecuencia[1]
        mode_percent <- freq_table$Porcentaje[1]
      } else {
        mode_category <- NA
        mode_count <- NA
        mode_percent <- NA
      }
      
      # Create a summary table
      overall_stats <- data.frame(
        Statistic = c("Categoría más frecuente", "Frecuencia", "Porcentaje", "Categorías únicas", 
                      "Total de respuestas", "No sabe/No contesta", "Datos faltantes"),
        Value = c(
          as.character(mode_category),
          as.character(mode_count),
          paste0(mode_percent, "%"),
          as.character(nrow(freq_table)),
          as.character(attr(data, "total_responses")),
          as.character(attr(data, "ns_nc_count")),
          as.character(attr(data, "missing_count"))
        )
      )
      
      # Create district breakdown - ALL categories for each district
      district_breakdown <- data %>%
        group_by(district, value) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(district) %>%
        mutate(percentage = round(100 * count / sum(count), 1)) %>%
        arrange(district, desc(count))
      
      # Create gender breakdown - ALL categories for each gender
      gender_breakdown <- data %>%
        group_by(gender, value) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(gender) %>%
        mutate(percentage = round(100 * count / sum(count), 1)) %>%
        arrange(gender, desc(count))
      
      # Create age group breakdown - ALL categories for each age group
      age_breakdown <- data %>%
        group_by(age_group, value) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(age_group) %>%
        mutate(percentage = round(100 * count / sum(count), 1)) %>%
        arrange(age_group, desc(count))
      
      # Create contingency tables
      district_contingency <- as.data.frame.matrix(table(data$district, data$value))
      gender_contingency <- as.data.frame.matrix(table(data$gender, data$value))
      age_contingency <- as.data.frame.matrix(table(data$age_group, data$value))
      
      # Add row totals
      district_contingency$Total <- rowSums(district_contingency)
      gender_contingency$Total <- rowSums(gender_contingency)
      age_contingency$Total <- rowSums(age_contingency)
      
      return(list(
        overall = overall_stats,
        frequencies = freq_table,
        district = district_breakdown,
        gender = gender_breakdown,
        age = age_breakdown,
        district_table = district_contingency,
        gender_table = gender_contingency,
        age_table = age_contingency
      ))
    })
    
    # CSV download handler - unchanged
    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("resumen_categorico_", selected_question(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        summaries <- summary_tables()
        
        # Create temporary directory for files
        temp_dir <- tempdir()
        
        # Write each summary to a CSV file
        write.csv(summaries$overall, file.path(temp_dir, "estadisticas_generales.csv"), row.names = FALSE)
        write.csv(summaries$frequencies, file.path(temp_dir, "tabla_frecuencias.csv"), row.names = FALSE)
        write.csv(summaries$district, file.path(temp_dir, "distribucion_por_distrito.csv"), row.names = FALSE)
        write.csv(summaries$gender, file.path(temp_dir, "distribucion_por_genero.csv"), row.names = FALSE)
        write.csv(summaries$age, file.path(temp_dir, "distribucion_por_edad.csv"), row.names = FALSE)
        write.csv(summaries$district_table, file.path(temp_dir, "tabla_contingencia_distrito.csv"))
        write.csv(summaries$gender_table, file.path(temp_dir, "tabla_contingencia_genero.csv"))
        write.csv(summaries$age_table, file.path(temp_dir, "tabla_contingencia_edad.csv"))
        
        # Save current working directory
        oldwd <- getwd()
        
        # Change to temp directory before zipping
        setwd(temp_dir)
        
        # Create zip file with all CSVs - using just filenames (not full paths)
        files_to_zip <- c(
          "estadisticas_generales.csv",
          "tabla_frecuencias.csv",
          "distribucion_por_distrito.csv",
          "distribucion_por_genero.csv",
          "distribucion_por_edad.csv",
          "tabla_contingencia_distrito.csv",
          "tabla_contingencia_genero.csv",
          "tabla_contingencia_edad.csv"
        )
        
        zip(file, files_to_zip)
        
        # Change back to original working directory
        setwd(oldwd)
      }
    )
    
    # Excel download handler - unchanged
    output$download_summary_excel <- downloadHandler(
      filename = function() {
        paste0("resumen_categorico_", selected_question(), "_", Sys.Date(), ".xlsx")
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
        
        openxlsx::addWorksheet(wb, "Tabla de Frecuencias")
        openxlsx::writeData(wb, "Tabla de Frecuencias", summaries$frequencies)
        
        openxlsx::addWorksheet(wb, "Por Distrito")
        openxlsx::writeData(wb, "Por Distrito", summaries$district)
        
        openxlsx::addWorksheet(wb, "Por Género")
        openxlsx::writeData(wb, "Por Género", summaries$gender)
        
        openxlsx::addWorksheet(wb, "Por Grupo de Edad")
        openxlsx::writeData(wb, "Por Grupo de Edad", summaries$age)
        
        openxlsx::addWorksheet(wb, "Tabla Distrito-Categoría")
        openxlsx::writeData(wb, "Tabla Distrito-Categoría", summaries$district_table, rowNames = TRUE)
        
        openxlsx::addWorksheet(wb, "Tabla Género-Categoría")
        openxlsx::writeData(wb, "Tabla Género-Categoría", summaries$gender_table, rowNames = TRUE)
        
        openxlsx::addWorksheet(wb, "Tabla Edad-Categoría")
        openxlsx::writeData(wb, "Tabla Edad-Categoría", summaries$age_table, rowNames = TRUE)
        
        # Save workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Update plot outputs to use the active theme
    output$bars_plot <- renderPlotly({
      req(filtered_data())
      create_category_bars(
        filtered_data(), 
        max_categories = input$max_categories,
        title = paste("Distribución de", selected_question()),
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$pie_plot <- renderPlotly({
      req(filtered_data())
      create_category_pie(
        filtered_data(),
        max_categories = input$pie_max_categories,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$district_heatmap_plot <- renderPlotly({
      req(filtered_data())
      create_category_district_heatmap(
        filtered_data(),
        max_categories = input$heatmap_max_categories,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$district_map_plot <- renderLeaflet({
      req(filtered_data(), geo_data())
      create_category_district_map(
        filtered_data(),
        geo_data(),
        selected_categories = input$map_categories,
        highlight_extremes = input$highlight_extremes,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$stacked_bars_plot <- renderPlotly({
      req(filtered_data())
      create_category_stacked_bars(
        filtered_data(),
        max_categories = input$stacked_max_categories,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$gender_bars_plot <- renderPlotly({
      req(filtered_data())
      create_category_gender_bars(
        filtered_data(),
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$age_bars_plot <- renderPlotly({
      req(filtered_data())
      create_category_age_bars(
        filtered_data(),
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$treemap_plot <- renderPlotly({
      req(filtered_data())
      create_category_treemap(
        filtered_data(), 
        include_demographics = FALSE,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$hierarchical_treemap_plot <- renderPlotly({
      req(filtered_data())
      create_category_treemap(
        filtered_data(), 
        include_demographics = TRUE,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$relationship_plot <- renderPlotly({
      req(filtered_data())
      create_category_relationships(
        filtered_data(),
        cat_var1 = input$rel_cat1,
        cat_var2 = input$rel_cat2,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
  })
}