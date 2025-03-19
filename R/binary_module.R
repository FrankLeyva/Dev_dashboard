prepare_binary_data <- function(data, question_id, metadata, treat_na_as_negative = NULL) {
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
  
  # Set default value for treat_na_as_negative if not provided
  local_treat_na_as_negative <- if(is.null(treat_na_as_negative)) FALSE else treat_na_as_negative
  
  is_checkbox_question <- FALSE
  invert_selected_logic <- FALSE
  
  # SPECIFIC QUESTION HANDLERS - Add special case handling for known problematic questions
  if (grepl("^Q(6|17)\\.[0-9]+$", question_id)) {
    # For questions like Q6.x and Q17.x in PAR survey - these are checkbox questions
    # where a value of 1 means "Selected" and NA means "Not Selected"
    is_checkbox_question <- TRUE
    local_treat_na_as_negative <- TRUE  # Override for these special cases
    invert_selected_logic <- FALSE
    
    # Add debugging for these specific questions
    cat(sprintf("Handling checkbox question %s with special logic\n", question_id))
  } else {
    is_checkbox_question <- FALSE
  }
  
  # Process value labels if they exist
  if (question_metadata$has_value_labels && !is.na(question_metadata$value_labels)) {
    tryCatch({
      # Split the labels string by semicolon
      label_pairs <- strsplit(question_metadata$value_labels, ";")[[1]]
      
      # Create mapping vectors
      values <- character()
      labels <- character()
      
      # Check if this is a Selected/Not Selected question
      is_selection_type <- any(grepl("Selected", label_pairs, fixed = TRUE))
      if (is_selection_type && !is_checkbox_question) {
        is_checkbox_question <- TRUE
        local_treat_na_as_negative <- TRUE
      }
      
      # Process each pair
      for(pair in label_pairs) {
        # Split by equals and clean up
        parts <- strsplit(trimws(pair), "=")[[1]]
        if(length(parts) == 2) {
          value <- trimws(parts[1])
          label <- trimws(parts[2])
          
          values <- c(values, value)
          labels <- c(labels, label)
          
          # Handle inverted label logic
          if (label == "Selected" && value == "0") {
            invert_selected_logic <- TRUE
          }
          if (label == "Not Selected" && value == "1") {
            invert_selected_logic <- TRUE
          }
          
          # Handle standard Yes/No questions
          if (value == "2" && grepl("^no\\s|^no$|no\\s", tolower(label), ignore.case = TRUE)) {
            negative_values <- c(negative_values, "2")
          }
          
          if (value == "1" && grepl("^no\\s|^no$|no\\s", tolower(label), ignore.case = TRUE)) {
            negative_values <- c(negative_values, "1")
            positive_values <- positive_values[positive_values != "1"]
          }
          
          if (value == "2" && grepl("^si\\s|^sí\\s|^si$|^sí$", tolower(label), ignore.case = TRUE)) {
            positive_values <- c(positive_values, "2")
          }
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
      district = DISTRICT, 
      gender = GENDER,
      age_group = AGE_GROUP
    )

  # Handle NA values
  subset_data$value_original <- subset_data$value
  subset_data$is_na <- is.na(subset_data$value)
  
  # Check distribution of data to determine if we need to invert logic
  if (!is_checkbox_question) {
    # For non-checkbox questions, check if this might be a checkbox type
    # based on data distribution
    if (mean(subset_data$is_na) > 0.5) {
      # If most values are NA, this is likely a checkbox question
      is_checkbox_question <- TRUE
      local_treat_na_as_negative <- TRUE
    }
  }
  
  # For checkbox questions, analyze distribution to determine logic
  if (is_checkbox_question) {
    # Count occurrences explicitly
    ones_count <- sum(subset_data$value == "1" | subset_data$value == "Selected", na.rm = TRUE)
    zeros_count <- sum(subset_data$value == "0" | subset_data$value == "Not Selected", na.rm = TRUE)
    na_count <- sum(subset_data$is_na)
    
    # Store these for debugging
    attr(subset_data, "ones_count") <- ones_count
    attr(subset_data, "zeros_count") <- zeros_count
    attr(subset_data, "na_count") <- na_count
    
    # If ones are rare, this is a standard checkbox (1=checked, uncommon)
    if (ones_count < nrow(subset_data) * 0.1 && ones_count > 0) {
      invert_selected_logic <- FALSE
    }
    # If zeros are rare, this might be an inverted checkbox
    else if (zeros_count < nrow(subset_data) * 0.1 && zeros_count > 0) {
      invert_selected_logic <- TRUE
    }
  }
  
  # If we need to invert the logic, swap positive and negative values
  if (invert_selected_logic) {
    temp <- positive_values
    positive_values <- negative_values
    negative_values <- temp
  }
  
  # CRITICAL FIX: Special handling for specific questions from PAR survey
  if (grepl("^Q(6|17)\\.[0-9]+$", question_id)) {
    # For these questions, 1 = Selected, everything else = Not Selected
    # Ensure positive_values contains "1" and "Selected"
    positive_values <- unique(c("1", "Selected", positive_values))
    negative_values <- negative_values[!negative_values %in% positive_values]
  }
  
  # Create binary values - this is the crucial part
  subset_data <- subset_data %>%
    mutate(
      binary_value = case_when(
        # For checkbox questions, NA typically means "not checked"
        is_na & is_checkbox_question ~ FALSE,
        # For other questions, follow the user's preference
        is_na & local_treat_na_as_negative ~ FALSE,
        is_na & !local_treat_na_as_negative ~ NA,
        # Process actual values
        value %in% positive_values ~ TRUE,
        value %in% negative_values ~ FALSE,
        as.character(value) == "1" & !"1" %in% negative_values ~ TRUE,
        as.character(value) == "0" & !"0" %in% positive_values ~ FALSE,
        # Handle text values
        toupper(value) == "TRUE" ~ TRUE,
        toupper(value) == "FALSE" ~ FALSE,
        toupper(value) == "YES" ~ TRUE,
        toupper(value) == "NO" ~ FALSE,
        toupper(value) == "SELECTED" ~ TRUE,
        toupper(value) == "NOT SELECTED" ~ FALSE,
        # Add pattern matching for strings that start with Sí/Si or No
        grepl("^Sí|^Si", value, ignore.case = TRUE) ~ TRUE,
        grepl("^No", value, ignore.case = TRUE) ~ FALSE,
        # Default to NA for unhandled cases
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
    attr(empty_data, "treat_na_as_negative") <- local_treat_na_as_negative
    attr(empty_data, "invert_selected_logic") <- invert_selected_logic
    attr(empty_data, "is_checkbox_question") <- is_checkbox_question
    attr(empty_data, "positive_values") <- positive_values
    attr(empty_data, "negative_values") <- negative_values
    return(empty_data)
  }
  
  # Add attributes to the result
  attr(valid_data, "has_labels") <- !is.null(value_labels)
  attr(valid_data, "value_labels") <- value_labels
  attr(valid_data, "missing_count") <- missing_count
  attr(valid_data, "total_responses") <- total_responses
  attr(valid_data, "question_id") <- question_id
  attr(valid_data, "question_label") <- question_metadata$label
  attr(valid_data, "treat_na_as_negative") <- local_treat_na_as_negative
  attr(valid_data, "invert_selected_logic") <- invert_selected_logic
  attr(valid_data, "is_checkbox_question") <- is_checkbox_question
  attr(valid_data, "positive_values") <- positive_values
  attr(valid_data, "negative_values") <- negative_values
  
  return(valid_data)
}
# Helper function to consistently get binary labels across all visualizations
get_binary_labels <- function(data) {
  value_labels <- attr(data, "value_labels")
  positive_values <- attr(data, "positive_values")
  negative_values <- attr(data, "negative_values")
  is_checkbox <- attr(data, "is_checkbox_question")
  
  # First, handle any special cases with no data
  if (nrow(data) == 0) {
    # Default labels for empty data
    return(list(true_label = "Sí", false_label = "No"))
  }
  
  # Check if this is a special case where 1=No, 2=Yes
  is_reversed <- "1" %in% negative_values && "2" %in% positive_values
  
  # Count actual value occurrences in original data
  value_counts <- table(data$value_original, useNA = "ifany")
  
  # Initialize labels
  true_label <- NULL
  false_label <- NULL
  
  # Try to get the label for positive values (TRUE)
  if (!is.null(value_labels) && length(value_labels) >= 1) {
    # Special handling for common patterns
    if (is_reversed) {
      # If 1=No, 2=Yes - use label for "2"
      if ("2" %in% names(value_labels)) {
        true_label <- value_labels["2"]
      }
    } else if ("1" %in% positive_values && "1" %in% names(value_labels)) {
      # Standard case: 1=Yes
      true_label <- value_labels["1"]
    }
    
    # If no label yet, try to find which positive values are in the data
    if (is.null(true_label)) {
      pos_in_data <- intersect(names(value_counts), as.character(positive_values))
      if (length(pos_in_data) > 0) {
        # Use most common positive value
        most_common_pos <- pos_in_data[which.max(as.vector(value_counts[pos_in_data]))]
        if (most_common_pos %in% names(value_labels)) {
          true_label <- value_labels[most_common_pos]
        }
      }
    }
    
    # Final fallback for positive label
    if (is.null(true_label)) {
      # Try any positive value with a label
      for (val in positive_values) {
        val_str <- as.character(val)
        if (val_str %in% names(value_labels)) {
          true_label <- value_labels[val_str]
          break
        }
      }
      if (is.null(true_label)) true_label <- "Sí"
    }
    
    # Now get the label for negative values (FALSE)
    if (is_reversed) {
      # If 1=No, 2=Yes - use label for "1"
      if ("1" %in% names(value_labels)) {
        false_label <- value_labels["1"]
      }
    } else {
      # Try to find which negative values are in the data
      neg_in_data <- intersect(names(value_counts), as.character(negative_values))
      if (length(neg_in_data) > 0) {
        # Use most common negative value
        most_common_neg <- neg_in_data[which.max(as.vector(value_counts[neg_in_data]))]
        if (most_common_neg %in% names(value_labels)) {
          false_label <- value_labels[most_common_neg]
        }
      }
    }
    
    # If still no label, try common negative values
    if (is.null(false_label)) {
      if ("2" %in% negative_values && "2" %in% names(value_labels)) {
        false_label <- value_labels["2"]
      } else if ("0" %in% negative_values && "0" %in% names(value_labels)) {
        false_label <- value_labels["0"]
      }
    }
    
    # Final fallback for negative label
    if (is.null(false_label)) {
      # Try any negative value
      for (val in negative_values) {
        val_str <- as.character(val)
        if (val_str %in% names(value_labels)) {
          false_label <- value_labels[val_str]
          break
        }
      }
      if (is.null(false_label)) false_label <- "No"
    }
  } else {
    # Default labels if no value_labels available
    true_label <- "Sí"
    false_label <- "No"
  }
  
  # Fix for checkbox questions
  if (is_checkbox) {
    if (true_label == "Not Selected" && false_label == "Selected") {
      temp <- true_label
      true_label <- false_label
      false_label <- temp
    }
  }
  
  return(list(true_label = true_label, false_label = false_label))
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
create_binary_bar <- function(data, title = "Distribución de Respuestas", custom_theme = NULL) {
  # Calculate proportions
  response_counts <- table(data$binary_value)
  
  # Create data frame for plotting
  if (length(response_counts) == 0) {
    return(plotly_empty() %>% 
           layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get correct labels using the helper function
  labels <- get_binary_labels(data)
  true_label <- labels$true_label
  false_label <- labels$false_label
  
  # Count actual TRUE/FALSE values
  true_count <- sum(data$binary_value, na.rm = TRUE)
  false_count <- sum(!data$binary_value, na.rm = TRUE)
  
  plot_df <- data.frame(
    Response = c(true_label, false_label),
    Count = c(true_count, false_count),
    Percentage = c(
      round(100 * true_count / (true_count + false_count), 1),
      round(100 * false_count / (true_count + false_count), 1)
    )
  )
  
  # Get colors from custom theme if provided
  bar_colors <- if (!is.null(custom_theme)) {
    c(custom_theme$colors$primary, custom_theme$colors$secondary)
  } else {
    c(theme_config$colors$primary, theme_config$colors$secondary)
  }
  
  # Create bar chart
  plot_ly(
    data = plot_df,
    x = ~Response,
    y = ~Count,
    type = "bar",
    text = ~paste0(Response, ": ", Count, " (", Percentage, "%)"),
    hoverinfo = "text",
    marker = list(
      color = bar_colors
    )
  ) %>%
    apply_plotly_theme(
      title = title,
      xlab = "Respuesta",
      ylab = "Frecuencia",
      custom_theme = custom_theme
    )
}

create_binary_pie <- function(data, title = "Distribución de Respuestas", custom_theme = NULL) {
  # Calculate proportions
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
           layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get correct labels using the helper function
  labels <- get_binary_labels(data)
  true_label <- labels$true_label
  false_label <- labels$false_label
  
  # Count actual TRUE/FALSE values
  true_count <- sum(data$binary_value, na.rm = TRUE)
  false_count <- sum(!data$binary_value, na.rm = TRUE)
  
  values <- c(true_count, false_count)
  
  labels <- c(true_label, false_label)
  
  percentages <- c(
    round(100 * true_count / (true_count + false_count), 1),
    round(100 * false_count / (true_count + false_count), 1)
  )
  
  # Get colors from custom theme if provided
  pie_colors <- if (!is.null(custom_theme)) {
    c(custom_theme$colors$primary, custom_theme$colors$secondary)
  } else {
    c(theme_config$colors$primary, theme_config$colors$secondary)
  }
  
  # Create pie chart
  plot_ly(
    labels = ~labels,
    values = ~values,
    type = "pie",
    textinfo = "label+percent",
    insidetextorientation = "radial",
    marker = list(
      colors = pie_colors,
      line = list(color = '#FFFFFF', width = 1)
    ),
    hoverinfo = "text",
    text = ~paste0(labels, ": ", values, " respuestas (", percentages, "%)")
  ) %>%
    layout(
      title = list(
        text = title,
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
}

# Updated district map visualization
create_binary_district_map <- function(data, geo_data, highlight_extremes = TRUE, focus_on_true = TRUE, custom_theme = NULL) {
  # Check if we have data
  if (is.null(data) || nrow(data) == 0 || is.null(geo_data)) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get correct labels using the helper function
  labels <- get_binary_labels(data)
  true_label <- labels$true_label
  false_label <- labels$false_label
  
  # Get district colors from custom theme if provided
  district_colors <- if (!is.null(custom_theme)) {
    custom_theme$palettes$district
  } else {
    theme_config$palettes$district
  }
  
  # Calculate percentages by district - we'll focus on TRUE values by default
  district_stats <- data %>%
    group_by(district) %>%
    summarise(
      total_responses = n(),
      true_count = sum(binary_value, na.rm = TRUE),
      false_count = sum(!binary_value, na.rm = TRUE),
      true_percent = round(100 * mean(binary_value, na.rm = TRUE), 1),
      false_percent = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 1),
      .groups = 'drop'
    )
  
  # Choose which percentage to display based on focus parameter
  if (!focus_on_true) {
    district_stats$selected_percent <- district_stats$false_percent
    district_stats$selected_count <- district_stats$false_count
    display_label <- false_label
  } else {
    district_stats$selected_percent <- district_stats$true_percent
    district_stats$selected_count <- district_stats$true_count
    display_label <- true_label
  }
  
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
  
  # Create map
  map <- leaflet(geo_data) %>%
    addTiles() %>% 
    addPolygons(
      fillOpacity = 0.7,
      weight = 1,
      color = ~{
        # Safer color mapping
        district_idx <- match(No_Distrit, as.numeric(district_stats$district))
        if(!is.na(district_idx) && district_idx <= length(district_colors)) {
          district_colors[district_idx]
        } else {
          "#CCCCCC"  # Default gray
        }
      },
      dashArray = "3",
      highlight = highlightOptions(
        weight = 2,
        color = "#666666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "Distrito: %s<br>%s: %s%%<br>Respuestas: %d/%d",
        district_stats$district[match(No_Distrit, district_stats$district)],
        display_label,
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
      '<div style="background-color: #333333; color: white; padding: 5px; border-radius: 3px;"><strong>%s general: %s%%</strong></div>',
      display_label, overall_percent
    ),
    position = "topright"
  )
  
  return(map)
}

create_binary_district_bars <- function(data, orientation = "v", custom_theme = NULL) {
  # Check if we have data
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get correct labels using the helper function
  labels <- get_binary_labels(data)
  true_label <- labels$true_label
  
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
  
  # Get primary color from theme
  bar_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$primary
  } else {
    theme_config$colors$primary
  }
  
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
      marker = list(color = bar_color)
    ) %>%
      apply_plotly_theme(
        title = paste0("Porcentaje de ", true_label, " por Distrito"),
        xlab = paste0("% de ", true_label),
        ylab = "Distrito",
        custom_theme = custom_theme
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
      marker = list(color = bar_color)
    ) %>%
      apply_plotly_theme(
        title = paste0("Porcentaje de ", true_label, " por Distrito"),
        xlab = "Distrito",
        ylab = paste0("% de ", true_label),
        custom_theme = custom_theme
      ) %>%
      layout(
        xaxis = list(tickangle = 45)
      )
  }
}

create_binary_gender_dumbbell <- function(data, custom_theme = NULL) {
  # Check if we have data
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get correct labels using the helper function
  labels <- get_binary_labels(data)
  true_label <- labels$true_label
  
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
  
  # Get gender colors from custom theme if provided
  gender_colors <- if (!is.null(custom_theme)) {
    custom_theme$palettes$gender
  } else {
    get_color_palette("gender")
  }
  
  # Get neutral color from theme
  neutral_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$neutral
  } else {
    theme_config$colors$neutral
  }
  
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
      marker = list(color = gender_colors[2])
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
      marker = list(color = gender_colors[1])
    )
  
  # Add connecting lines
  for(i in 1:nrow(gender_stats_wide)) {
    p <- add_segments(p,
      x = gender_stats_wide$percentage_true_Hombre[i],
      xend = gender_stats_wide$percentage_true_Mujer[i],
      y = gender_stats_wide$district[i],
      yend = gender_stats_wide$district[i],
      line = list(color = neutral_color),
      showlegend = FALSE
    )
  }
  
  p %>% 
    apply_plotly_theme(
      title = paste0("Comparación por Género: % de ", true_label, " por Distrito"),
      xlab = paste0("% de ", true_label),
      ylab = "Distrito",
      custom_theme = custom_theme
    ) %>%
    layout(showlegend = TRUE)
}

create_binary_demographics_bars <- function(data, group_var = "gender", custom_theme = NULL) {
  # Check if we have data
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Get correct labels using the helper function
  labels <- get_binary_labels(data)
  true_label <- labels$true_label
  false_label <- labels$false_label
  
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
  
  # Get colors from custom theme if provided
  demo_colors <- if (group_var == "gender") {
    if (!is.null(custom_theme)) {
      custom_theme$palettes$gender
    } else {
      get_color_palette("gender")
    }
  } else if (group_var == "age_group") {
    if (!is.null(custom_theme)) {
      custom_theme$palettes$age_group
    } else {
      get_color_palette("age_group")
    }
  } else {
    if (!is.null(custom_theme)) {
      custom_theme$colors$primary
    } else {
      theme_config$colors$primary
    }
  }
  
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
    marker = list(color = demo_colors)
  ) %>%
    apply_plotly_theme(
      title = paste("Distribución por", ifelse(group_var == "gender", "Género", "Grupo de Edad")),
      xlab = ifelse(group_var == "gender", "Género", "Grupo de Edad"),
      ylab = paste0("Porcentaje de ", true_label),
      custom_theme = custom_theme
    )
}

create_multiple_binary_comparison <- function(data_list, comparison_type = "bars", top_n = 5, custom_theme = NULL) {
  # Check if we have data
  if (length(data_list) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay preguntas seleccionadas para comparar"))
  }
  
  # Prepare comparison data
  comparison_data <- data.frame(
    QuestionID = character(),
    QuestionLabel = character(),
    TrueLabel = character(),
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
    
    # Get the appropriate label for TRUE values
    labels <- get_binary_labels(data)
    true_label <- labels$true_label
    
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
        TrueLabel = true_label,
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
  
  # Get color palette from custom theme if provided
  color_ramp <- if (!is.null(custom_theme)) {
    colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))
  } else {
    colorRampPalette(c(theme_config$colors$primary, theme_config$colors$highlight))
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
        TrueLabel, ": ", CountTrue, " (", round(PercentageTrue, 1), "%)<br>",
        "Total: ", CountTotal
      ),
      hoverinfo = "text",
      marker = list(
        color = color_ramp(nrow(comparison_data))
      )
    ) %>%
      apply_plotly_theme(
        title = "Comparación de Preguntas Binarias",
        xlab = "Porcentaje de Respuestas Positivas",
        ylab = "",
        custom_theme = custom_theme
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
        comparison_data$TrueLabel, ": ", comparison_data$CountTrue, " (", round(comparison_data$PercentageTrue, 1), "%)<br>",
        "Total: ", comparison_data$CountTotal
      ),
      marker = list(
        colors = color_ramp(nrow(comparison_data))
      )
    ) %>%
      layout(
        title = list(
          text = "Distribución de Respuestas Positivas por Pregunta",
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
  } else if (comparison_type == "bubbles") {
    # Apply theme-based colorscale
    colorscale <- if (!is.null(custom_theme) && !is.null(custom_theme$palettes$sequential)) {
      custom_theme$palettes$sequential
    } else {
      "Blues"
    }
    
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
        colorscale = colorscale,
        colorbar = list(title = list(text = "% Respuestas Positivas"))
      ),
      text = ~paste0(
        QuestionLabel, "<br>",
        TrueLabel, ": ", CountTrue, " (", round(PercentageTrue, 1), "%)<br>",
        "Total: ", CountTotal
      ),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Comparación de Preguntas Binarias",
        xlab = "",
        ylab = "Porcentaje de Respuestas Positivas",
        custom_theme = custom_theme
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
        TrueLabel, ": ", CountTrue, " (", round(PercentageTrue, 1), "%)<br>",
        "Total: ", CountTotal
      ),
      hoverinfo = "text",
      marker = list(
        color = color_ramp(nrow(comparison_data))
      )
    ) %>%
      apply_plotly_theme(
        title = "Comparación de Preguntas Binarias",
        xlab = "Porcentaje de Respuestas Positivas",
        ylab = "",
        custom_theme = custom_theme
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
          
          # Add district map options
          conditionalPanel(
            condition = sprintf("input['%s'] == 'district_map'", ns("plot_type")),
            radioButtons(
              ns("map_focus"),
              "Valor a destacar:",
              choices = c(
                "Sí/Verdadero/Seleccionado" = "true",
                "No/Falso/No Seleccionado" = "false"
              ),
              selected = "true"
            ),
            checkboxInput(
              ns("highlight_extremes"),
              "Resaltar valores extremos",
              value = TRUE
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
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'summary'", ns("plot_type")),
            checkboxInput(
              ns("treat_na_as_negative"),
              "Tratar valores vacíos como 'No'",
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

# Server Definition for the binary module
binaryServer <- function(id, data, metadata, selected_question, geo_data, all_binary_questions = NULL,current_theme = NULL) {
  moduleServer(id, function(input, output, session) {
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
        
        # Pass the treat_na_as_negative parameter from UI
        prep_data <- prepare_binary_data(
          data(), 
          selected_question(), 
          metadata(), 
          treat_na_as_negative = input$treat_na_as_negative
        )
        
        # No need to set the attribute here anymore as it's now set within the function
        
        return(prep_data)
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
    observe({
      # Update the checkbox default value based on the question
      question_key <- selected_question()
      default_value <- TRUE  # Default to treating NA as negative
      
      # Check if there's a survey-specific config
      if (!is.null(survey_config) && 
          !is.null(survey_config$binary_response_config)) {
        
        if (question_key %in% names(survey_config$binary_response_config$question_exceptions)) {
          default_value <- survey_config$binary_response_config$question_exceptions[[question_key]]
        } else {
          default_value <- survey_config$binary_response_config$treat_na_as_negative_by_default
        }
      }
      
      updateCheckboxInput(session, "treat_na_as_negative", value = default_value)
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
        "district_map" = leafletOutput(session$ns("district_map"), height = "600px"),
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
        total_responses <- attr(data, "total_responses")
        missing_count <- attr(data, "missing_count")
        valid_responses <- nrow(data)
        
        # Calculate binary proportions
        true_count <- sum(data$binary_value, na.rm = TRUE)
        false_count <- sum(!data$binary_value, na.rm = TRUE)
        true_percent <- round(100 * true_count / (true_count + false_count), 2)
        false_percent <- round(100 * false_count / (true_count + false_count), 2)
        
        # Get the appropriate labels using the helper function
        labels <- get_binary_labels(data)
        true_label <- labels$true_label
        false_label <- labels$false_label
        
        cat("Estadísticas para Datos Binarios:\n")
        cat("\nDistribución de Respuestas:\n")
        cat("Total de respuestas:", total_responses, "\n")
        cat("Respuestas válidas:", valid_responses, "\n")
        cat("Datos faltantes:", missing_count,
            sprintf("(%.1f%%)", 100 * missing_count/total_responses), "\n")
        
        cat("\nDistribución de valores:\n")
        cat(true_label, ":", true_count, sprintf("(%.2f%%)", true_percent), "\n")
        cat(false_label, ":", false_count, sprintf("(%.2f%%)", false_percent), "\n")
        
        # District breakdown
        cat("\nDistribución por Distrito:\n")
        district_breakdown <- data %>%
          group_by(district) %>%
          summarise(
            Total = n(),
            Positivo = sum(binary_value, na.rm = TRUE),
            `%Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            Negativo = sum(!binary_value, na.rm = TRUE),
            `%Negativo` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
            .groups = 'drop'
          )
        
        print(district_breakdown)
        
        # Gender breakdown
        cat("\nDistribución por Género:\n")
        gender_breakdown <- data %>%
          group_by(gender) %>%
          summarise(
            Total = n(),
            Positivo = sum(binary_value, na.rm = TRUE),
            `%Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            Negativo = sum(!binary_value, na.rm = TRUE),
            `%Negativo` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
            .groups = 'drop'
          )
        
        print(gender_breakdown)
        
        # Age breakdown
        cat("\nDistribución por Grupo de Edad:\n")
        age_breakdown <- data %>%
          group_by(age_group) %>%
          summarise(
            Total = n(),
            Positivo = sum(binary_value, na.rm = TRUE),
            `%Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            Negativo = sum(!binary_value, na.rm = TRUE),
            `%Negativo` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
            .groups = 'drop'
          )
        
        print(age_breakdown)
        
        cat("\nNotas sobre el procesamiento de datos:\n")
        if (attr(data, "treat_na_as_negative")) {
          cat("- Los valores vacíos (NA) se están tratando como respuestas 'No'.\n")
        } else {
          cat("- Los valores vacíos (NA) se están tratando como datos faltantes.\n")
        }
        
        # Add info about special value mappings if present
        value_labels <- attr(data, "value_labels")
        if (!is.null(value_labels)) {
          cat("- Mapeo de valores: ")
          for (i in seq_along(value_labels)) {
            cat(names(value_labels)[i], "=", value_labels[i])
            if (i < length(value_labels)) cat(", ")
          }
          cat("\n")
        }
        
        cat("\nDetalles técnicos para diagnóstico:\n")
        cat("- ID de pregunta:", attr(data, "question_id"), "\n")
        cat("- Tipo de pregunta:", ifelse(attr(data, "is_checkbox_question"), "Casilla de verificación", "Binaria estándar"), "\n")
        cat("- Lógica invertida:", ifelse(attr(data, "invert_selected_logic"), "Sí", "No"), "\n")
        cat("- Valores positivos:", paste(attr(data, "positive_values"), collapse=", "), "\n")
        cat("- Valores negativos:", paste(attr(data, "negative_values"), collapse=", "), "\n")
        
        if (!is.null(attr(data, "ones_count"))) {
          cat("- Valores '1':", attr(data, "ones_count"), "\n")
          cat("- Valores '0':", attr(data, "zeros_count"), "\n")
          cat("- Valores NA:", attr(data, "na_count"), "\n")
        }
      }, error = function(e) {
        cat("Error al generar estadísticas:", e$message)
      })
    })
    
    # Add this function to generate summary tables for download
    summary_tables <- reactive({
      data <- filtered_data()
      
      # Get the appropriate labels
      labels <- get_binary_labels(data)
      true_label <- labels$true_label
      false_label <- labels$false_label
      
      # Create overall statistics table
      true_count <- sum(data$binary_value, na.rm = TRUE)
      false_count <- sum(!data$binary_value, na.rm = TRUE)
      true_percent <- round(100 * true_count / (true_count + false_count), 2)
      false_percent <- round(100 * false_count / (true_count + false_count), 2)
      
      overall_stats <- data.frame(
        Categoria = c(true_label, false_label, "Total", "Datos Faltantes"),
        Conteo = c(true_count, false_count, attr(data, "total_responses"), attr(data, "missing_count")),
        Porcentaje = c(true_percent, false_percent, 100, NA)
      )
      
      # District breakdown
      district_breakdown <- data %>%
        group_by(district) %>%
        summarise(
          Total = n(),
          Positivo = sum(binary_value, na.rm = TRUE),
          `Porcentaje_Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2),
          Negativo = sum(!binary_value, na.rm = TRUE),
          `Porcentaje_Negativo` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
          .groups = 'drop'
        )
      
      # Gender breakdown
      gender_breakdown <- data %>%
        group_by(gender) %>%
        summarise(
          Total = n(),
          Positivo = sum(binary_value, na.rm = TRUE),
          `Porcentaje_Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2),
          Negativo = sum(!binary_value, na.rm = TRUE),
          `Porcentaje_Negativo` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
          .groups = 'drop'
        )
      
      # Age breakdown
      age_breakdown <- data %>%
        group_by(age_group) %>%
        summarise(
          Total = n(),
          Positivo = sum(binary_value, na.rm = TRUE),
          `Porcentaje_Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2),
          Negativo = sum(!binary_value, na.rm = TRUE),
          `Porcentaje_Negativo` = round(100 * (1 - mean(binary_value, na.rm = TRUE)), 2),
          .groups = 'drop'
        )
      
      # Technical details
      technical_info <- data.frame(
        Parametro = c(
          "ID de pregunta",
          "Tipo de pregunta",
          "Logica invertida",
          "Tratar NA como negativo"
        ),
        Valor = c(
          attr(data, "question_id"),
          ifelse(attr(data, "is_checkbox_question"), "Casilla de verificación", "Binaria estándar"),
          ifelse(attr(data, "invert_selected_logic"), "Sí", "No"),
          ifelse(attr(data, "treat_na_as_negative"), "Sí", "No")
        )
      )
      
      # Create a list of all tables
      return(list(
        overall = overall_stats, 
        district = district_breakdown,
        gender = gender_breakdown,
        age = age_breakdown,
        technical = technical_info
      ))
    })
    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("resumen_binario_", selected_question(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        summaries <- summary_tables()
        
        # Create temporary directory for files
        temp_dir <- tempdir()
        
        # Write each summary to a CSV file
        write.csv(summaries$overall, file.path(temp_dir, "resumen_general.csv"), row.names = FALSE)
        write.csv(summaries$district, file.path(temp_dir, "por_distrito.csv"), row.names = FALSE)
        write.csv(summaries$gender, file.path(temp_dir, "por_genero.csv"), row.names = FALSE)
        write.csv(summaries$age, file.path(temp_dir, "por_edad.csv"), row.names = FALSE)
        write.csv(summaries$technical, file.path(temp_dir, "detalles_tecnicos.csv"), row.names = FALSE)
        
        # Create zip file with all CSVs - using just filenames not full paths
        # Save current working directory
        oldwd <- getwd()
        
        # Change to temp directory before zipping
        setwd(temp_dir)
        
        # Create zip file with just filenames (not full paths)
        files_to_zip <- c(
          "resumen_general.csv",
          "por_distrito.csv",
          "por_genero.csv",
          "por_edad.csv",
          "detalles_tecnicos.csv"
        )
        
        zip(file, files_to_zip)
        
        # Change back to original working directory
        setwd(oldwd)
      }
    )
    
    # Add Excel download handler
    output$download_summary_excel <- downloadHandler(
      filename = function() {
        paste0("resumen_binario_", selected_question(), "_", Sys.Date(), ".xlsx")
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
        
        openxlsx::addWorksheet(wb, "Resumen General")
        openxlsx::writeData(wb, "Resumen General", summaries$overall)
        
        openxlsx::addWorksheet(wb, "Por Distrito")
        openxlsx::writeData(wb, "Por Distrito", summaries$district)
        
        openxlsx::addWorksheet(wb, "Por Género")
        openxlsx::writeData(wb, "Por Género", summaries$gender)
        
        openxlsx::addWorksheet(wb, "Por Grupo de Edad")
        openxlsx::writeData(wb, "Por Grupo de Edad", summaries$age)
        
        openxlsx::addWorksheet(wb, "Detalles Técnicos")
        openxlsx::writeData(wb, "Detalles Técnicos", summaries$technical)
        
        # Save workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    # Bar plot
    output$bars_plot <- renderPlotly({
      req(filtered_data())
      question_label <- attr(filtered_data(), "question_label")
      if (is.null(question_label)) question_label <- selected_question()
      
      create_binary_bar(
        filtered_data(), 
        title = paste("Distribución de", question_label),
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$pie_plot <- renderPlotly({
      req(filtered_data())
      question_label <- attr(filtered_data(), "question_label")
      if (is.null(question_label)) question_label <- selected_question()
      
      create_binary_pie(
        filtered_data(),
        title = paste("Distribución de", question_label),
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$district_map <- renderLeaflet({
      req(filtered_data(), geo_data())
      focus_on_true <- input$map_focus == "true"
      
      create_binary_district_map(
        filtered_data(), 
        geo_data(),
        highlight_extremes = input$highlight_extremes,
        focus_on_true = focus_on_true,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$district_bars_plot <- renderPlotly({
      req(filtered_data())
      create_binary_district_bars(
        filtered_data(),
        orientation = input$bar_orientation,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$gender_dumbbell_plot <- renderPlotly({
      req(filtered_data())
      create_binary_gender_dumbbell(
        filtered_data(),
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$gender_bars_plot <- renderPlotly({
      req(filtered_data())
      create_binary_demographics_bars(
        filtered_data(), 
        "gender",
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$age_bars_plot <- renderPlotly({
      req(filtered_data())
      create_binary_demographics_bars(
        filtered_data(), 
        "age_group",
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    output$multiple_comparison_plot <- renderPlotly({
      req(multiple_questions_data())
      create_multiple_binary_comparison(
        multiple_questions_data(),
        comparison_type = input$comparison_type,
        top_n = input$top_n,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
  })

}