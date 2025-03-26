library(colourpicker)
library(RColorBrewer)
library(viridisLite)
library(plotly)

server <- function(input, output, session) {
  data <- reactive({
    req(input$survey_selector)
    survey_data <- load_survey_data(input$survey_selector)
    
    # Add logging to help with debugging
    message(paste("Loaded survey:", input$survey_selector))
    message(paste("Number of rows:", nrow(survey_data$responses)))
    message(paste("Number of columns:", ncol(survey_data$responses)))
    message(paste("Column names sample:", paste(head(names(survey_data$responses), 5), collapse=", ")))
    
    return(survey_data)
  })
  # Load theme metadata
all_themes <- reactive({
  theme_metadata$load_thematic_classifications()
})

# Populate theme selectors in UI
observe({
  themes_list <- theme_metadata$get_all_themes()
  
  # Update all theme selectors with "all" option
  updateSelectInput(session, "theme_selector", 
                   choices = c("Todos los temas" = "all", themes_list),
                   selected = "all")
  
  updateSelectInput(session, "filter_by_theme", 
                   choices = c("Todos" = "all", themes_list),
                   selected = "all")
  
  updateSelectInput(session, "test_theme_filter", 
                   choices = c("Todos" = "all", themes_list),
                   selected = "all")
  
  updateSelectInput(session, "search_theme_filter", 
                   choices = c("Todos" = "all", themes_list),
                   selected = "all")
})

# Update subtheme selector based on selected theme
observe({
  req(input$theme_selector)
  
  if (input$theme_selector != "all") {
    subthemes <- theme_metadata$get_subthemes_by_theme(input$theme_selector)
    updateSelectInput(session, "subtheme_selector", 
                     choices = c("Todos los subtemas" = "all", subthemes),
                     selected = "all")
  } else {
    # If "all themes" is selected, disable subtheme selection
    updateSelectInput(session, "subtheme_selector", 
                     choices = c("Todos los subtemas" = "all"),
                     selected = "all")
  }
})

# Update filter_by_subtheme based on selected theme
observe({
  req(input$filter_by_theme)
  if (input$filter_by_theme != "all") {
    subthemes <- theme_metadata$get_subthemes_by_theme(input$filter_by_theme)
    updateSelectInput(session, "filter_by_subtheme", 
                     choices = c("Todos" = "all", subthemes))
  } else {
    updateSelectInput(session, "filter_by_subtheme", 
                     choices = c("Todos" = "all"))
  }
})

# Update test_subtheme_filter based on selected theme
observe({
  req(input$test_theme_filter)
  if (input$test_theme_filter != "all") {
    subthemes <- theme_metadata$get_subthemes_by_theme(input$test_theme_filter)
    updateSelectInput(session, "test_subtheme_filter", 
                     choices = c("Todos" = "all", subthemes))
  } else {
    updateSelectInput(session, "test_subtheme_filter", 
                     choices = c("Todos" = "all"))
  }
})

# Display theme info
output$theme_info_panel <- renderUI({
  if (is.null(input$theme_selector) || input$theme_selector == "all") {
    return(div(
      class = "alert alert-info",
      "Mostrando información para todos los temas. Seleccione un tema específico para ver más detalles."
    ))
  }
  
  theme_property <- theme_metadata$get_theme_property(input$theme_selector)
  
  div(
    h4(input$theme_selector),
    p(theme_property$description),
    tags$span(
      class = "badge",
      style = paste0("background-color: ", theme_property$color, "; color: white;"),
      icon(theme_property$icon), " ", input$theme_selector
    ),
    hr(),
    if (!is.null(input$subtheme_selector) && input$subtheme_selector != "all") {
      subtheme_property <- theme_metadata$get_subtheme_property(
        input$theme_selector, 
        input$subtheme_selector
      )
      div(
        h5(input$subtheme_selector),
        p(subtheme_property$description)
      )
    }
  )
})
  current_theme <- reactiveVal(theme_config)
  palette_options <- list(
    Default = theme_config$palettes$district,
    Viridis = viridisLite::viridis(9),
    Plasma = viridisLite::plasma(9),
    Inferno = viridisLite::inferno(9),
    Magma = viridisLite::magma(9),
    Blues = colorRampPalette(c("#deebf7", "#08519c"))(9),
    Greens = colorRampPalette(c("#e5f5e0", "#31a354"))(9)
  )
  
  gender_palette_options <- list(
    Default = theme_config$palettes$gender,
    Pastel = c("#FFB6C1", "#ADD8E6"),
    Dark = c("#8B0046", "#00008B"),
    Set1 = RColorBrewer::brewer.pal(3, "Set1")[1:2],
    Set2 = RColorBrewer::brewer.pal(3, "Set2")[1:2]
  )
  
  age_palette_options <- list(
    Default = theme_config$palettes$age_group,
    Pastel = RColorBrewer::brewer.pal(5, "Pastel1"),
    Dark = RColorBrewer::brewer.pal(5, "Dark2"),
    Set1 = RColorBrewer::brewer.pal(5, "Set1"),
    Set2 = RColorBrewer::brewer.pal(5, "Set2")
  )

  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error loading geo data:", e$message), type = "error")
      NULL
    })
  })

  output$survey_name <- renderText({
    if (input$survey_selector == "PAR_2023") {
      "2023 Participación Ciudadana (PAR 2023)"
    } else if (input$survey_selector == "PER_2023"){
      "2023 Percepción Ciudadana (PER 2023)"
    }else if (input$survey_selector == "PER_2024"){
      "ACTUAL 2024 Percepción Ciudadana (PER 2024)"
    } else if (input$survey_selector == "PAR_2024"){
      "ACTUAL 2024 Participación Ciudadana (PAR 2024)"
    }
  })
  observeEvent(input$survey_selector, {
    # Reset the test_question when survey changes
    updateSelectInput(session, "test_question", choices = NULL, selected = NULL)
    
    # Also reset the question_type in the Classification panel if needed
    updateSelectInput(session, "question_type", selected = "razon")
  })

  
  observe({
    # Skip if no input values yet
    req(input$primary_color, input$font_family)
    
    # Colors
    new_colors <- list(
      primary = input$primary_color,
      secondary = input$secondary_color,
      highlight = input$highlight_color,
      neutral = theme_config$colors$neutral,
      background = theme_config$colors$background,
      text = theme_config$colors$text
    )
    
    # Typography
    new_typography <- list(
      font_family = input$font_family,
      sizes = list(
        title = input$title_size,
        subtitle = theme_config$typography$sizes$subtitle,
        axis = input$axis_size,
        text = theme_config$typography$sizes$text
      )
    )
    
    # Update palettes
    new_palettes <- theme_config$palettes
    
    # Update district palette
    if (input$district_palette != "Default" && !is.null(palette_options[[input$district_palette]])) {
      new_palettes$district <- palette_options[[input$district_palette]]
    }
    
    # Update gender palette
    if (input$gender_palette != "Default" && !is.null(gender_palette_options[[input$gender_palette]])) {
      new_palettes$gender <- gender_palette_options[[input$gender_palette]]
    }
    
    # Update age group palette
    if (input$age_palette != "Default" && !is.null(age_palette_options[[input$age_palette]])) {
      new_palettes$age_group <- age_palette_options[[input$age_palette]]
    }
    
    # Update derived palettes
    new_palettes$sequential <- colorRampPalette(c("#FFFFFF", new_colors$text))(9)
    new_palettes$diverging <- colorRampPalette(c(new_colors$secondary, "#FFFFFF", new_colors$primary))(11)
    
    # Update current theme
    theme_update <- theme_config
    theme_update$colors <- new_colors
    theme_update$typography <- new_typography
    theme_update$palettes <- new_palettes
    
    current_theme(theme_update)
  })
  
  # Reset theme to defaults
  observeEvent(input$reset_theme, {
    # Reset all inputs to defaults
    colourpicker::updateColourInput(session, "primary_color", value = theme_config$colors$primary)
    colourpicker::updateColourInput(session, "secondary_color", value = theme_config$colors$secondary)
    colourpicker::updateColourInput(session, "highlight_color", value = theme_config$colors$highlight)
    
    updateSelectInput(session, "district_palette", selected = "Default")
    updateSelectInput(session, "gender_palette", selected = "Default")
    updateSelectInput(session, "age_palette", selected = "Default")
    
    updateSelectInput(session, "font_family", selected = theme_config$typography$font_family)
    updateNumericInput(session, "title_size", value = theme_config$typography$sizes$title)
    updateNumericInput(session, "axis_size", value = theme_config$typography$sizes$axis)
    
    # Reset theme to original
    current_theme(theme_config)
    
    showNotification("Tema restablecido a valores predeterminados", type = "message")
  })
  
  # Save theme
  observeEvent(input$save_theme, {
    # Generate JSON string of current theme
    theme_json <- toJSON(current_theme(), pretty = TRUE)
    
    # Save to a file in a config directory
    dir.create("config", showWarnings = FALSE)
    write(theme_json, file = "config/custom_theme.json")
    
    showNotification("Tema guardado correctamente", type = "message")
  })
  
  # Download theme
  output$download_theme <- downloadHandler(
    filename = function() {
      paste("custom_theme_", format(Sys.time(), "%Y%m%d_%H%M"), ".json", sep = "")
    },
    content = function(file) {
      theme_json <- toJSON(current_theme(), pretty = TRUE)
      write(theme_json, file)
    }
  )
  
  # Upload theme
  observeEvent(input$upload_theme, {
    req(input$upload_theme)
    
    # Read the uploaded JSON file
    tryCatch({
      uploaded_theme <- fromJSON(input$upload_theme$datapath)
      
      # Validate the uploaded theme - simple check for required elements
      if (!all(c("colors", "typography", "palettes") %in% names(uploaded_theme))) {
        showNotification("Archivo de tema inválido", type = "error")
        return()
      }
      
      # Update the current theme
      current_theme(uploaded_theme)
      
      # Update UI controls to match the uploaded theme
      colourpicker::updateColourInput(session, "primary_color", value = uploaded_theme$colors$primary)
      colourpicker::updateColourInput(session, "secondary_color", value = uploaded_theme$colors$secondary)
      colourpicker::updateColourInput(session, "highlight_color", value = uploaded_theme$colors$highlight)
      
      updateSelectInput(session, "font_family", selected = uploaded_theme$typography$font_family)
      updateNumericInput(session, "title_size", value = uploaded_theme$typography$sizes$title)
      updateNumericInput(session, "axis_size", value = uploaded_theme$typography$sizes$axis)
      
      showNotification("Tema importado correctamente", type = "message")
    }, error = function(e) {
      showNotification(paste("Error al importar tema:", e$message), type = "error")
    })
  })
  output$theme_preview_gender <- renderPlotly({
  # Create a sample dataset for gender comparison
  sample_gender <- data.frame(
    gender = c("Hombre", "Mujer"),
    value = c(65, 75)
  )
  
  # Create gender preview
  plot_ly(
    data = sample_gender,
    x = ~gender,
    y = ~value,
    type = "bar",
    marker = list(
      color = current_theme()$palettes$gender
    )
  ) %>%
    layout(
      title = list(
        text = "Vista Previa: Paleta de Género",
        font = list(
          family = current_theme()$typography$font_family,
          size = current_theme()$typography$sizes$title,
          color = current_theme()$colors$text
        )
      ),
      xaxis = list(
        title = "Género",
        titlefont = list(
          family = current_theme()$typography$font_family,
          size = current_theme()$typography$sizes$axis,
          color = current_theme()$colors$text
        )
      ),
      yaxis = list(
        title = "Valor",
        titlefont = list(
          family = current_theme()$typography$font_family,
          size = current_theme()$typography$sizes$axis,
          color = current_theme()$colors$text
        )
      ),
      paper_bgcolor = current_theme()$colors$background,
      plot_bgcolor = current_theme()$colors$background
    )
})

# Preview plot for age group palette
output$theme_preview_age <- renderPlotly({
  # Create a sample dataset for age groups
  sample_age <- data.frame(
    age_group = c("18-24", "25-34", "35-44", "45-64", "65+"),
    value = c(45, 60, 75, 65, 55)
  )
  
  # Create age group preview
  plot_ly(
    data = sample_age,
    x = ~age_group,
    y = ~value,
    type = "bar",
    marker = list(
      color = current_theme()$palettes$age_group
    )
  ) %>%
    layout(
      title = list(
        text = "Vista Previa: Paleta de Grupos de Edad",
        font = list(
          family = current_theme()$typography$font_family,
          size = current_theme()$typography$sizes$title,
          color = current_theme()$colors$text
        )
      ),
      xaxis = list(
        title = "Grupo de Edad",
        titlefont = list(
          family = current_theme()$typography$font_family,
          size = current_theme()$typography$sizes$axis,
          color = current_theme()$colors$text
        )
      ),
      yaxis = list(
        title = "Valor",
        titlefont = list(
          family = current_theme()$typography$font_family,
          size = current_theme()$typography$sizes$axis,
          color = current_theme()$colors$text
        )
      ),
      paper_bgcolor = current_theme()$colors$background,
      plot_bgcolor = current_theme()$colors$background
    )
})
  # Sample previews with the current theme
  output$theme_preview_plot <- renderPlotly({
    # Create a sample dataset
    set.seed(123)
    sample_data <- data.frame(
      category = LETTERS[1:6],
      value = sample(10:50, 6),
      group = rep(c("Grupo A", "Grupo B"), each = 3)
    )
    
    # Use the current theme to create a plot
    plot_ly(
      data = sample_data,
      x = ~category,
      y = ~value,
      type = "bar",
      color = ~group,
      colors = c(current_theme()$colors$primary, current_theme()$colors$secondary)
    ) %>%
      layout(
        title = list(
          text = "Vista Previa: Gráfico de Barras",
          font = list(
            family = current_theme()$typography$font_family,
            size = current_theme()$typography$sizes$title,
            color = current_theme()$colors$text
          )
        ),
        xaxis = list(
          title = "Categorías",
          titlefont = list(
            family = current_theme()$typography$font_family,
            size = current_theme()$typography$sizes$axis,
            color = current_theme()$colors$text
          )
        ),
        yaxis = list(
          title = "Valores",
          titlefont = list(
            family = current_theme()$typography$font_family,
            size = current_theme()$typography$sizes$axis,
            color = current_theme()$colors$text
          )
        ),
        paper_bgcolor = current_theme()$colors$background,
        plot_bgcolor = current_theme()$colors$background
      )
  })
  
  output$theme_preview_district <- renderPlotly({
    # Create a sample dataset for districts
    sample_district <- data.frame(
      district = factor(1:9),
      value = sample(30:70, 9)
    )
    
    # Create district preview
    plot_ly(
      data = sample_district,
      x = ~district,
      y = ~value,
      type = "bar",
      marker = list(
        color = current_theme()$palettes$district
      )
    ) %>%
      layout(
        title = list(
          text = "Vista Previa: Paleta de Distritos",
          font = list(
            family = current_theme()$typography$font_family,
            size = current_theme()$typography$sizes$title,
            color = current_theme()$colors$text
          )
        ),
        xaxis = list(
          title = "Distrito",
          titlefont = list(
            family = current_theme()$typography$font_family,
            size = current_theme()$typography$sizes$axis,
            color = current_theme()$colors$text
          )
        ),
        yaxis = list(
          title = "Valor",
          titlefont = list(
            family = current_theme()$typography$font_family,
            size = current_theme()$typography$sizes$axis,
            color = current_theme()$colors$text
          )
        ),
        paper_bgcolor = current_theme()$colors$background,
        plot_bgcolor = current_theme()$colors$background
      )
  })
  output$survey_info <- renderUI({
    req(data())
    
    # Calculate some basic stats
    num_questions <- ncol(data()$responses)
    num_responses <- nrow(data()$responses)
    survey_id <- data()$survey_id
    
    tagList(
      p(paste("ID de Encuesta:", survey_id)),
      p(paste("Número de Preguntas:", num_questions)),
      p(paste("Número de Respuestas:", num_responses)),
      p(paste("Fecha de actualización:", format(Sys.Date(), "%d/%m/%Y")))
    )
  })
  # Classify questions
  question_classification <- reactive({
    classify_questions(data()$metadata)
  })
  
  # Update question choices based on selected module
  observe({
    questions <- question_classification()[[input$test_module]]
    updateSelectInput(
      session,
      "test_question",
      choices = questions
    )
  })
  
  # Display total responses
  output$total_responses <- renderText({
    nrow(data()$responses)
  })
  
  # Display total questions
  output$total_questions <- renderText({
    ncol(data()$responses)
  })
  
  # Display classification summary with themes
output$classification_summary <- renderTable({
  types <- question_classification()
  
  # Get theme counts
  themes_data <- theme_metadata$load_thematic_classifications()
  theme_counts <- themes_data %>%
    group_by(MainTheme) %>%
    summarise(Cantidad = n()) %>%
    filter(!is.na(MainTheme))
  
  # Type counts
  type_counts <- data.frame(
    Tipo = c("Razón", "Intervalo", "Ordinal", "Categórico", "Binaria", "Nominal"),
    Cantidad = sapply(types, length)
  )
  
  # Combine and return both tables
  rbind(
    type_counts,
    data.frame(
      Tipo = paste0("Tema: ", theme_counts$MainTheme),
      Cantidad = theme_counts$Cantidad
    )
  )
})
  output$question_label <- renderText({
    req(input$test_question, data()$metadata)
    get_question_label(input$test_question, data()$metadata)
  })
  # Display questions by type
  output$questions_by_type <- DT::renderDataTable({
    req(input$question_type)
    questions <- question_classification()[[input$question_type]]
    
    # Filter by theme if selected
    filtered_questions <- questions
    
    if (input$filter_by_theme != "all" || input$filter_by_subtheme != "all") {
      themes_data <- theme_metadata$load_thematic_classifications()
      
      if (input$filter_by_theme != "all" && input$filter_by_subtheme == "all") {
        # Filter by theme only
        theme_questions <- themes_data %>%
          filter(MainTheme == input$filter_by_theme) %>%
          pull(variable)
        
        filtered_questions <- questions[questions %in% theme_questions]
      } else if (input$filter_by_theme != "all" && input$filter_by_subtheme != "all") {
        # Filter by theme and subtheme
        theme_questions <- themes_data %>%
          filter(MainTheme == input$filter_by_theme, Subtheme == input$filter_by_subtheme) %>%
          pull(variable)
        
        filtered_questions <- questions[questions %in% theme_questions]
      }
    }
    
    if(input$show_metadata) {
      metadata_subset <- data()$metadata[data()$metadata$variable %in% filtered_questions, ]
      
      # Add theme information
      themes_data <- theme_metadata$load_thematic_classifications()
      themes_data <- themes_data %>%
        select(variable, MainTheme, Subtheme)
      
      metadata_with_themes <- left_join(
        metadata_subset,
        themes_data,
        by = "variable"
      )
      
      DT::datatable(
        metadata_with_themes,
        options = list(pageLength = 10),
        filter = 'top'
      )
    } else {
      DT::datatable(
        data.frame(Variable = filtered_questions),
        options = list(pageLength = 10),
        filter = 'top'
      )
    }
  })
  # Update test question selection based on module, theme, and subtheme
observe({
  # Clear the question selection first
  updateSelectInput(session, "test_question", choices = NULL, selected = NULL)
  
  # Get questions for the selected module
  questions <- question_classification()[[input$test_module]]
  
  # Filter by theme and subtheme if selected
  if (input$test_theme_filter != "all" || input$test_subtheme_filter != "all") {
    themes_data <- theme_metadata$load_thematic_classifications()
    
    if (input$test_theme_filter != "all" && input$test_subtheme_filter == "all") {
      # Filter by theme only
      theme_questions <- themes_data %>%
        filter(MainTheme == input$test_theme_filter) %>%
        pull(variable)
      
      questions <- questions[questions %in% theme_questions]
    } else if (input$test_theme_filter != "all" && input$test_subtheme_filter != "all") {
      # Filter by theme and subtheme
      theme_questions <- themes_data %>%
        filter(MainTheme == input$test_theme_filter, Subtheme == input$test_subtheme_filter) %>%
        pull(variable)
      
      questions <- questions[questions %in% theme_questions]
    }
  }
  
  # Update the question selection with filtered questions
  updateSelectInput(
    session,
    "test_question",
    choices = questions
  )
})

# Display theme information for the selected question
output$question_theme_label <- renderUI({
  req(input$test_question)
  
  themes_data <- theme_metadata$load_thematic_classifications()
  question_theme_info <- themes_data %>%
    filter(variable == input$test_question) %>%
    select(MainTheme, Subtheme) %>%
    first()
  
  if (!is.null(question_theme_info) && !is.na(question_theme_info$MainTheme)) {
    theme_property <- theme_metadata$get_theme_property(question_theme_info$MainTheme)
    
    div(
      tags$span(
        "Tema: ",
        tags$span(
          class = "badge",
          style = paste0("background-color: ", theme_property$color, "; color: white;"),
          icon(theme_property$icon), " ", question_theme_info$MainTheme
        )
      ),
      if (!is.na(question_theme_info$Subtheme)) {
        tags$span(
          " | Subtema: ",
          tags$span(
            class = "badge",
            style = "background-color: #6c757d; color: white;",
            question_theme_info$Subtheme
          )
        )
      }
    )
  } else {
    div(
      tags$span(
        "Tema: ",
        tags$span(
          class = "badge",
          style = "background-color: #6c757d; color: white;",
          "No clasificado"
        )
      )
    )
  }
})
  # Selected question reactive
  selected_question <- reactive({
    input$test_question
  })
  
  # Call appropriate module based on selection
  observeEvent(input$test_module, {
    if(input$test_module == "razon") {
      razonServer(
        "razon_test",
        data = reactive(data()$responses),
        selected_question = selected_question,
        geo_data = geo_data,
        metadata = reactive(data()$metadata),
        current_theme = current_theme  # Pass the current theme reactive
      )
    } else if(input$test_module == "intervalo") {
      intervalServer(
        "interval_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data,
        current_theme = current_theme  # Pass the current theme
      )
    } else if(input$test_module == "ordinal") {
      ordinalServer(
        "ordinal_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data,
        current_theme = current_theme  # Pass the current theme
      )
    } else if(input$test_module == "categorico") {
      categoricoServer(
        "categorico_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data,
        current_theme = current_theme  # Pass the current theme
      )
    } else if(input$test_module == "binaria") {
      # Get all binary questions for the comparison feature
      all_binary_questions <- question_classification()[["binaria"]]
      
      binaryServer(
        "binary_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data,
        all_binary_questions = all_binary_questions,
        current_theme = current_theme  # Pass the current theme
      )
    } else if(input$test_module == "nominal") {
      # Add the nominal module server
      nominalServer(
        "nominal_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data,
        current_theme = current_theme  # Pass the current theme
      )
    }
  })
  # Search functionality
  observeEvent(input$execute_search, {
    req(input$global_search, data())
    search_text <- tolower(input$global_search)
    
    # Get all questions with their labels and INCLUDE CURRENT SURVEY ID
    current_survey_id <- data()$survey_id
    all_questions <- data.frame(
      variable = data()$metadata$variable,
      label = data()$metadata$label,
      scale_type = data()$metadata$scale_type,
      survey_id = current_survey_id,  # Add current survey ID
      stringsAsFactors = FALSE
    )
    
    # Filter questions where label or variable contains the search text
    matching_questions <- all_questions[
      grepl(search_text, tolower(all_questions$label)) | 
      grepl(search_text, tolower(all_questions$variable)),
    ]
    
    # Always get theme information
    themes_data <- theme_metadata$load_thematic_classifications()
    
    # Filter by theme if selected
    if (input$search_theme_filter != "all") {
      theme_questions <- themes_data %>%
        filter(MainTheme == input$search_theme_filter) %>%
        pull(variable)
      
      matching_questions <- matching_questions[matching_questions$variable %in% theme_questions, ]
    }
    
    # Create a nice results table with theme information - KEY CHANGE: JOIN BY BOTH VARIABLE AND SURVEY_ID
    if (nrow(matching_questions) > 0) {
      # Normalize survey IDs before joining to match format in themes_data
      matching_questions$survey_id <- sub("_V2$", "", matching_questions$survey_id)
      
      # Join with theme data using both variable AND survey_id to prevent duplicates
      results_df <- matching_questions %>%
        left_join(
          themes_data %>% select(variable, survey_id, MainTheme, Subtheme),
          by = c("variable", "survey_id")
        ) %>%
        select(
          Variable = variable,
          Pregunta = label,
          Tipo = scale_type,
          Tema = MainTheme,
          Subtema = Subtheme,
          Encuesta = survey_id
        )
      
      # Replace NA values with "No clasificado"
      results_df$Tema[is.na(results_df$Tema)] <- "No clasificado"
      results_df$Subtema[is.na(results_df$Subtema)] <- "No clasificado"
      
      # Add a nicer survey display name
      results_df$Encuesta <- ifelse(
        results_df$Encuesta == "PER_2024",
        "Percepción 2024",
        "Participación 2024"
      )
    } else {
      # Empty dataframe with all required columns
      results_df <- data.frame(
        Variable = character(0),
        Pregunta = character(0),
        Tipo = character(0),
        Tema = character(0),
        Subtema = character(0),
        Encuesta = character(0)
      )
    }
    
    # Store the search results
    output$search_results_table <- DT::renderDataTable({
      if (nrow(results_df) == 0) {
        # Empty table with message
        output$search_info <- renderUI({
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"), 
            "No se encontraron preguntas que coincidan con el texto de búsqueda."
          )
        })
        return(results_df)
      }
      
      # Show success message
      output$search_info <- renderUI({
        div(
          class = "alert alert-info",
          icon("info-circle"), 
          paste0("Se encontraron ", nrow(results_df), " preguntas. Haga clic en cualquier fila para ver la pregunta.")
        )
      })
      
      # Return the datatable with clickable rows
      DT::datatable(
        results_df, 
        selection = 'single',
        options = list(
          pageLength = 15,
          language = list(
            search = "Filtrar:",
            paginate = list(previous = "Anterior", `next` = "Siguiente")
          )
        )
      )
    })
  })

# Enhanced handler for search result selection with robust error handling
observeEvent(input$search_results_table_rows_selected, {
  row_index <- input$search_results_table_rows_selected
  
  # Make sure we have a selection and search results
  if (length(row_index) > 0 && !is.null(input$global_search)) {
    # Get the DT table data directly (safer than reconstructing the search)
    table_data <- isolate({
      dataTableProxy("search_results_table") %>% 
        DT::selectRows(NULL) %>% 
        DT::selectPage(NULL)
      
      results_df <- input$search_results_table_rows_all
      if (is.null(results_df)) return(NULL)
      
      selected_question <- results_df[row_index, ]
    })
    
    if (is.null(table_data)) {
      showNotification("No se pudo obtener información de la pregunta seleccionada", type = "error")
      return()
    }
    
    # Extract question info
    question_id <- selected_question$Variable
    question_survey_raw <- selected_question$Encuesta
    
    # Map the survey name back to the survey ID
    survey_id <- ifelse(
      grepl("Percepción", question_survey_raw),
      "PER_2024_V2",
      "PAR_2024_V2"
    )
    
    # Map the scale type to module name
    module_mapping <- c(
      "razon" = "razon",
      "Razon" = "razon",
      "intervalo" = "intervalo", 
      "Intervalo" = "intervalo",
      "ordinal" = "ordinal",
      "Ordinal" = "ordinal",
      "categorica" = "categorico",
      "Categorica" = "categorico", 
      "binaria" = "binaria",
      "Binaria" = "binaria",
      "nominal" = "nominal",
      "Nominal" = "nominal",
      "nominal (abierta)" = "nominal",
      "Nominal (Abierta)" = "nominal"
    )
    
    question_type <- selected_question$Tipo
    question_module <- module_mapping[question_type]
    
    if (is.na(question_module)) {
      question_module <- "categorico"  # Default fallback
    }
    
    # CRITICAL SAFETY CHECK: Verify the question exists in the target survey
    temp_survey_data <- tryCatch({
      temp_data <- load_survey_data(survey_id)
      if (question_id %in% names(temp_data$responses)) {
        TRUE  # Question exists
      } else {
        FALSE  # Question doesn't exist
      }
    }, error = function(e) {
      FALSE  # Error loading survey
    })
    
    if (!temp_survey_data) {
      showNotification(
        paste0("La pregunta ", question_id, " no existe en la encuesta ", survey_id), 
        type = "error"
      )
      return()
    }
    
    # First switch to the correct survey if needed
    current_survey <- input$survey_selector
    
    if (current_survey != survey_id) {
      # Update the survey selector
      updateRadioButtons(session, "survey_selector", selected = survey_id)
      
      # Wait for data to load before proceeding
      showNotification(
        paste0("Cambiando a encuesta: ", survey_id), 
        type = "message"
      )
      
      # Use a longer delay
      shinyjs::delay(2000, {
        # Continue with module selection
        updateTabsetPanel(session, inputId = "main_tabs", selected = "Prueba de Módulos")
        
        shinyjs::delay(500, {
          updateSelectInput(session, "test_module", selected = question_module)
          
          shinyjs::delay(800, {
            # Final safety check before updating question selection
            tryCatch({
              module_questions <- question_classification()[[question_module]]
              if (question_id %in% module_questions) {
                updateSelectInput(session, "test_question", selected = question_id)
                showNotification(
                  paste0("Mostrando pregunta: ", question_id), 
                  type = "message"
                )
              } else {
                showNotification(
                  paste0("Pregunta ", question_id, " no encontrada en módulo ", question_module), 
                  type = "warning"
                )
              }
            }, error = function(e) {
              showNotification(
                paste0("Error al cargar la pregunta: ", e$message), 
                type = "error"
              )
            })
          })
        })
      })
    } else {
      # No survey change needed, proceed directly
      updateTabsetPanel(session, inputId = "main_tabs", selected = "Prueba de Módulos")
      
      shinyjs::delay(300, {
        updateSelectInput(session, "test_module", selected = question_module)
        
        shinyjs::delay(500, {
          tryCatch({
            module_questions <- question_classification()[[question_module]]
            if (question_id %in% module_questions) {
              updateSelectInput(session, "test_question", selected = question_id)
              showNotification(
                paste0("Mostrando pregunta: ", question_id), 
                type = "message"
              )
            } else {
              showNotification(
                paste0("Pregunta ", question_id, " no encontrada en módulo ", question_module), 
                type = "warning"
              )
            }
          }, error = function(e) {
            showNotification(
              paste0("Error al cargar la pregunta: ", e$message), 
              type = "error"
            )
          })
        })
      })
    }
  }
})

# Add automatic search when pressing Enter
observeEvent(input$global_search, {
  if (input$global_search != "" && !is.null(input$keyPressed) && input$keyPressed == 13) {
    shinyjs::click("execute_search")
  }
}, ignoreInit = TRUE)
 # Update the theme questions table in server.R
output$theme_questions_table <- DT::renderDataTable({
  # Get all theme data
  themes_data <- theme_metadata$load_thematic_classifications()
  
  # Filter based on selections
  if (input$theme_selector == "all") {
    # Show all themes
    questions <- themes_data
  } else if (input$subtheme_selector == "all") {
    # Show all subthemes for the selected theme
    questions <- themes_data %>%
      filter(MainTheme == input$theme_selector)
  } else {
    # Show specific theme and subtheme
    questions <- themes_data %>%
      filter(MainTheme == input$theme_selector, Subtheme == input$subtheme_selector)
  }
  
  # Add survey name column with friendly names
  questions$Encuesta <- ifelse(
    questions$survey_id == "PER_2024",
    "Percepción 2024",
    "Participación 2024"
  )
  
  # Rename columns for display
  questions <- questions %>%
    select(
      Variable = variable,
      Pregunta = label,
      Tipo = scale_type,
      Subtema = Subtheme,
      Tema = MainTheme,
      Encuesta
    )
  
  DT::datatable(
    questions,
    options = list(
      pageLength = 15,
      language = list(
        search = "Filtrar:",
        paginate = list(previous = "Anterior", `next` = "Siguiente")
      )
    ),
    selection = 'single'
  )
})
# Enhanced handler for theme question selection in server.R
observeEvent(input$theme_questions_table_rows_selected, {
  row_index <- input$theme_questions_table_rows_selected
  
  if (length(row_index) > 0) {
    # Get themes data
    themes_data <- theme_metadata$load_thematic_classifications()
    
    # Get filtered data based on selections
    if (input$theme_selector == "all") {
      filtered_data <- themes_data
    } else if (input$subtheme_selector == "all") {
      filtered_data <- themes_data %>%
        filter(MainTheme == input$theme_selector)
    } else {
      filtered_data <- themes_data %>%
        filter(MainTheme == input$theme_selector, Subtheme == input$subtheme_selector)
    }
    
    # Get the selected question info
    selected_question <- filtered_data[row_index, ]
    question_id <- selected_question$variable
    question_survey <- selected_question$survey_id
    
    # Determine the module type for this question
    question_type <- tolower(selected_question$scale_type)
    
    # Map scale_type to module name
    module_mapping <- c(
      "razon" = "razon",
      "intervalo" = "intervalo",
      "ordinal" = "ordinal",
      "categorica" = "categorico", 
      "binaria" = "binaria",
      "nominal (abierta)" = "nominal"
    )
    
    # Default fallback for unknown types
    question_module <- "categorico"
    
    # Try to match the question type to a module
    if (question_type %in% names(module_mapping)) {
      question_module <- module_mapping[question_type]
    }
    
    # Map survey_id to the corresponding radio button value
    survey_mapping <- c(
      "PER_2024" = "PER_2024_V2",
      "PAR_2024" = "PAR_2024_V2"
    )
    
    # Get target survey
    target_survey <- survey_mapping[question_survey]
    
    if (is.na(target_survey)) {
      showNotification(
        paste0("No se pudo determinar la encuesta para: ", question_id), 
        type = "error"
      )
      return()
    }
    
    # CRITICAL SAFETY CHECK: Verify the question exists in the target survey
    # Load the target survey data temporarily to check
    temp_survey_data <- tryCatch({
      temp_data <- load_survey_data(target_survey)
      if (question_id %in% names(temp_data$responses)) {
        TRUE  # Question exists
      } else {
        FALSE  # Question doesn't exist
      }
    }, error = function(e) {
      FALSE  # Error loading survey
    })
    
    if (!temp_survey_data) {
      showNotification(
        paste0("La pregunta ", question_id, " no existe en la encuesta ", target_survey), 
        type = "error"
      )
      return()
    }
    
    # Now it's safe to switch surveys if needed
    current_survey <- input$survey_selector
    if (current_survey != target_survey) {
      # Update the survey selector
      updateRadioButtons(session, "survey_selector", selected = target_survey)
      
      # Notify user
      showNotification(
        paste0("Cambiando a encuesta: ", target_survey), 
        type = "message"
      )
      
      # Give it plenty of time to load (increase if still having issues)
      shinyjs::delay(2000, {
        # Now navigate to Module Testing tab
        updateTabsetPanel(session, inputId = "main_tabs", selected = "Prueba de Módulos")
        
        # Update module selection with more delay
        shinyjs::delay(500, {
          updateSelectInput(session, "test_module", selected = question_module)
          
          # Wait for module questions to load with even more delay
          shinyjs::delay(800, {
            # Final safety check before updating question selection
            tryCatch({
              module_questions <- question_classification()[[question_module]]
              if (question_id %in% module_questions) {
                updateSelectInput(session, "test_question", selected = question_id)
                showNotification(
                  paste0("Mostrando pregunta: ", question_id), 
                  type = "message"
                )
              } else {
                showNotification(
                  paste0("Pregunta ", question_id, " no encontrada en módulo ", question_module), 
                  type = "warning"
                )
              }
            }, error = function(e) {
              showNotification(
                paste0("Error al cargar la pregunta: ", e$message), 
                type = "error"
              )
            })
          })
        })
      })
    } else {
      # No survey change needed, proceed directly but still with safety checks
      updateTabsetPanel(session, inputId = "main_tabs", selected = "Prueba de Módulos")
      
      shinyjs::delay(300, {
        updateSelectInput(session, "test_module", selected = question_module)
        
        shinyjs::delay(500, {
          tryCatch({
            module_questions <- question_classification()[[question_module]]
            if (question_id %in% module_questions) {
              updateSelectInput(session, "test_question", selected = question_id)
              showNotification(
                paste0("Mostrando pregunta: ", question_id), 
                type = "message"
              )
            } else {
              showNotification(
                paste0("Pregunta ", question_id, " no encontrada en módulo ", question_module), 
                type = "warning"
              )
            }
          }, error = function(e) {
            showNotification(
              paste0("Error al cargar la pregunta: ", e$message), 
              type = "error"
            )
          })
        })
      })
    }
  }
})
}