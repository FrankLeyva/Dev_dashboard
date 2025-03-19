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
    } else if (input$survey_selector == "PER_2024"){
      "2024 Percepción Ciudadana (PER 2024)"
    }else if (input$survey_selector == "PAR_2024"){
      "2024 Participación Ciudadana (PAR 2024)"
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
  
  # Display classification summary
  output$classification_summary <- renderTable({
    types <- question_classification()
    data.frame(
      Tipo = c("Razón", "Intervalo", "Ordinal", "Categórico", "Binaria", "Nominal"),
      Cantidad = sapply(types, length)
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
    
    if(input$show_metadata) {
      metadata_subset <- data()$metadata[data()$metadata$variable %in% questions, ]
      DT::datatable(
        metadata_subset,
        options = list(pageLength = 10),
        filter = 'top'
      )
    } else {
      DT::datatable(
        data.frame(Variable = questions),
        options = list(pageLength = 10),
        filter = 'top'
      )
    }
  })
  observe({
    # Clear the question selection first
    updateSelectInput(session, "test_question", choices = NULL, selected = NULL)
    
    # Then update with new choices
    questions <- question_classification()[[input$test_module]]
    updateSelectInput(
      session,
      "test_question",
      choices = questions
    )
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
  
  # Get all questions with their labels
  all_questions <- data.frame(
    variable = data()$metadata$variable,
    label = data()$metadata$label,
    scale_type = data()$metadata$scale_type,
    stringsAsFactors = FALSE
  )
  
  # Filter questions where label or variable contains the search text
  matching_questions <- all_questions[
    grepl(search_text, tolower(all_questions$label)) | 
    grepl(search_text, tolower(all_questions$variable)),
  ]
  
  # Create a nice results table
  results_df <- matching_questions %>%
    select(
      variable,
      Pregunta = label,
      Tipo = scale_type
    )
  
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

observeEvent(input$search_results_table_rows_selected, {
  row_index <- input$search_results_table_rows_selected
  
  # Make sure we have a selection and search results
  if (length(row_index) > 0 && !is.null(input$global_search)) {
    # Get all questions with their labels again
    all_questions <- data.frame(
      variable = data()$metadata$variable,
      label = data()$metadata$label,
      scale_type = data()$metadata$scale_type,
      stringsAsFactors = FALSE
    )
    
    # Filter to get matching questions
    search_text <- tolower(input$global_search)
    matching_questions <- all_questions[
      grepl(search_text, tolower(all_questions$label)) | 
      grepl(search_text, tolower(all_questions$variable)),
    ]
    
    # Get the selected question info
    if (row_index <= nrow(matching_questions)) {
      selected_question <- matching_questions[row_index, ]
      
      # Map the scale type to module name
      module_mapping <- c(
        "Razon" = "razon",
        "Intervalo" = "intervalo",
        "Ordinal" = "ordinal",
        "Categorica" = "categorico", 
        "Binaria" = "binaria",
        "Nominal (Abierta)" = "nominal"
      )
      
      question_module <- module_mapping[selected_question$scale_type]
      question_id <- selected_question$variable
      
      # First, navigate to the "Prueba de Módulos" tab
      updateTabsetPanel(session, inputId = "main_tabs", selected = "Prueba de Módulos")
      
      # Wait a moment to ensure the tab has switched
      shinyjs::delay(100, {
        # Update the module type
        updateSelectInput(session, "test_module", selected = question_module)
        
        # Wait for the module's questions to load
        shinyjs::delay(300, {
          # Now update the question selection
          # First get the list of questions for this module
          module_questions <- question_classification()[[question_module]]
          
          # Check if our question is in the list
          if (question_id %in% module_questions) {
            updateSelectInput(session, "test_question", selected = question_id)
            
            showNotification(
              paste0("Mostrando pregunta: ", question_id), 
              type = "message"
            )
          } else {
            showNotification(
              paste0("No se pudo seleccionar la pregunta, no se encontró en el módulo ", question_module), 
              type = "warning"
            )
          }
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
}