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
        metadata = reactive(data()$metadata)
      )
    } else if(input$test_module == "intervalo") {
      intervalServer(
        "interval_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data
      )
    } else if(input$test_module == "ordinal") {
      ordinalServer(
        "ordinal_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data
      )
    } else if(input$test_module == "categorico") {
      categoricoServer(
        "categorico_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data
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
        all_binary_questions = all_binary_questions
      )
    } else if(input$test_module == "nominal") {
      # Add the nominal module server
      nominalServer(
        "nominal_test",
        data = reactive(data()$responses),
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data
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