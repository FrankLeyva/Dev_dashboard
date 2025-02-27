server <- function(input, output, session) {
  data <- reactive({
    load_survey_data("data/processed/PER_2023_responses.csv", "data/processed/PER_2023_metadata_classified.csv")
  })

  
  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error loading geo data:", e$message), type = "error")
      NULL
    })
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
        metadata = reactive(data()$metadata),
        selected_question = selected_question,
        geo_data = geo_data
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
}