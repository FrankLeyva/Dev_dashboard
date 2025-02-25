# binary_module.R

process_binary_data <- function(data, question_id, metadata) {
  message(sprintf("Processing binary data for question: %s", question_id))
  
  # Get metadata information
  question_meta <- metadata %>%
    filter(variable == question_id)
  
  message("Metadata for question:")
  print(question_meta)
  
  # Get value labels if they exist
  value_labels <- if(question_meta$has_value_labels) {
    # Clean and split labels
    labels <- str_split(
      iconv(question_meta$value_labels, from = "", to = "UTF-8", sub = ""),
      ";"
    )[[1]]
    message("Using value labels from metadata:", paste(labels, collapse = ", "))
    labels
  } else {
    message("Using default No/Sí labels")
    c("No", "Sí")
  }
  
  message("Processing data...")
  processed <- data %>%
    select(response = !!sym(question_id), 
           district = Q2, 
           gender = Q101, 
           age = Q103)
  
  message("Sample of raw data:")
  print(head(processed))
  
  processed <- processed %>%
    mutate(
      # Clean all character columns
      across(where(is.character), ~iconv(., from = "", to = "UTF-8", sub = "")),
      
      # Convert string "NA" to actual NA
      across(everything(), ~ifelse(. == "NA", NA_character_, .)),
      
      # Handle binary response
      response = case_when(
        response %in% c("0", "No", "2") ~ "No",
        response %in% c("1", "Sí", "Si", "1") ~ "Sí",
        TRUE ~ NA_character_
      ),
      response = factor(response, levels = c("No", "Sí")),
      
      # Handle gender
      gender = case_when(
        gender %in% c("1", "Hombre") ~ "Masculino",
        gender %in% c("2", "Mujer") ~ "Femenino",
        TRUE ~ NA_character_
      ),
      gender = factor(gender),
      
      # Convert district and age to factors
      district = factor(district),
      age = factor(age)
    ) %>%
    filter(!is.na(response))
  
  message("Sample of processed data:")
  print(head(processed))
  message(sprintf("Number of rows after processing: %d", nrow(processed)))
  
  return(processed)
}

create_binary_viz <- function(data, viz_type, title = "") {
  message(sprintf("Creating visualization: %s", viz_type))
  message(sprintf("Input data dimensions: %d rows, %d columns", nrow(data), ncol(data)))
  
  result <- tryCatch({
    switch(viz_type,
      "bar" = {
        message("Creating bar chart")
        plot_data <- data %>%
          count(response) %>%
          mutate(prop = n/sum(n))
        
        message("Plot data:")
        print(plot_data)
        
        plot_ly(plot_data) %>%
          add_bars(x = ~response, y = ~prop,
                  text = ~scales::percent(prop),
                  hoverinfo = "text") %>%
          layout(
            title = title,
            xaxis = list(title = "Respuesta"),
            yaxis = list(title = "Proporción",
                        tickformat = "%"),
            showlegend = FALSE
          )
      },
      "demographic_bars" = {
        message("Creating demographic bars")
        plot_data <- data %>%
          group_by(gender, response) %>%
          summarise(n = n(), .groups = 'drop') %>%
          group_by(gender) %>%
          mutate(prop = n/sum(n))
        
        message("Plot data:")
        print(plot_data)
        
        plot_ly(plot_data) %>%
          add_bars(x = ~gender, y = ~prop, color = ~response,
                  text = ~scales::percent(prop),
                  hoverinfo = "text") %>%
          layout(
            title = title,
            xaxis = list(title = "Género"),
            yaxis = list(title = "Proporción",
                        tickformat = "%"),
            barmode = 'group'
          )
      },
      "age_bars" = {
        message("Creating age bars")
        plot_data <- data %>%
          group_by(age, response) %>%
          summarise(n = n(), .groups = 'drop') %>%
          group_by(age) %>%
          mutate(prop = n/sum(n))
        
        message("Plot data:")
        print(plot_data)
        
        plot_ly(plot_data) %>%
          add_bars(x = ~age, y = ~prop, color = ~response,
                  text = ~scales::percent(prop),
                  hoverinfo = "text") %>%
          layout(
            title = title,
            xaxis = list(title = "Grupo de Edad",
                        tickangle = 45),
            yaxis = list(title = "Proporción",
                        tickformat = "%"),
            barmode = 'group'
          )
      },
      "district_bars" = {
        message("Creating district bars")
        plot_data <- data %>%
          group_by(district, response) %>%
          summarise(n = n(), .groups = 'drop') %>%
          group_by(district) %>%
          mutate(prop = n/sum(n))
        
        message("Plot data:")
        print(plot_data)
        
        plot_ly(plot_data) %>%
          add_bars(x = ~district, y = ~prop, color = ~response,
                  text = ~scales::percent(prop),
                  hoverinfo = "text") %>%
          layout(
            title = title,
            xaxis = list(title = "Distrito",
                        tickangle = 45),
            yaxis = list(title = "Proporción",
                        tickformat = "%"),
            barmode = 'stack'
          )
      }
    )
  }, error = function(e) {
    message(sprintf("Error in visualization: %s", e$message))
    NULL
  })
  
  message("Visualization created successfully")
  return(result)
}

create_binary_summary <- function(data) {
  message("Creating summary statistics")
  message(sprintf("Input data dimensions: %d rows, %d columns", nrow(data), ncol(data)))
  
  tryCatch({
    # Overall proportions
    overall_stats <- data %>%
      count(response) %>%
      mutate(prop = n/sum(n))
    
    message("Overall statistics:")
    print(overall_stats)
    
    # Gender breakdown
    gender_stats <- data %>%
      filter(!is.na(gender)) %>%
      group_by(gender, response) %>%
      summarise(n = n(), .groups = 'drop') %>%
      group_by(gender) %>%
      mutate(prop = n/sum(n))
    
    message("Gender statistics:")
    print(gender_stats)
    
    # Age breakdown
    age_stats <- data %>%
      filter(!is.na(age)) %>%
      group_by(age, response) %>%
      summarise(n = n(), .groups = 'drop') %>%
      group_by(age) %>%
      mutate(prop = n/sum(n))
    
    message("Age statistics:")
    print(age_stats)
    
    # Create summary text
    summary_text <- paste0(
      "Estadísticas Descriptivas:\n",
      "Total de respuestas válidas: ", sum(overall_stats$n), "\n\n",
      "Distribución general:\n",
      paste(
        overall_stats$response,
        paste0("(n=", overall_stats$n, ", ",
               scales::percent(overall_stats$prop), ")"),
        sep = ": ",
        collapse = "\n"
      ),
      "\n\nPor Género:\n",
      paste(
        unique(gender_stats$gender),
        sapply(unique(gender_stats$gender), function(g) {
          stats <- filter(gender_stats, gender == g)
          paste(
            stats$response,
            scales::percent(stats$prop),
            sep = ": ",
            collapse = "; "
          )
        }),
        sep = ": ",
        collapse = "\n"
      ),
      "\n\nPor Grupo de Edad:\n",
      paste(
        unique(age_stats$age),
        sapply(unique(age_stats$age), function(a) {
          stats <- filter(age_stats, age == a)
          paste(
            stats$response,
            scales::percent(stats$prop),
            sep = ": ",
            collapse = "; "
          )
        }),
        sep = ": ",
        collapse = "\n"
      )
    )
    
    message("Summary created successfully")
    return(summary_text)
    
  }, error = function(e) {
    message(sprintf("Error in summary creation: %s", e$message))
    return("Error creating summary statistics")
  })
}

binaryUI <- function(id) {
  ns <- NS(id)
  message(sprintf("Creating UI for module: %s", id))
  
  tagList(
    fluidRow(
      column(3,
        card(
          card_header("Filtros"),
          checkboxGroupInput(ns("district_filter"), 
                           "Distrito:",
                           choices = NULL),
          checkboxGroupInput(ns("gender_filter"), 
                           "Género:",
                           choices = NULL),
          checkboxGroupInput(ns("age_filter"), 
                           "Grupo de Edad:",
                           choices = NULL)
        )
      ),
      column(9,
        card(
          card_header("Visualización"),
          radioButtons(ns("viz_type"), "Tipo de Gráfica:",
                      choices = c(
                        "Barras Simples" = "bar",
                        "Por Género" = "demographic_bars",
                        "Por Edad" = "age_bars",
                        "Por Distrito" = "district_bars"
                      )),
          plotlyOutput(ns("plot"), height = "400px")
        ),
        card(
          card_header("Resumen Estadístico"),
          verbatimTextOutput(ns("summary"))
        )
      )
    )
  )
}

binaryServer <- function(id, data, question_id, metadata) {
  message(sprintf("Initializing binary server module: %s", id))
  
  moduleServer(id, function(input, output, session) {
    message("Setting up reactive elements")
    
    # Process the data with error handling
    processed_data <- reactive({
      message("Binary module: Processing data...")
      req(data(), question_id, metadata)
      
      tryCatch({
        result <- process_binary_data(data(), question_id, metadata)
        message(sprintf("Data processing complete. Number of rows: %d", nrow(result)))
        result
      }, error = function(e) {
        message(sprintf("Error processing data: %s", e$message))
        NULL
      })
    })
    
    # Apply filters with debugging
    filtered_data <- reactive({
      message("Binary module: Applying filters...")
      req(processed_data())
      
      current_data <- processed_data()
      message(sprintf("Starting with rows: %d", nrow(current_data)))
      
      if (!is.null(input$district_filter) && length(input$district_filter) > 0) {
        current_data <- current_data %>%
          filter(district %in% input$district_filter)
        message(sprintf("After district filter: %d", nrow(current_data)))
      }
      
      if (!is.null(input$gender_filter) && length(input$gender_filter) > 0) {
        current_data <- current_data %>%
          filter(gender %in% input$gender_filter)
        message(sprintf("After gender filter: %d", nrow(current_data)))
      }
      
      if (!is.null(input$age_filter) && length(input$age_filter) > 0) {
        current_data <- current_data %>%
          filter(age %in% input$age_filter)
        message(sprintf("After age filter: %d", nrow(current_data)))
      }
      
      message(sprintf("Final filtered data rows: %d", nrow(current_data)))
      current_data
    })
    
    # Create visualization with debugging
    output$plot <- renderPlotly({
      message("Binary module: Creating plot...")
      req(filtered_data())
      
      question_label <- metadata %>%
        filter(variable == question_id) %>%
        pull(label)
      
      message(sprintf("Creating visualization for: %s", question_label))
      
      result <- create_binary_viz(filtered_data(), 
                                input$viz_type,
                                title = question_label)
      
      message("Plot created")
      result
    })
    
    # Create summary statistics with debugging
    output$summary <- renderText({
      message("Binary module: Creating summary...")
      req(filtered_data())
      
      result <- create_binary_summary(filtered_data())
      message("Summary created")
      result
    })
    
    # Initialize filters with debugging
    observe({
      message("Binary module: Initializing filters...")
      req(processed_data())
      
      districts <- sort(unique(na.omit(processed_data()$district)))
      genders <- sort(unique(na.omit(processed_data()$gender)))
      ages <- sort(unique(na.omit(processed_data()$age)))
      
      message("Available filters:")
      message(sprintf("Districts: %s", paste(districts, collapse = ", ")))
      message(sprintf("Genders: %s", paste(genders, collapse = ", ")))
      message(sprintf("Ages: %s", paste(ages, collapse = ", ")))
      
      updateCheckboxGroupInput(session, "district_filter",
                             choices = districts,
                             selected = districts)
      
      updateCheckboxGroupInput(session, "gender_filter",
                             choices = genders,
                             selected = genders)
      
      updateCheckboxGroupInput(session, "age_filter",
                             choices = ages,
                             selected = ages)
      
      message("Filters initialized")
    })
  })
}