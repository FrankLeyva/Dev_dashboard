# nominal_module.R

# Function to prepare nominal data for analysis
prepare_nominal_data <- function(data, question_id, metadata) {
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
  
  # Count responses by type
  missing_count <- sum(is.na(subset_data$value))
  total_responses <- nrow(subset_data)
  
  # Filter to valid responses for analysis
  valid_data <- subset_data %>%
    filter(!is.na(value)) %>%
    mutate(
      district = as.factor(district),
      gender = as.factor(gender),
      age_group = as.factor(age_group),
      value = as.character(value)  # Ensure text values are character type
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
    attr(empty_data, "missing_count") <- missing_count
    attr(empty_data, "total_responses") <- total_responses
    attr(empty_data, "tokens") <- character()
    attr(empty_data, "word_freq") <- data.frame(word = character(), freq = integer())
    return(empty_data)
  }
  
  # Preprocess text data - normalize, remove punctuation, etc.
  preprocessed_text <- tolower(valid_data$value)
  preprocessed_text <- iconv(preprocessed_text, to = "ASCII//TRANSLIT")  # Remove accents
  preprocessed_text <- gsub("[[:punct:]]", " ", preprocessed_text)       # Remove punctuation
  preprocessed_text <- gsub("\\s+", " ", preprocessed_text)              # Normalize spaces
  preprocessed_text <- trimws(preprocessed_text)                         # Trim whitespace
  
  # Store preprocessed text
  valid_data$preprocessed_text <- preprocessed_text
  
  # Tokenize text into words
  tokens <- unlist(strsplit(preprocessed_text, "\\s+"))
  tokens <- tokens[tokens != ""]  # Remove empty strings
  
  # Calculate word frequencies
  word_freq <- as.data.frame(table(tokens))
  names(word_freq) <- c("word", "freq")
  # Explicitly convert the word column to character
  word_freq$word <- as.character(word_freq$word)
  word_freq <- word_freq[order(-word_freq$freq), ]
  
  # Add text analysis attributes to the data
  attr(valid_data, "missing_count") <- missing_count
  attr(valid_data, "total_responses") <- total_responses
  attr(valid_data, "tokens") <- tokens
  attr(valid_data, "word_freq") <- word_freq
  attr(valid_data, "question_label") <- get_question_label(question_id, metadata)

  return(valid_data)
}

# Function to get common Spanish stopwords
get_spanish_stopwords <- function() {
  c("a", "al", "algo", "algunas", "algunos", "ante", "antes", "como", "con", "contra",
    "cual", "cuando", "de", "del", "desde", "donde", "durante", "e", "el", "ella",
    "ellas", "ellos", "en", "entre", "era", "erais", "eran", "eras", "eres", "es",
    "esa", "esas", "ese", "eso", "esos", "esta", "estaba", "estabais", "estaban",
    "estabas", "estad", "estada", "estadas", "estado", "estados", "estamos", "estando",
    "estar", "estaremos", "estará", "estarán", "estarás", "estaré", "estaréis",
    "estaría", "estaríais", "estaríamos", "estarían", "estarías", "estas", "este",
    "estemos", "esto", "estos", "estoy", "estuve", "estuviera", "estuvierais",
    "estuvieran", "estuvieras", "estuvieron", "estuviese", "estuvieseis", "estuviesen",
    "estuvieses", "estuvimos", "estuviste", "estuvisteis", "estuviéramos", "estuviésemos",
    "estuvo", "está", "estábamos", "estáis", "están", "estás", "esté", "estéis", "estén",
    "estés", "fue", "fuera", "fuerais", "fueran", "fueras", "fueron", "fuese", "fueseis",
    "fuesen", "fueses", "fui", "fuimos", "fuiste", "fuisteis", "fuéramos", "fuésemos",
    "ha", "habida", "habidas", "habido", "habidos", "habiendo", "habremos", "habrá",
    "habrán", "habrás", "habré", "habréis", "habría", "habríais", "habríamos", "habrían",
    "habrías", "habéis", "había", "habíais", "habíamos", "habían", "habías", "han",
    "has", "hasta", "hay", "haya", "hayamos", "hayan", "hayas", "hayáis", "he", "hemos",
    "hube", "hubiera", "hubierais", "hubieran", "hubieras", "hubieron", "hubiese",
    "hubieseis", "hubiesen", "hubieses", "hubimos", "hubiste", "hubisteis", "hubiéramos",
    "hubiésemos", "hubo", "la", "las", "le", "les", "lo", "los", "me", "mi", "mis",
    "mucho", "muchos", "muy", "más", "mí", "mía", "mías", "mío", "míos", "nada", "ni",
    "no", "nos", "nosotras", "nosotros", "nuestra", "nuestras", "nuestro", "nuestros",
    "o", "os", "otra", "otras", "otro", "otros", "para", "pero", "poco", "por", "porque",
    "que", "quien", "quienes", "qué", "se", "sea", "seamos", "sean", "seas", "sepa", "sepamos",
    "sepan", "sepas", "sepa", "sepáis", "ser", "seremos", "será", "serán", "serás", "seré",
    "seréis", "sería", "seríais", "seríamos", "serían", "serías", "seáis", "si", "sido",
    "siendo", "sin", "sintiendo", "sobre", "sois", "somos", "son", "soy", "su", "sus",
    "suya", "suyas", "suyo", "suyos", "sí", "también", "tanto", "te", "tendremos", "tendrá",
    "tendrán", "tendrás", "tendré", "tendréis", "tendría", "tendríais", "tendríamos",
    "tendrían", "tendrías", "tened", "tenemos", "tenga", "tengamos", "tengan", "tengas",
    "tengo", "tengáis", "tenida", "tenidas", "tenido", "tenidos", "teniendo", "tenéis",
    "tenía", "teníais", "teníamos", "tenían", "tenías", "ti", "tiene", "tienen", "tienes",
    "todo", "todos", "tu", "tus", "tuve", "tuviera", "tuvierais", "tuvieran", "tuvieras",
    "tuvieron", "tuviese", "tuvieseis", "tuviesen", "tuvieses", "tuvimos", "tuviste",
    "tuvisteis", "tuviéramos", "tuviésemos", "tuvo", "tuya", "tuyas", "tuyo", "tuyos",
    "tú", "un", "una", "uno", "unos", "vosotras", "vosotros", "vuestra", "vuestras",
    "vuestro", "vuestros", "y", "ya", "yo", "él", "éramos")
}

# Function to get the most frequent word
get_most_frequent_word <- function(data, exclude_stopwords = TRUE) {
  word_freq <- attr(data, "word_freq")
  
  if (exclude_stopwords) {
    stopwords <- get_spanish_stopwords()
    word_freq <- word_freq[!word_freq$word %in% stopwords, ]
  }
  
  if (nrow(word_freq) == 0) {
    return(NA)
  }
  
  return(word_freq$word[1])
}

# Create word frequency table
create_word_freq_table <- function(data, 
                                   max_words = 20, 
                                   exclude_stopwords = TRUE,
                                   min_chars = 3) {
  # Get word frequency from data attributes
  word_freq <- attr(data, "word_freq")
  
  # Make sure the word column is character
  word_freq$word <- as.character(word_freq$word)
  
  # Apply filters
  if (exclude_stopwords) {
    stopwords <- get_spanish_stopwords()
    word_freq <- word_freq[!word_freq$word %in% stopwords, ]
  }
  
  # Filter by minimum word length
  word_freq <- word_freq[nchar(word_freq$word) >= min_chars, ]
  
  # Limit to max words
  if (nrow(word_freq) > max_words) {
    word_freq <- word_freq[1:max_words, ]
  }
  
  return(word_freq)
}

# Create horizontal bar chart of word frequencies
create_word_freq_bars <- function(data, 
                                  max_words = 20, 
                                  exclude_stopwords = TRUE,
                                  min_chars = 3) {
  # Check if we have word frequency data
  if (is.null(attr(data, "word_freq"))) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos de frecuencia de palabras disponibles"))
  }
  
  # Get the word frequency table
  tryCatch({
    word_freq <- create_word_freq_table(data, max_words, exclude_stopwords, min_chars)
    
    if (nrow(word_freq) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay palabras que cumplan con los criterios de filtrado"))
    }
    
    # Create horizontal bar chart
    plot_ly(
      data = word_freq,
      y = ~reorder(word, freq),
      x = ~freq,
      type = "bar",
      orientation = 'h',
      marker = list(
        color = theme_config$colors$primary
      ),
      text = ~paste0(word, ": ", freq),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Frecuencia de Palabras",
        xlab = "Frecuencia",
        ylab = ""
      )
  }, error = function(e) {
    warning(paste("Error in create_word_freq_bars:", e$message))
    return(plotly_empty() %>% 
             layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create word cloud
create_word_cloud <- function(data, max_words = 100, exclude_stopwords = TRUE, min_chars = 3) {
  tryCatch({
    # Check if wordcloud2 is available
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(plotly_empty() %>% 
               layout(title = "Paquete wordcloud2 no disponible. Instale con install.packages('wordcloud2')"))
    }
    
    # Get word frequency table
    word_freq <- create_word_freq_table(data, max_words, exclude_stopwords, min_chars)
    
    if (nrow(word_freq) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay palabras que cumplan con los criterios de filtrado"))
    }
    
    # Create wordcloud
    wordcloud2::wordcloud2(data = word_freq, size = 0.5, color = "random-dark")
    
  }, error = function(e) {
    warning(paste("Error in create_word_cloud:", e$message))
    return(plotly_empty() %>% 
             layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create district-word heatmap
create_district_word_heatmap <- function(data, 
                                        max_words = 10, 
                                        exclude_stopwords = TRUE,
                                        min_chars = 3) {
  tryCatch({
    # Input validation
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos suficientes para visualizar"))
    }
    
    # Process data by district and word
    word_list <- list()
    districts <- unique(data$district)
    
    for (dist in districts) {
      # Get text for this district
      district_text <- data$preprocessed_text[data$district == dist]
      district_text <- paste(district_text, collapse = " ")
      
      # Tokenize
      tokens <- unlist(strsplit(district_text, "\\s+"))
      tokens <- tokens[tokens != ""]  # Remove empty strings
      
      # Filter stopwords and short words
      if (exclude_stopwords) {
        stopwords <- get_spanish_stopwords()
        tokens <- tokens[!tokens %in% stopwords]
      }
      # Convert to character to ensure nchar works properly
      tokens <- as.character(tokens)
      tokens <- tokens[nchar(tokens) >= min_chars]
      
      # Calculate frequencies
      word_freq <- as.data.frame(table(tokens))
      names(word_freq) <- c("word", "freq")
      word_freq <- word_freq[order(-word_freq$freq), ]
      
      # Add to list
      word_list[[as.character(dist)]] <- word_freq
    }
    
    # Get top words across all districts
    all_words <- unique(unlist(lapply(word_list, function(df) df$word)))
    top_words <- numeric(length(all_words))
    names(top_words) <- all_words
    
    for (dist in names(word_list)) {
      for (i in 1:nrow(word_list[[dist]])) {
        word <- as.character(word_list[[dist]]$word[i])
        top_words[word] <- top_words[word] + word_list[[dist]]$freq[i]
      }
    }
    
    top_words <- sort(top_words, decreasing = TRUE)
    if (length(top_words) > max_words) {
      top_words <- top_words[1:max_words]
    }
    
    # Create matrix for heatmap
    heatmap_data <- matrix(0, nrow = length(names(word_list)), ncol = length(names(top_words)))
    rownames(heatmap_data) <- names(word_list)
    colnames(heatmap_data) <- names(top_words)
    
    for (i in 1:length(names(word_list))) {
      dist <- names(word_list)[i]
      for (j in 1:length(names(top_words))) {
        word <- names(top_words)[j]
        idx <- which(word_list[[dist]]$word == word)
        if (length(idx) > 0) {
          heatmap_data[i, j] <- word_list[[dist]]$freq[idx]
        }
      }
    }
    
    # Convert to data frame for plotly
    heatmap_df <- expand.grid(district = rownames(heatmap_data), word = colnames(heatmap_data))
    heatmap_df$freq <- as.vector(heatmap_data)
    
    # Create heatmap
    plot_ly(
      data = heatmap_df,
      x = ~word,
      y = ~district,
      z = ~freq,
      type = "heatmap",
      colorscale = "Blues",
      text = ~paste0(district, " - ", word, ": ", freq),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Palabras Clave por Distrito",
        xlab = "",
        ylab = "Distrito"
      ) %>%
      layout(
        xaxis = list(tickangle = 45)
      )
    
  }, error = function(e) {
    warning(paste("Error in create_district_word_heatmap:", e$message))
    return(plotly_empty() %>% 
             layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create word treemap
create_word_treemap <- function(data, 
                               max_words = 30, 
                               exclude_stopwords = TRUE,
                               min_chars = 3) {
  tryCatch({
    # Get word frequency table
    word_freq <- create_word_freq_table(data, max_words, exclude_stopwords, min_chars)
    
    if (nrow(word_freq) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay palabras que cumplan con los criterios de filtrado"))
    }
    
    # Create treemap data
    treemap_data <- data.frame(
      ids = word_freq$word,
      labels = word_freq$word,
      parents = rep("", nrow(word_freq)),
      values = word_freq$freq
    )
    
    # Create treemap
    plot_ly(
      data = treemap_data,
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      type = "treemap",
      branchvalues = "total",
      textinfo = "label+value",
      hoverinfo = "label+value",
      marker = list(
        colorscale = "Blues",
        line = list(width = 1)
      )
    ) %>%
      layout(
        title = "Treemap de Palabras"
      )
  }, error = function(e) {
    warning(paste("Error in create_word_treemap:", e$message))
    return(plotly_empty() %>% 
             layout(title = paste("Error en la visualización:", e$message)))
  })
}

# Create bigram network (word pairs that appear together)
create_bigram_network <- function(data, 
                                 max_bigrams = 30, 
                                 exclude_stopwords = TRUE,
                                 min_chars = 3) {
  tryCatch({
    # Check for required packages
    if (!requireNamespace("igraph", quietly = TRUE)) {
      return(plotly_empty() %>% 
               layout(title = "Paquete igraph no disponible. Instale con install.packages('igraph')"))
    }
    
    # Extract and process text
    all_text <- paste(data$preprocessed_text, collapse = " ")
    
    # Create bigrams
    words <- unlist(strsplit(all_text, "\\s+"))
    words <- words[words != ""]  # Remove empty strings
    
    # Filter by stopwords and length
    if (exclude_stopwords) {
      stopwords <- get_spanish_stopwords()
      words <- words[!words %in% stopwords]
    }
    # Convert to character to ensure nchar works properly
    words <- as.character(words)
    words <- words[nchar(words) >= min_chars]
    
    # Create bigrams
    if (length(words) < 2) {
      return(plotly_empty() %>% 
               layout(title = "No hay suficientes palabras para crear bigramas"))
    }
    
    bigrams <- data.frame(
      word1 = words[1:(length(words)-1)],
      word2 = words[2:length(words)]
    )
    
    # Count bigram frequencies
    bigram_counts <- bigrams %>%
      group_by(word1, word2) %>%
      summarise(weight = n(), .groups = 'drop') %>%
      arrange(desc(weight))
    
    # Limit to max_bigrams
    if (nrow(bigram_counts) > max_bigrams) {
      bigram_counts <- bigram_counts[1:max_bigrams, ]
    }
    
    # Create network graph
    g <- igraph::graph_from_data_frame(bigram_counts)
    
    # Calculate node sizes based on degree
    deg <- igraph::degree(g)
    V(g)$size <- 5 + 3 * log(deg + 1)
    
    # Create layout
    layout <- igraph::layout_with_fr(g)
    
    # Create plotly network visualization
    edge_x <- c()
    edge_y <- c()
    for (i in 1:nrow(bigram_counts)) {
      from_idx <- match(bigram_counts$word1[i], names(V(g)))
      to_idx <- match(bigram_counts$word2[i], names(V(g)))
      
      edge_x <- c(edge_x, layout[from_idx, 1], layout[to_idx, 1], NA)
      edge_y <- c(edge_y, layout[from_idx, 2], layout[to_idx, 2], NA)
    }
    
    edge_trace <- list(
      type = "scatter",
      x = edge_x,
      y = edge_y,
      mode = "lines",
      line = list(width = 0.5, color = 'lightgrey'),
      hoverinfo = "none"
    )
    
    node_x <- layout[, 1]
    node_y <- layout[, 2]
    
    node_trace <- list(
      type = "scatter",
      x = node_x,
      y = node_y,
      mode = "markers+text",
      text = names(V(g)),
      textfont = list(family = "Arial", size = 10),
      marker = list(
        size = V(g)$size,
        color = theme_config$colors$primary,
        line = list(width = 1)
      ),
      hoverinfo = "text"
    )
    
    plot_ly() %>%
      add_trace(type = edge_trace$type, x = edge_trace$x, y = edge_trace$y, 
                mode = edge_trace$mode, line = edge_trace$line, hoverinfo = edge_trace$hoverinfo,
                name = "Connections") %>%
      add_trace(type = node_trace$type, x = node_trace$x, y = node_trace$y, 
                mode = node_trace$mode, text = node_trace$text, textfont = node_trace$textfont,
                marker = node_trace$marker, hoverinfo = node_trace$hoverinfo,
                name = "Words") %>%
      layout(
        title = "Red de Bigramas (Pares de Palabras)",
        showlegend = FALSE,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
    
  }, error = function(e) {
    warning(paste("Error in create_bigram_network:", e$message))
    return(plotly_empty() %>% 
             layout(title = paste("Error en la visualización:", e$message)))
  })
}

# UI Definition for the nominal module
nominalUI <- function(id) {
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
              "Frecuencia de Palabras" = "word_freq",
              "Nube de Palabras" = "word_cloud",
              "Mapa de Calor por Distrito" = "district_heatmap",
              "Treemap de Palabras" = "word_treemap",
              "Red de Bigramas" = "bigram_network"
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
          
          # Text processing options
          card_header("Opciones de Procesamiento de Texto"),
          checkboxInput(
            ns("exclude_stopwords"),
            "Excluir Palabras Comunes (Stopwords)",
            value = TRUE
          ),
          sliderInput(
            ns("min_chars"),
            "Longitud Mínima de Palabra",
            min = 1,
            max = 10,
            value = 3
          )
        ),
          accordion_panel(
            "Opciones Adicionales",
          conditionalPanel(
            condition = sprintf("input['%s'] == 'word_freq'", ns("plot_type")),
            sliderInput(
              ns("max_words_freq"),
              "Número Máximo de Palabras",
              min = 5,
              max = 50,
              value = 20
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'word_cloud'", ns("plot_type")),
            sliderInput(
              ns("max_words_cloud"),
              "Número Máximo de Palabras",
              min = 20,
              max = 200,
              value = 100
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'district_heatmap'", ns("plot_type")),
            sliderInput(
              ns("max_words_heatmap"),
              "Número Máximo de Palabras",
              min = 5,
              max = 30,
              value = 10
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'word_treemap'", ns("plot_type")),
            sliderInput(
              ns("max_words_treemap"),
              "Número Máximo de Palabras",
              min = 10,
              max = 50,
              value = 30
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bigram_network'", ns("plot_type")),
            sliderInput(
              ns("max_bigrams"),
              "Número Máximo de Bigramas",
              min = 10,
              max = 50,
              value = 30
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

# Server Definition for the nominal module
nominalServer <- function(id, data, metadata, selected_question, geo_data) {
  moduleServer(id, function(input, output, session) {
    # Initial data preparation with metadata
    prepared_data <- reactive({
      tryCatch({
        req(data(), selected_question(), metadata())
        
        # Add validation
        if (is.null(selected_question()) || selected_question() == "") {
          return(NULL)
        }
        
        prepare_nominal_data(data(), selected_question(), metadata())
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
        
        # Need to recalculate text analysis after filtering
        preprocessed_text <- data$preprocessed_text
        
        # Tokenize text into words
        tokens <- unlist(strsplit(preprocessed_text, "\\s+"))
        tokens <- tokens[tokens != ""]  # Remove empty strings
        
        # Calculate word frequencies
        word_freq <- as.data.frame(table(tokens))
        names(word_freq) <- c("word", "freq")
        # Ensure word column is character
        word_freq$word <- as.character(word_freq$word)
        word_freq <- word_freq[order(-word_freq$freq), ]
        
        # Update attributes
        attr(data, "tokens") <- tokens
        attr(data, "word_freq") <- word_freq
        
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
        "word_freq" = plotlyOutput(session$ns("word_freq_plot"), height = "600px"),
        "word_cloud" = htmlOutput(session$ns("word_cloud_plot"), height = "600px"),
        "district_heatmap" = plotlyOutput(session$ns("district_heatmap_plot"), height = "600px"),
        "word_treemap" = plotlyOutput(session$ns("word_treemap_plot"), height = "600px"),
        "bigram_network" = plotlyOutput(session$ns("bigram_network_plot"), height = "600px")
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
        
        # Get response counts from attributes
        missing_count <- attr(data, "missing_count")
        total_responses <- attr(data, "total_responses")
        valid_responses <- nrow(data)
        
        # Get word frequency data
        word_freq <- attr(data, "word_freq")
        
        # Apply filters for word analysis
        filtered_word_freq <- word_freq
        
        if (input$exclude_stopwords) {
          stopwords <- get_spanish_stopwords()
          filtered_word_freq <- filtered_word_freq[!filtered_word_freq$word %in% stopwords, ]
        }
        
        filtered_word_freq <- filtered_word_freq[nchar(filtered_word_freq$word) >= input$min_chars, ]
        
        cat("Estadísticas para Datos Nominales (Abiertos):\n")
        cat("\nDistribución de Respuestas:\n")
        cat("Total de respuestas:", total_responses, "\n")
        cat("Respuestas válidas:", valid_responses, "\n")
        cat("Datos faltantes:", missing_count,
            sprintf("(%.1f%%)", 100 * missing_count/total_responses), "\n")
        
        cat("\nEstadísticas de Texto:\n")
        cat("Total de palabras (tokens):", length(attr(data, "tokens")), "\n")
        cat("Palabras únicas:", nrow(word_freq), "\n")
        
        if (nrow(filtered_word_freq) > 0) {
          cat("\nPalabras más frecuentes (excluyendo stopwords y palabras cortas):\n")
          top_words <- head(filtered_word_freq, 10)
          print(top_words)
          
          cat("\nEstadísticas de longitud de respuesta:\n")
          response_lengths <- sapply(strsplit(data$preprocessed_text, "\\s+"), length)
          cat("Promedio de palabras por respuesta:", round(mean(response_lengths), 2), "\n")
          cat("Mediana de palabras por respuesta:", median(response_lengths), "\n")
          cat("Respuesta más corta:", min(response_lengths), "palabras\n")
          cat("Respuesta más larga:", max(response_lengths), "palabras\n")
        } else {
          cat("\nNo hay palabras que cumplan con los criterios de filtrado.\n")
        }
        
        cat("\nDistribución por Distrito (número de respuestas):\n")
        print(table(data$district))
        
        cat("\nDistribución por Género (número de respuestas):\n")
        print(table(data$gender))
        
        cat("\nDistribución por Grupo de Edad (número de respuestas):\n")
        print(table(data$age_group))
        
      }, error = function(e) {
        cat("Error al generar estadísticas:", e$message)
      })
    })
    
    # Word frequency bar chart
    output$word_freq_plot <- renderPlotly({
      req(filtered_data())
      create_word_freq_bars(
        filtered_data(),
        max_words = input$max_words_freq,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars
      )
    })
    
    output$word_cloud_plot <- renderUI({
      req(filtered_data())
      # Create word frequency table
      word_freq <- create_word_freq_table(
        filtered_data(),
        max_words = input$max_words_cloud,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars
      )
      
      if (nrow(word_freq) == 0) {
        return(p("No hay palabras que cumplan con los criterios de filtrado"))
      }
      
      # Create and return the wordcloud
      tryCatch({
        # Use the simpler version without elementId
        html <- wordcloud2::wordcloud2(data = word_freq, size = 0.5, color = "random-dark")
        return(html)
      }, error = function(e) {
        return(p(paste("Error al generar la nube de palabras:", e$message)))
      })
    })
    
    # District word heatmap
    output$district_heatmap_plot <- renderPlotly({
      req(filtered_data())
      create_district_word_heatmap(
        filtered_data(),
        max_words = input$max_words_heatmap,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars
      )
    })
    
    # Word treemap
    output$word_treemap_plot <- renderPlotly({
      req(filtered_data())
      create_word_treemap(
        filtered_data(),
        max_words = input$max_words_treemap,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars
      )
    })
    
    # Bigram network
    output$bigram_network_plot <- renderPlotly({
      req(filtered_data())
      create_bigram_network(
        filtered_data(),
        max_bigrams = input$max_bigrams,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars
      )
    })
  })
}