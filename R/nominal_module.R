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
      district = DISTRICT,  
      gender = GENDER,      
      age_group = AGE_GROUP 
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
  min_chars = 3,
  custom_theme = NULL) {
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

# Get primary color from theme
bar_color <- if (!is.null(custom_theme)) {
custom_theme$colors$primary
} else {
theme_config$colors$primary
}

# Create horizontal bar chart
plot_ly(
data = word_freq,
y = ~reorder(word, freq),
x = ~freq,
type = "bar",
orientation = 'h',
marker = list(
color = bar_color
),
text = ~paste0(word, ": ", freq),
hoverinfo = "text"
) %>%
apply_plotly_theme(
title = "Frecuencia de Palabras",
xlab = "Frecuencia",
ylab = "",
custom_theme = custom_theme
)
}, error = function(e) {
warning(paste("Error in create_word_freq_bars:", e$message))
return(plotly_empty() %>% 
layout(title = paste("Error en la visualización:", e$message)))
})
}


# Create district-word heatmap
create_district_word_heatmap <- function(data, 
  max_words = 10, 
  exclude_stopwords = TRUE,
  min_chars = 3,
  custom_theme = NULL) {
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

# Get colorscale from theme
colorscale <- if (!is.null(custom_theme) && !is.null(custom_theme$palettes$sequential)) {
custom_theme$palettes$sequential
} else {
"Blues"
}

# Create heatmap
plot_ly(
data = heatmap_df,
x = ~word,
y = ~district,
z = ~freq,
type = "heatmap",
colorscale = colorscale,
text = ~paste0(district, " - ", word, ": ", freq),
hoverinfo = "text"
) %>%
apply_plotly_theme(
title = "Palabras Clave por Distrito",
xlab = "",
ylab = "Distrito",
custom_theme = custom_theme
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
  min_chars = 3,
  custom_theme = NULL) {
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

# Get colorscale from theme
colorscale <- if (!is.null(custom_theme) && !is.null(custom_theme$palettes$sequential)) {
custom_theme$palettes$sequential
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
textinfo = "label+value",
hoverinfo = "label+value",
marker = list(
colorscale = colorscale,
line = list(width = 1)
)
) %>%
layout(
title = list(
text = "Treemap de Palabras",
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
warning(paste("Error in create_word_treemap:", e$message))
return(plotly_empty() %>% 
layout(title = paste("Error en la visualización:", e$message)))
})
}

# Create bigram network (word pairs that appear together)
create_bigram_network <- function(data, 
  max_bigrams = 30, 
  exclude_stopwords = TRUE,
  min_chars = 3,
  custom_theme = NULL) {
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

# Get primary and neutral colors from theme
primary_color <- if (!is.null(custom_theme)) {
custom_theme$colors$primary
} else {
theme_config$colors$primary
}

edge_color <- if (!is.null(custom_theme)) {
custom_theme$colors$neutral
} else {
"lightgrey"
}

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
line = list(width = 0.5, color = edge_color),
hoverinfo = "none"
)

node_x <- layout[, 1]
node_y <- layout[, 2]

# Apply custom font if available
text_font <- if (!is.null(custom_theme)) {
list(family = custom_theme$typography$font_family, size = 10)
} else {
list(family = "Arial", size = 10)
}

node_trace <- list(
type = "scatter",
x = node_x,
y = node_y,
mode = "markers+text",
text = names(V(g)),
textfont = text_font,
marker = list(
size = V(g)$size,
color = primary_color,
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
title = list(
text = "Red de Bigramas (Pares de Palabras)",
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
nominalServer <- function(id, data, metadata, selected_question, geo_data,current_theme = NULL) {
  moduleServer(id, function(input, output, session) {
    # Initial data preparation with metadata
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
          top_words <- head(filtered_word_freq, 15)
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
        
        # District Analysis
        cat("\nAnálisis por Distrito:\n")
        district_data <- data.frame(distrito = character(), palabra_top = character(), 
                                    frecuencia = integer(), total_palabras = integer(), 
                                    palabras_unicas = integer(), stringsAsFactors = FALSE)
        
        for (dist in unique(data$district)) {
          district_text <- data$preprocessed_text[data$district == dist]
          if (length(district_text) == 0) next
          
          district_tokens <- unlist(strsplit(paste(district_text, collapse = " "), "\\s+"))
          district_tokens <- district_tokens[district_tokens != ""]
          
          # Apply filters
          if (input$exclude_stopwords) {
            stopwords <- get_spanish_stopwords()
            district_tokens <- district_tokens[!district_tokens %in% stopwords]
          }
          district_tokens <- district_tokens[nchar(district_tokens) >= input$min_chars]
          
          if (length(district_tokens) == 0) {
            cat("Distrito", dist, ": Sin palabras que cumplan los criterios\n")
            next
          }
          
          # Find top word
          district_word_freq <- table(district_tokens)
          top_word <- names(district_word_freq)[which.max(district_word_freq)]
          top_freq <- max(district_word_freq)
          
          # Add to data frame
          district_data <- rbind(district_data, data.frame(
            distrito = dist,
            palabra_top = top_word,
            frecuencia = top_freq,
            total_palabras = length(district_tokens),
            palabras_unicas = length(unique(district_tokens)),
            stringsAsFactors = FALSE
          ))
          
          cat("Distrito", dist, "- Palabra más común:", top_word, "(", top_freq, "veces) -",
              "Total palabras:", length(district_tokens), "-",
              "Palabras únicas:", length(unique(district_tokens)), "\n")
        }
        
        # Gender Analysis
        cat("\nAnálisis por Género:\n")
        gender_data <- data.frame(genero = character(), palabra_top = character(), 
                                  frecuencia = integer(), total_palabras = integer(), 
                                  palabras_unicas = integer(), stringsAsFactors = FALSE)
        
        for (gen in unique(data$gender)) {
          gender_text <- data$preprocessed_text[data$gender == gen]
          if (length(gender_text) == 0) next
          
          gender_tokens <- unlist(strsplit(paste(gender_text, collapse = " "), "\\s+"))
          gender_tokens <- gender_tokens[gender_tokens != ""]
          
          # Apply filters
          if (input$exclude_stopwords) {
            gender_tokens <- gender_tokens[!gender_tokens %in% stopwords]
          }
          gender_tokens <- gender_tokens[nchar(gender_tokens) >= input$min_chars]
          
          if (length(gender_tokens) == 0) {
            cat("Género", gen, ": Sin palabras que cumplan los criterios\n")
            next
          }
          
          # Find top word
          gender_word_freq <- table(gender_tokens)
          top_word <- names(gender_word_freq)[which.max(gender_word_freq)]
          top_freq <- max(gender_word_freq)
          
          # Add to data frame
          gender_data <- rbind(gender_data, data.frame(
            genero = gen,
            palabra_top = top_word,
            frecuencia = top_freq,
            total_palabras = length(gender_tokens),
            palabras_unicas = length(unique(gender_tokens)),
            stringsAsFactors = FALSE
          ))
          
          cat("Género", gen, "- Palabra más común:", top_word, "(", top_freq, "veces) -",
              "Total palabras:", length(gender_tokens), "-",
              "Palabras únicas:", length(unique(gender_tokens)), "\n")
        }
        
        # Age Group Analysis
        cat("\nAnálisis por Grupo de Edad:\n")
        age_data <- data.frame(edad = character(), palabra_top = character(), 
                                frecuencia = integer(), total_palabras = integer(), 
                                palabras_unicas = integer(), stringsAsFactors = FALSE)
        
        for (age in unique(data$age_group)) {
          age_text <- data$preprocessed_text[data$age_group == age]
          if (length(age_text) == 0) next
          
          age_tokens <- unlist(strsplit(paste(age_text, collapse = " "), "\\s+"))
          age_tokens <- age_tokens[age_tokens != ""]
          
          # Apply filters
          if (input$exclude_stopwords) {
            age_tokens <- age_tokens[!age_tokens %in% stopwords]
          }
          age_tokens <- age_tokens[nchar(age_tokens) >= input$min_chars]
          
          if (length(age_tokens) == 0) {
            cat("Grupo de Edad", age, ": Sin palabras que cumplan los criterios\n")
            next
          }
          
          # Find top word
          age_word_freq <- table(age_tokens)
          top_word <- names(age_word_freq)[which.max(age_word_freq)]
          top_freq <- max(age_word_freq)
          
          # Add to data frame
          age_data <- rbind(age_data, data.frame(
            edad = age,
            palabra_top = top_word,
            frecuencia = top_freq,
            total_palabras = length(age_tokens),
            palabras_unicas = length(unique(age_tokens)),
            stringsAsFactors = FALSE
          ))
          
          cat("Grupo de Edad", age, "- Palabra más común:", top_word, "(", top_freq, "veces) -",
              "Total palabras:", length(age_tokens), "-",
              "Palabras únicas:", length(unique(age_tokens)), "\n")
        }
        
      }, error = function(e) {
        cat("Error al generar estadísticas:", e$message)
      })
    })
    
    # Generate summary tables for download
    summary_tables <- reactive({
      data <- filtered_data()
      
      # Get word frequency data with filters applied
      word_freq <- attr(data, "word_freq")
      filtered_word_freq <- word_freq
      
      if (input$exclude_stopwords) {
        stopwords <- get_spanish_stopwords()
        filtered_word_freq <- filtered_word_freq[!filtered_word_freq$word %in% stopwords, ]
      }
      
      filtered_word_freq <- filtered_word_freq[nchar(filtered_word_freq$word) >= input$min_chars, ]
      filtered_word_freq <- filtered_word_freq[order(-filtered_word_freq$freq), ]
      
      # Calculate response length statistics
      response_lengths <- sapply(strsplit(data$preprocessed_text, "\\s+"), length)
      length_stats <- data.frame(
        Statistic = c("Promedio", "Mediana", "Mínimo", "Máximo", "Desviación Estándar"),
        Value = c(
          round(mean(response_lengths), 2),
          median(response_lengths),
          min(response_lengths),
          max(response_lengths),
          round(sd(response_lengths), 2)
        )
      )
      
      # Overall statistics
      overall_stats <- data.frame(
        Statistic = c("Total de respuestas", "Respuestas válidas", "Datos faltantes", 
                      "Total de palabras", "Palabras únicas"),
        Value = c(
          attr(data, "total_responses"),
          nrow(data),
          attr(data, "missing_count"),
          length(attr(data, "tokens")),
          nrow(word_freq)
        )
      )
      
      # District Analysis
      district_data <- data.frame(distrito = character(), palabra_top = character(), 
                                 frecuencia = integer(), total_palabras = integer(), 
                                 palabras_unicas = integer(), num_respuestas = integer(),
                                 promedio_longitud = numeric(),
                                 stringsAsFactors = FALSE)
      
      for (dist in unique(data$district)) {
        district_text <- data$preprocessed_text[data$district == dist]
        if (length(district_text) == 0) next
        
        # Calculate average response length
        district_resp_lengths <- sapply(strsplit(district_text, "\\s+"), length)
        avg_length <- mean(district_resp_lengths)
        
        district_tokens <- unlist(strsplit(paste(district_text, collapse = " "), "\\s+"))
        district_tokens <- district_tokens[district_tokens != ""]
        
        # Apply filters
        if (input$exclude_stopwords) {
          stopwords <- get_spanish_stopwords()
          district_tokens <- district_tokens[!district_tokens %in% stopwords]
        }
        district_tokens <- district_tokens[nchar(district_tokens) >= input$min_chars]
        
        if (length(district_tokens) == 0) next
        
        # Find top word
        district_word_freq <- table(district_tokens)
        top_word <- names(district_word_freq)[which.max(district_word_freq)]
        top_freq <- max(district_word_freq)
        
        # Add to data frame
        district_data <- rbind(district_data, data.frame(
          distrito = dist,
          palabra_top = top_word,
          frecuencia = top_freq,
          total_palabras = length(district_tokens),
          palabras_unicas = length(unique(district_tokens)),
          num_respuestas = length(district_text),
          promedio_longitud = round(avg_length, 2),
          stringsAsFactors = FALSE
        ))
      }
      
      # Gender Analysis - Similar to district but for gender
      gender_data <- data.frame(genero = character(), palabra_top = character(), 
                               frecuencia = integer(), total_palabras = integer(), 
                               palabras_unicas = integer(), num_respuestas = integer(),
                               promedio_longitud = numeric(),
                               stringsAsFactors = FALSE)
      
      for (gen in unique(data$gender)) {
        gender_text <- data$preprocessed_text[data$gender == gen]
        if (length(gender_text) == 0) next
        
        # Calculate average response length
        gender_resp_lengths <- sapply(strsplit(gender_text, "\\s+"), length)
        avg_length <- mean(gender_resp_lengths)
        
        gender_tokens <- unlist(strsplit(paste(gender_text, collapse = " "), "\\s+"))
        gender_tokens <- gender_tokens[gender_tokens != ""]
        
        # Apply filters
        if (input$exclude_stopwords) {
          stopwords <- get_spanish_stopwords()
          gender_tokens <- gender_tokens[!gender_tokens %in% stopwords]
        }
        gender_tokens <- gender_tokens[nchar(gender_tokens) >= input$min_chars]
        
        if (length(gender_tokens) == 0) next
        
        # Find top word
        gender_word_freq <- table(gender_tokens)
        top_word <- names(gender_word_freq)[which.max(gender_word_freq)]
        top_freq <- max(gender_word_freq)
        
        # Add to data frame
        gender_data <- rbind(gender_data, data.frame(
          genero = gen,
          palabra_top = top_word,
          frecuencia = top_freq,
          total_palabras = length(gender_tokens),
          palabras_unicas = length(unique(gender_tokens)),
          num_respuestas = length(gender_text),
          promedio_longitud = round(avg_length, 2),
          stringsAsFactors = FALSE
        ))
      }
      
      # Age Group Analysis - Similar to district but for age groups
      age_data <- data.frame(edad = character(), palabra_top = character(), 
                            frecuencia = integer(), total_palabras = integer(), 
                            palabras_unicas = integer(), num_respuestas = integer(),
                            promedio_longitud = numeric(),
                            stringsAsFactors = FALSE)
      
      for (age in unique(data$age_group)) {
        age_text <- data$preprocessed_text[data$age_group == age]
        if (length(age_text) == 0) next
        
        # Calculate average response length
        age_resp_lengths <- sapply(strsplit(age_text, "\\s+"), length)
        avg_length <- mean(age_resp_lengths)
        
        age_tokens <- unlist(strsplit(paste(age_text, collapse = " "), "\\s+"))
        age_tokens <- age_tokens[age_tokens != ""]
        
        # Apply filters
        if (input$exclude_stopwords) {
          stopwords <- get_spanish_stopwords()
          age_tokens <- age_tokens[!age_tokens %in% stopwords]
        }
        age_tokens <- age_tokens[nchar(age_tokens) >= input$min_chars]
        
        if (length(age_tokens) == 0) next
        
        # Find top word
        age_word_freq <- table(age_tokens)
        top_word <- names(age_word_freq)[which.max(age_word_freq)]
        top_freq <- max(age_word_freq)
        
        # Add to data frame
        age_data <- rbind(age_data, data.frame(
          edad = age,
          palabra_top = top_word,
          frecuencia = top_freq,
          total_palabras = length(age_tokens),
          palabras_unicas = length(unique(age_tokens)),
          num_respuestas = length(age_text),
          promedio_longitud = round(avg_length, 2),
          stringsAsFactors = FALSE
        ))
      }
      
      return(list(
        overall = overall_stats,
        length_stats = length_stats,
        word_freq = filtered_word_freq,
        district = district_data,
        gender = gender_data,
        age = age_data
      ))
    })
    
    # Word frequency bar chart
    output$word_freq_plot <- renderPlotly({
      req(filtered_data())
      create_word_freq_bars(
        filtered_data(),
        max_words = input$max_words_freq,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars,
        custom_theme = active_theme()  # Pass the active theme
      )
    })

    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("resumen_nominal_", selected_question(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        summaries <- summary_tables()
        
        # Create temporary directory for files
        temp_dir <- tempdir()
        
        # Write each summary to a CSV file
        write.csv(summaries$overall, file.path(temp_dir, "estadisticas_generales.csv"), row.names = FALSE)
        write.csv(summaries$length_stats, file.path(temp_dir, "estadisticas_longitud.csv"), row.names = FALSE)
        write.csv(summaries$word_freq, file.path(temp_dir, "tabla_frecuencias.csv"), row.names = FALSE)
        write.csv(summaries$district, file.path(temp_dir, "analisis_por_distrito.csv"), row.names = FALSE)
        write.csv(summaries$gender, file.path(temp_dir, "analisis_por_genero.csv"), row.names = FALSE)
        write.csv(summaries$age, file.path(temp_dir, "analisis_por_edad.csv"), row.names = FALSE)
        
        # Save current working directory
        oldwd <- getwd()
        
        # Change to temp directory before zipping
        setwd(temp_dir)
        
        # Create zip file with just filenames (not full paths)
        files_to_zip <- c(
          "estadisticas_generales.csv",
          "estadisticas_longitud.csv",
          "tabla_frecuencias.csv",
          "analisis_por_distrito.csv",
          "analisis_por_genero.csv",
          "analisis_por_edad.csv"
        )
        
        zip(file, files_to_zip)
        
        # Change back to original working directory
        setwd(oldwd)
      }
    )
    
    # Excel download handler
    output$download_summary_excel <- downloadHandler(
      filename = function() {
        paste0("resumen_nominal_", selected_question(), "_", Sys.Date(), ".xlsx")
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
        
        openxlsx::addWorksheet(wb, "Estadísticas de Longitud")
        openxlsx::writeData(wb, "Estadísticas de Longitud", summaries$length_stats)
        
        openxlsx::addWorksheet(wb, "Frecuencia de Palabras")
        # Limit to top 1000 rows to avoid Excel limitations
        openxlsx::writeData(wb, "Frecuencia de Palabras", head(summaries$word_freq, 1000))
        
        openxlsx::addWorksheet(wb, "Análisis por Distrito")
        openxlsx::writeData(wb, "Análisis por Distrito", summaries$district)
        
        openxlsx::addWorksheet(wb, "Análisis por Género")
        openxlsx::writeData(wb, "Análisis por Género", summaries$gender)
        
        openxlsx::addWorksheet(wb, "Análisis por Edad")
        openxlsx::writeData(wb, "Análisis por Edad", summaries$age)
        
        # Save workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

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
        # Get theme colors if available
        colors <- if (!is.null(active_theme()) && !is.null(active_theme()$palettes$sequential)) {
          "random-dark"  # Default
        } else {
          "random-dark"  # wordcloud2 doesn't easily support custom colors, stick with defaults
        }
        
        # Use the simpler version without elementId
        html <- wordcloud2::wordcloud2(data = word_freq, size = 0.5, color = colors)
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
        min_chars = input$min_chars,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    # Word treemap
    output$word_treemap_plot <- renderPlotly({
      req(filtered_data())
      create_word_treemap(
        filtered_data(),
        max_words = input$max_words_treemap,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
    
    # Bigram network
    output$bigram_network_plot <- renderPlotly({
      req(filtered_data())
      create_bigram_network(
        filtered_data(),
        max_bigrams = input$max_bigrams,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars,
        custom_theme = active_theme()  # Pass the active theme
      )
    })
  })
}