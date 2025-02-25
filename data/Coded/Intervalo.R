intervalo <- page_fluid(
  titlePanel("Razón"),
  
  layout_columns(
    col_widths = 12,
    card(
      p(HTML("Las preguntas de intervalo tienen una respuesta numérica en sus preguntas, pero los números <strong>NO</strong> son representaciones precisas de la respuesta del participante")),
      h3("Ejemplos"), 
      p("En una escala del 1 al 10, Qué tan satisfecho está con la educación que recibe?"),
      p("PEJ es una asociación apartidista y ciudadana (escala de acuerdo/desacuerdo)"),
      h4("Posibles Filtros:"),
      p(HTML("<strong>Distrito</strong>, <strong>Grupo de Edad</strong>, <strong>Género</strong>")),
      h4("Posibles Gráficos:"),
      p(HTML("<strong>Barra/Columna</strong>, <strong>Mapa de Distrito/Chloropleth</strong>,<strong>Mancuerna</strong>" ))
    ),
    card(
      card_header('Resumen descriptivo'),
      card_body(
        layout_sidebar(
          sidebar = sidebar(
            checkboxGroupInput("districtFilterI1", "Filtrar por Distrito:",
                              choices = NULL),
            checkboxGroupInput("genderFilterI1", "Filtrar por Género:",
                             choices = NULL),
            checkboxGroupInput("ageFilterI1", "Filtrar por Edad:",
                            choices = NULL)
          ),
          verbatimTextOutput('summary_stats_4')
        )
      )
    ),
    card(
      card_header('Gráfica de Columnas'),
      card_body(
        layout_sidebar(
         sidebar = sidebar(
           checkboxGroupInput("districtFilterI", "Filtrar por Distrito:",
                             choices = NULL),
           checkboxGroupInput("genderFilterI", "Filtrar por Género:",
                            choices = NULL),
           checkboxGroupInput("ageFilterI", "Filtrar por Edad:",
                           choices = NULL)
         ),
         plotlyOutput('interbar')
       )
    )
   ),
   card(
    card_header('Gráfica de Columnas'),
    card_body(
      layout_sidebar(
       sidebar = sidebar(
         checkboxGroupInput("districtFilterI2", "Filtrar por Distrito:",
                           choices = NULL),
         checkboxGroupInput("genderFilterI2", "Filtrar por Género:",
                          choices = NULL),
         checkboxGroupInput("ageFilterI2", "Filtrar por Edad:",
                         choices = NULL)
       ),
       plotlyOutput('intercol')
     )
  )
 ),
   card(
    card_header('Gráfica de mancuerna: Género'),
    card_body(
       plotlyOutput('intergenero')

  )
 ),
 card(
  card_header('Gráfica de Barras agrupadad: Edad'),
  card_body(
     plotlyOutput('interedad')

)
),
 card(
  card_header('Mapa de la ciudad'),
  card_body(
      layout_sidebar(
          sidebar = sidebar(
              checkboxGroupInput("map_genderI", "Filtrar por Género:",
                               choices = NULL),
              checkboxGroupInput("map_ageI", "Filtrar por Edad:",
                               choices = NULL)
          ),
          leafletOutput('intermap') 
      )
  )
)
  )
)