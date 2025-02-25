razon <- page_fluid(
  titlePanel("Razón"),
  
  layout_columns(
    col_widths = 12,
    card(
      p("Las preguntas de razón tienen una respuesta numérica en sus preguntas, los números son representaciones precisas de la respuesta del participante dejando poco a interpretación"),
      h3("Ejemplos"), 
      p("¿Cuántos vehículos de motor hay disponibles en su hogar?"),
      p("¿Cuántas bicicletas para niños hay disponibles en su hogar?"),
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
            checkboxGroupInput("districtFilter1", "Filtrar por Distrito:",
                              choices = NULL),
            checkboxGroupInput("genderFilter1", "Filtrar por Género:",
                             choices = NULL),
            checkboxGroupInput("ageFilter1", "Filtrar por Edad:",
                            choices = NULL)
          ),
          verbatimTextOutput('summary_stats_66')
        )
      )
    ),
    card(
      card_header('Gráfica de Columnas'),
      card_body(
        layout_sidebar(
         sidebar = sidebar(
           checkboxGroupInput("districtFilter", "Filtrar por Distrito:",
                             choices = NULL),
           checkboxGroupInput("genderFilter", "Filtrar por Género:",
                            choices = NULL),
           checkboxGroupInput("ageFilter", "Filtrar por Edad:",
                           choices = NULL)
         ),
         plotlyOutput('razonbar')
       )
    )
   ),
   card(
    card_header('Gráfica de Columnas'),
    card_body(
      layout_sidebar(
       sidebar = sidebar(
         checkboxGroupInput("districtFilter2", "Filtrar por Distrito:",
                           choices = NULL),
         checkboxGroupInput("genderFilter2", "Filtrar por Género:",
                          choices = NULL),
         checkboxGroupInput("ageFilter2", "Filtrar por Edad:",
                         choices = NULL)
       ),
       plotlyOutput('razoncol')
     )
  )
 ),
   card(
    card_header('Gráfica de mancuerna: Género'),
    card_body(
       plotlyOutput('razongenero')

  )
 ),
 card(
  card_header('Gráfica de Barras agrupadad: Edad'),
  card_body(
     plotlyOutput('razonedad')

)
),
 card(
  card_header('Mapa de la ciudad'),
  card_body(
      layout_sidebar(
          sidebar = sidebar(
              checkboxGroupInput("map_gender", "Filtrar por Género:",
                               choices = NULL),
              checkboxGroupInput("map_age", "Filtrar por Edad:",
                               choices = NULL)
          ),
          leafletOutput('razonmap') 
      )
  )
)
  )
)