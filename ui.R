
source("Code/OAuth.R")
source("Code/TrendingTopics.R")
library(shiny)
library(shinydashboard)
library(leaflet)


dashboardPage(
  dashboardHeader(title = "Andotter"),
  dashboardSidebar(
    ###################### MENÚ ###########################
    sidebarMenu(
      id = "menu1",
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem(
        "Trending Topics",
        icon = icon("twitter"),
        tabName = "trending"
        
      ),
      hr(),
      h5("Análisis de Sentimiento"),
      menuItem(
        "Basado en el léxico",
        icon = icon("book"),
        tabName = "analisis"
      ),
      menuItem(
        "Algoritmo Naive Bayes",
        icon = icon("bar-chart"),
        tabName = "naiveBayes",
        badgeLabel = "Inglés",
        badgeColor = "green"
      )
      
    ),
    conditionalPanel(
      condition = "input.menu1 == 'trending'",
      hr(),
      h5("Búsqueda de Trending Topics"),
      selectInput(
        inputId = "country",
        label = "Seleccionar país",
        selected = "Spain",
        choices = getTrendingTopics()
      ),
      selectInput(
        inputId = "state",
        label = "Seleccionar estado o provincia",
        choices = c("Spain")
      ),
      em("*Nombre del país para una búsqueda del país completo")
      
    ),
    conditionalPanel(
      condition = "input.menu1 == 'analisis' || input.menu1 == 'naiveBayes'",
      hr(),
      strong("Análisis de sentimiento"),
      selectInput(inputId = "tendenciaSelect", label = "Seleccionar un Trending Topic", choices = as.list(trends$Tendencia)),
      textInput(inputId = "tendenciaText", label = "o introducir manualmente"),
      em("*Dejar en blanco para usar Trending Topic"),
      sliderInput(inputId = "nSlider",
                  label = "Número de Tweets a recolectar",
                  min = 10, max = 200, value = 100, step = 1
      ),
      selectInput(
        inputId = "countryAn",
        label = "Localización geográfica",
        selected = "Todas",
        choices = c("Todas",getTrendingTopics())
      ),
      selectInput(inputId = "langSelect", label = "Idioma de los Tweets", choices = c("Español", "Inglés"), selected = "Español"),
      dateRangeInput(inputId = "fechaRange", label = "Rango de fecha de publicación de los Tweets a analizar", start = "2010-01-01", end = Sys.Date(), max = Sys.Date(), language = "es")
      
    )
  ),
  ##########################################################
  
  dashboardBody(tabItems(
    ###################### INICIO ###########################
    tabItem(
      tabName = "inicio",
      fluidRow(
        
        valueBox(textOutput("limitTweet"), "Búsquedas de tweets disponibles", icon = icon("twitter"),width = 6),
        
        valueBox(
          textOutput("limitTrending"), "Búsquedas de Trending Topics disponibles", icon = icon("line-chart"), color = "yellow",width = 6)
        
        
      ),
      fluidRow(
        box(
          title = "Andotter",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h4(
            align = "center",
            "Andotter es parte del Trabajo Fin de Grado de José Luis Navarro Motos, estudiante del Grado en Ingeniería Informática de la Universidad de Almería"
          ),
          h4(
            align = "center",
            "Con el desarrollo de este proyecto se pretende obtener una herramienta que nos permita analizar el sentimiento (positivo, negativo o neutro) de los usuarios de Twitter ante una determinada tendencia o temática, todo esto desde un análisis temporal y geográfico."
          ),
          hr(),
          img(align = "center", src = "escudo-Ual.gif", height = 150, width = 400, style="display: block; margin-left: auto; margin-right: auto;")
        )
        
      )
      
    ),
    ##########################################################
    
    ###################### TRENDING TOPICS ###########################
    
    tabItem(tabName = "trending",
            fluidRow(
              box(
                title = "Trending Topics",
                width = 7,
                status = "primary",
                solidHeader = TRUE,
                column(2, tableOutput("trendingTable"))
           
              ),
              box(
                title = "Localizaciones",
                status = "info",
                solidHeader = TRUE,
                width = 5,
                leafletOutput("mapOutput"),
                textOutput("Click_text")
                
              )
            )),
    ##########################################################
    
    ###################### ANÁLISIS ###########################
    
    tabItem(tabName = "analisis",
            fluidRow(
              box(
                title = "Análisis de Sentimiento",
                width = 7,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("analisisHTML"),
                plotOutput("histPlot"),
                plotOutput("boxPlot")
                
                

              ),
              box(
                title = "WordCloud",
                width = 5,
                status = "info",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("wordCloud")
              )
            ),
            fluidRow(
              box(
                title = "Análisis Temporal",
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                plotOutput("temporalPlot")
                ),
              box(
                title = "Detalle resultado",
                width = 6,
                status = "warning",
                solidHeader = TRUE,
                collapsed = TRUE,
                tableOutput("analisisTable"),
                collapsible = TRUE
              )
            )
            ),
    ##########################################################
    ###################### NAIVE BAYES ###########################
    tabItem(
      tabName = "naiveBayes",
      fluidRow(
        box(
          title = "Análisis de Polaridad",
          width = 7,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          htmlOutput("polarityHTML"),
          plotOutput("polarityPlot")
          
          
        ),
        box(
          title = "WordCloud",
          width = 5,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("wordCloudNaive")
        )
      ),
      fluidRow(
        box(
          title = "Análisis de Emoción",
          width = 7,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          htmlOutput("emotionHTML"),
          plotOutput("emotionPlot")
        ),
        box(
          title = "Detalle resultado",
          width = 5,
          status = "warning",
          solidHeader = TRUE,
          collapsed = TRUE,
          tableOutput("detallePolarityTable"),
          collapsible = TRUE
        )
      )
      
    )
    ##########################################################
  ))
)