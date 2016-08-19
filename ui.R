
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
        tabName = "trending",
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "Análisis de Sentimiento",
        icon = icon("bar-chart"),
        tabName = "analisis"
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
        label = "Seleccionar estado, comunidad o provincia",
        choices = c("Spain")
      ),
      em("*Nombre del país para una búsqueda del país completo")
      
    ),
    conditionalPanel(
      condition = "input.menu1 == 'analisis'",
      hr(),
      strong("Análisis de sentimiento"),
      selectInput(inputId = "tendenciaSelect", label = "Seleccionar un Trending Topic", choices = as.list(trends$Tendencia)),
      textInput(inputId = "tendenciaText", label = "o introducir manualmente"),
      em("*Dejar en blanco para usar Trending Topic"),
      sliderInput(inputId = "nSlider",
                  label = "Número de Tweets a recolectar",
                  min = 10, max = 200, value = 100, step = 1
      ),
      dateRangeInput(inputId = "fechaRange", label = "Rango de fecha de publicación de los Tweets a analizar", start = "2010-01-01", end = Sys.Date(), max = Sys.Date(), language = "es"),
      checkboxGroupInput(inputId = "idiomasCheck", label = "Idiomas válidos para los Tweets", choices = c("es", "en"))

    )
  ),
  ##########################################################
  
  dashboardBody(tabItems(
    ###################### INICIO ###########################
    tabItem(
      tabName = "inicio",
      fluidRow(
        valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
        
        valueBox(
          10 * 2,
          "New Orders",
          icon = icon("credit-card"),
          color = "yellow"
        ),
        
        valueBox(
          10 * 2,
          "New Orders",
          icon = icon("credit-card"),
          color = "light-blue"
        )
      ),
      
      fluidRow(
        box(
          title = "Andotter",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h4(
            align = "center",
            "Andotter es parte del Trabajo Fin de Grado de José Luis Navarro Motos, estudiante del Grado en Ingeniería Informática en la Universidad de Almería"
          ),
          h4(
            align = "center",
            "Con el desarrollo de este proyecto se pretende obtener una herramienta que nos permita analizar el sentimiento (positivo, negativo o neutro) de los usuarios de Twitter ante una determinada tendencia o temática, todo esto desde un análisis temporal y geográfico."
          ),
          img(align = "center", src = "escudo-Ual.gif")
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
                leafletOutput("mapOutput")
                
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
                tableOutput("temporalTable")
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
            )
    ##########################################################
  ))
)