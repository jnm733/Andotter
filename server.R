
library(
  shiny) 


shinyServer(function(input, output, session) {
  
  observe({
    country <- input$country
    choices = getTrendingTopicsState(country)
    updateSelectInput(session, "state", choices=choices, selected = country)
  })
  
  output$trendingTable <- renderTable({
    withProgress(message = 'Por favor, espera',
                 detail = 'Cargando Trending Topics...', value = 0, {
    trends = selectTrendingTopics(input$country, input$state)
    updateSelectInput(session, "tendenciaSelect", choices=as.list(trends$Tendencia))
    
    incProgress(3/3)
    trends
    
                 })
  })
  
  output$analisisHTML <- renderUI({
    n <- input$nSlider
    inputTend <- input$tendenciaText
    withProgress(message = 'Por favor, espera',
                 detail = 'Realizando análisis...', value = 0, {
                   if(inputTend == ""){
                     inputTend = input$tendenciaSelect
                     AnalisisSentimientoASpanish = analisisSentimientoSpanishA(input$tendenciaSelect, n)
                   } 
                   if(inputTend != ""){
                     AnalisisSentimientoASpanish = analisisSentimientoSpanishA(inputTend, n)
                   }
                   incProgress(1/3)
                   histPlot(AnalisisSentimientoASpanish)
                   boxPlot(AnalisisSentimientoASpanish)
                   incProgress(1/3)
                   tableDetalle(AnalisisSentimientoASpanish)
                   incProgress(1/3)
                   
                 })
    media = median(AnalisisSentimientoASpanish$score)
    dv = sqrt(var(AnalisisSentimientoASpanish$score))
    h3(align = "center", "Análisis", inputTend ,h4(align = "center", "Media: ", media), h4(align = "center", "Desviación Típica: ", round(dv,2)), h5(align="center", em("Un resultado final menor que 0 indica un sentimiento negativo y un sentimiento positivo cuando es mayor que 0. Cuanto más próximo a 0 mas neutro será el sentimiento")), h5(align="center", "Con la desviación típica observamos la dispersión de las evaluaciones individuales respecto a la media."))
    
  })
  output$temporalTable <- renderTable({
    inputTend <- input$tendenciaText
    withProgress(message = 'Por favor, espera',
                 detail = 'Realizando análisis temporal...', value = 0, {
    if(inputTend == ""){
      inputTend = input$tendenciaSelect
      temporal = analisisTemporal(input$tendenciaSelect)
    } 
    if(inputTend != ""){
      temporal = analisisTemporal(inputTend)
    }
    incProgress(3/3)
    temporal
                 })
                   
  })
  
  tableDetalle = function(results)
  output$analisisTable <- renderTable({
    results
  })
  histPlot <- function(results){
  output$histPlot <- renderPlot({
    tendencia <- input$tendenciaSelect
    hist(results$score, main="Histograma de clasificación de Tweets", xlab="Nota", ylab="Frecuencia", border="blue",col="green", prob=TRUE)
    lines(density(results$score), col="blue", lwd=1)
    
  })
  }
  
  boxPlot <- function(results){
    output$boxPlot <- renderPlot({
      tendencia <- input$tendenciaSelect
      boxplot(results$score,main="Gráfico de caja",xlab="Nota",border="blue",col="green", horizontal = TRUE)

    })
  }
  
  
  
  output$wordCloud <- renderPlot({
    inputTend <- input$tendenciaText
    withProgress(message = 'Por favor, espera',
                 detail = 'Obteniendo el WordCloud...', value = 0, {
                   if(inputTend == ""){
                     inputTend = input$tendenciaSelect
                     wordCloud = getWordCloud(input$tendenciaSelect, input$nSlider)
                   } 
                   if(inputTend != ""){
                     wordCloud = getWordCloud(inputTend, input$nSlider)
                   }
                   incProgress(3/3)
                   wordCloud
                 })
  })
  
  output$mapOutput <- renderLeaflet({
    countryIn <- input$country
    countriesDF <- data.frame(country = unique(Locs$country))
    paises = read.csv("Data/paises.csv")
    colnames(paises) <- c("iso3166", "latitude", "longitude", "country")
    merge = merge(x=countriesDF, y=paises)
    geos = subset(merge, select = c(longitude, latitude, country))
    
    countryInput = subset(merge, country == countryIn)
    countryInput = subset(countryInput, select = c(longitude, latitude, country))
    m <- leaflet()
    m <- addTiles(m)
    m <- addMarkers(m, geos$longitude	, geos$latitude, popup=geos$country)
    m <- addCircleMarkers(m, countryInput$longitude	, countryInput$latitude, popup=countryInput$country)
    m <- setView(map = m, lng = countryInput$longitude, lat = countryInput$latitude, zoom = 3)
    m
  })
 
})