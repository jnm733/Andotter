library(
  shiny)
library(ggplot2)
source("Code/WordCloud.R")
source("Code/AnalisisSentimientoA.R")
source("Code/AnalisisSentimientoB.R")


shinyServer(function(input, output, session) {

  observeEvent(input$menu1, {
    limit = getCurRateLimitInfo()
    output$limitTweet <- renderText({ 
      limit$remaining[62]    }) 
    output$limitTrending <- renderText({ 
      limit$remaining[65]    }) 
  })
  
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
    hasTend = 1
    n <- input$nSlider
    inputTend <- input$tendenciaText
    lang <- input$langSelect
    countryIn <- input$countryAn
    withProgress(message = 'Por favor, espera',
                 detail = 'Realizando análisis...', value = 0, {
                   if(inputTend == "" && input$tendenciaSelect == ""){
                     hasTend = 0
                     return()
                   }
                   if(inputTend == ""){
                     if(lang == "Español"){
                     inputTend = input$tendenciaSelect
                     tweets = getTweets(input$tendenciaSelect, n, "es", countryIn)
                     AnalisisSentimientoASpanish = analisisSentimientoSpanishA(tweets)
                     }else{
                       inputTend = input$tendenciaSelect
                       tweets = getTweets(input$tendenciaSelect, n, "en", countryIn)
                       AnalisisSentimientoASpanish = analisisSentimientoInglesA(tweets)
                     }
                   }
                   else{
                     if(lang == "Español"){
                       tweets = getTweets(inputTend, n, "es", countryIn)
                     AnalisisSentimientoASpanish = analisisSentimientoSpanishA(tweets)
                     }else{
                       tweets = getTweets(inputTend, n, "en", countryIn)
                       AnalisisSentimientoASpanish = analisisSentimientoInglesA(tweets)
                     }
                   }
                   scoreDF = subset(AnalisisSentimientoASpanish, score != "NA")
    
                     incProgress(1/3)
                     wordCloud(tweets)
                     histPlot(scoreDF)
                     boxPlot(scoreDF)
                     incProgress(1/3)
                     tableDetalle(AnalisisSentimientoASpanish)
                     incProgress(1/3)
                     
                 })
    if(hasTend == 0){
      h4(align = "center", "Es necesario introducir una tendencia.", style = "color: red;", h4(align="center", "Por favor, vaya a la sección de trending topics o introduzca una de forma manual en el menú de la izquierda", style = "color: red;"))
    }else{
    media = median(scoreDF$score)
    dv = sqrt(var(scoreDF$score))
    h3(align = "center", "Análisis", inputTend ,h4(align = "center", "Media: ", media), h4(align = "center", "Desviación Típica: ", round(dv,2)), h5(align="center", em("Un resultado final menor que 0 indica un sentimiento negativo y un sentimiento positivo cuando es mayor que 0. Cuanto más próximo a 0 mas neutro será el sentimiento")), h5(align="center", "Con la desviación típica observamos la dispersión de las evaluaciones individuales respecto a la media."))
    }
      })
  
  output$polarityHTML <- renderUI({
    ingles = 1
    hasTend = 1
    n <- input$nSlider
    inputTend <- input$tendenciaText
    lang <- input$langSelect
    countryIn <- input$countryAn
    withProgress(message = 'Por favor, espera',
                 detail = 'Realizando análisis...', value = 0, {
                   if(inputTend == "" && input$tendenciaSelect == ""){
                     hasTend = 0
                     return()
                   }
                   if(inputTend == ""){
                     if(lang == "Inglés"){
                       inputTend = input$tendenciaSelect
                       tweets = getTweets(input$tendenciaSelect, n, "en", countryIn)
                       AnalisisSentimientoBEmotion = analisisSentimientoEnglishBEmotion(tweets)
                       AnalisisSentimientoBPolarity = analisisSentimientoeEnglishBPolarity(tweets)
                     }else{
                       ingles = 0
                       return()
                     }
                   }
                   else{
                     if(lang == "Inglés"){
                       tweets = getTweets(inputTend, n, "en", countryIn)
                       AnalisisSentimientoBEmotion = analisisSentimientoEnglishBEmotion(tweets)
                       AnalisisSentimientoBPolarity = analisisSentimientoeEnglishBPolarity(tweets)
                     }else{
                       ingles = 0
                       return()
                     }
                   }
                   incProgress(1/3)
                   wordCloudNaive(tweets)
                   Emotion = AnalisisSentimientoBEmotion[,7]
                   Polarity = AnalisisSentimientoBPolarity[,4]
                   Emotion[is.na(Emotion)] = "NA"
                   Emotion[Emotion == "joy"] = "Alegría"
                   Emotion[Emotion == "anger"] = "Enfado"
                   Emotion[Emotion == "disgust"] = "Disgusto"
                   Emotion[Emotion == "fear"] = "Miedo"
                   Emotion[Emotion == "sadness"] = "Tristeza"
                   Emotion[Emotion == "surprise"] = "Sorpresa"
                   
                   
                   
                   SentimentDF = data.frame(emotion=Emotion, polarity=Polarity, stringsAsFactors=FALSE)
                   fullDF = data.frame("Tweet"=tweets, SentimentDF)
                   colnames(fullDF) <- c("Tweet", "Emoción", "Polaridad")
                   
                   polarityPlot(SentimentDF)
                   emotionPlot(SentimentDF)
                   emotionHTML(SentimentDF, inputTend)
                   detallePolarityTable(fullDF)
                   incProgress(1/3)
                   incProgress(1/3)
                   
                 })
    if(hasTend == 0){
      h4(align = "center", "Es necesario introducir una tendencia.", style = "color: red;", h4(align="center", "Por favor, vaya a la sección de trending topics o introduzca una de forma manual en el menú de la izquierda", style = "color: red;"))
    }
    else if(ingles == 1){
    positivos = dim(subset(SentimentDF, polarity == "positive"))
    negativos = dim(subset(SentimentDF, polarity == "negative"))
    neutrales = dim(subset(SentimentDF, polarity == "neutral"))
    
    h3(align = "center", "Análisis de Polaridad", inputTend ,h4(align = "center", "Tweets Positivos: ", positivos), h4(align = "center", "Tweets Negativos: ", negativos), h4(align = "center", "Tweets Neutrales: ", neutrales))
    }else{
      h4(align = "center", "Sólo disponible para tweets en inglés.", style = "color: red;", h4(align="center", "Por favor, cambie el valor de idioma a inglés en el menú de la izquierda", style = "color: red;"))
    }
  })
  emotionPlot = function(results){
    output$emotionPlot <- renderPlot({
      results = subset(results, emotion != "NA")
      ggplot(results, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
        scale_fill_brewer(palette="Dark2") +
        ggtitle("Analisis de Emoción de Tweets") +
        theme(legend.position='bottom') + ylab('Número de Tweets') + xlab('Categoría de Emoción')
    })
  }
  polarityPlot = function(results){
    output$polarityPlot <- renderPlot({
      ggplot(results, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="RdGy") +
        ggtitle("Analisis de Polaridad de Tweets") +
        theme(legend.position='bottom') + ylab('Número de Tweets') + xlab('Categoria de polaridad')
    })
  }
  emotionHTML = function(results, tend){
    output$emotionHTML <- renderUI({
      subsetDim = subset(results, emotion == "Alegría")
      alegres = dim(subsetDim)
      subsetDim = subset(results, emotion == "Tristeza")
      tristes = dim(subsetDim)
      subsetDim = subset(results, emotion == "Enfado")
      enfado = dim(subsetDim)
      subsetDim = subset(results, emotion == "Disgusto")
      disgusto = dim(subsetDim)
      subsetDim = subset(results, emotion == "Sorpresa")
      sorpresa = dim(subsetDim)
      subsetDim = subset(results, emotion == "Miedo")
      miedo = dim(subsetDim)
      subsetDim = subset(results, emotion == "NA")
      na = dim(subsetDim)
      
      
      h3(align = "center", "Análisis de Emoción", tend ,h4(align = "center", "Alegria: ", alegres, "tweets"),h4(align = "center", "Tristeza: ", tristes, "tweets"), h4(align = "center", "Enfado: ", enfado, "tweets"), h4(align = "center", "Disgusto: ", disgusto, "tweets"), h4(align = "center", "Sorpresa: ", sorpresa, "tweets"), h4(align = "center", "Miedo: ", miedo, "tweets"), h4(align = "center", "No clasificados: ", na, "tweets"))
      
      })
  }
  output$temporalPlot <- renderPlot({
    inputTend <- input$tendenciaText
    withProgress(message = 'Por favor, espera',
                 detail = 'Realizando análisis temporal. Puede tardar bastante tiempo...', value = 0, {
    if(inputTend == ""){
      inputTend = input$tendenciaSelect
      temporal = analisisTemporal(input$tendenciaSelect)
    } 
    if(inputTend != ""){
      temporal = analisisTemporal(inputTend)
    }
    incProgress(3/3)
    ggplot(temporal, aes(dias, score, group=1)) +geom_line() + labs(x = "Día del mes", y = "Score", title = "Análisis temporal de los últimos 9 días")
                 })
                   
  })
  detallePolarityTable = function(results)
    output$detallePolarityTable <- renderTable({
      results
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
  
  
  wordCloud = function(tweets){

  output$wordCloud <- renderPlot({
    withProgress(message = 'Por favor, espera',
                 detail = 'Obteniendo el WordCloud...', value = 0, {
                   wordCloudResult = getWordCloud(tweets)
                   incProgress(3/3)
                   wordCloudResult
                 })
  })
  }
  wordCloudNaive = function(tweets){
    
    output$wordCloudNaive <- renderPlot({
      withProgress(message = 'Por favor, espera',
                   detail = 'Obteniendo el WordCloud...', value = 0, {
                     wordCloudResult = getWordCloud(tweets)
                     incProgress(3/3)
                     wordCloudResult
                   })
    })
  }
  
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
    m <- addMarkers(m, geos$longitude	, geos$latitude, layerId = geos$country)
    m <- addCircleMarkers(m, countryInput$longitude	, countryInput$latitude)
    m <- setView(map = m, lng = countryInput$longitude, lat = countryInput$latitude, zoom = 3)
    m
  })
  
  observe({
    click<-input$mapOutput_marker_click
    if(is.null(click))
      return()
    countryInput = (click$id)
    updateSelectInput(session, "country", selected = countryInput)
    
  })
 
})