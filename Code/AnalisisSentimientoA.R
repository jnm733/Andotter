
analisisSentimientoSpanishA = function(tendencia, n){
#Obtención de Tweets español
AnalisisTweetsSpanish = searchTwitter(tendencia, n, lang = "es", resultType = "recent")
AnalisisTweetsSpanish


#Limpieza de Tweets español
TextTweetsSpanish <- sapply(AnalisisTweetsSpanish, function(x) x$getText())
TextTweetsSpanish
AnalisisTweetsLimpioSpanish = cleanTweetsAndRemoveNAs(TextTweetsSpanish)
AnalisisTweetsLimpioSpanish



#Corpus español
opinion.lexicon.spanish.pos = scan('Data/opinion-lexicon-Spanish/positive-words.txt', what='character', comment.char=';')
opinion.lexicon.spanish.neg = scan('Data/opinion-lexicon-Spanish/negative-words.txt', what='character', comment.char=';')


#Score español
AnalisisTweetsScoreSpanish = getSentimentScore(AnalisisTweetsLimpioSpanish, opinion.lexicon.spanish.pos , opinion.lexicon.spanish.neg)
AnalisisTweetsScoreSpanish


#Medidas español
AnalisisTweetsScoreSpanish = subset(AnalisisTweetsScoreSpanish, score != 0)
mediaSpanish = median(AnalisisTweetsScoreSpanish$score)
desvTipicaSpanish = sqrt(var(AnalisisTweetsScoreSpanish$score))


return (AnalisisTweetsScoreSpanish)
}

analisisTemporal = function(tendencia){
  
  fechaActual = as.POSIXct(Sys.Date())
  mesActual = format(fechaActual, "%m")
  anoActual = format(fechaActual, "%Y")
  diaActual = format(fechaActual, "%d")
  diaActual = as.numeric(diaActual)
  
  since = paste0(anoActual, "-")
  since = paste0(since, mesActual)
  since = paste0(since, "-")
  since = paste0(since, diaActual-1)
  
  until = paste0(anoActual, "-")
  until = paste0(until, mesActual)
  until = paste0(until, "-")
  until = paste0(until, diaActual)
  cont = 0
  
  dias = ""
  score = ""

  while(cont < 9){
    
    AnalisisTweetsSpanish = searchTwitter(tendencia, 100, lang = "es", since=since, until=until, resultType = "recent")
    TextTweetsSpanish <- sapply(AnalisisTweetsSpanish, function(x) x$getText())
    AnalisisTweetsLimpioSpanish = cleanTweetsAndRemoveNAs(TextTweetsSpanish)
    
    opinion.lexicon.spanish.pos = scan('Data/opinion-lexicon-Spanish/positive-words.txt', what='character', comment.char=';')
    opinion.lexicon.spanish.neg = scan('Data/opinion-lexicon-Spanish/negative-words.txt', what='character', comment.char=';')
    
    AnalisisTweetsScoreSpanish = getSentimentScore(AnalisisTweetsLimpioSpanish, opinion.lexicon.spanish.pos , opinion.lexicon.spanish.neg)
    AnalisisTweetsScoreSpanish = subset(AnalisisTweetsScoreSpanish, score != 0)
    media = median(AnalisisTweetsScoreSpanish$score)    
    dias = paste0(dias, paste0(diaActual-cont, "-"))
    score = paste0(score, paste0(media, "-"))
    cont = cont+1
    
    
    since = paste0(anoActual, "-")
    since = paste0(since, mesActual)
    since = paste0(since, "-")
    since = paste0(since, diaActual-(cont+1))
    
    until = paste0(anoActual, "-")
    until = paste0(until, mesActual)
    until = paste0(until, "-")
    until = paste0(until, diaActual-cont)
    
  }
  score = unlist(strsplit(score, "-"))
  dias = unlist(strsplit(dias, "-"))
  df = data.frame(dias=dias, score=score)
  return(df)
  }
analisisSentimientoInglesA = function(tendencia, n){
  
  #Obtención de Tweets inglés
  AnalisisTweetsEnglish = searchTwitter(tendencia, n, lang = "en", resultType = "recent")
  AnalisisTweetsEnglish
  
  
  #Limpieza de Tweets inglés
  TextTweetsEnglish <- sapply(AnalisisTweetsEnglish, function(x) x$getText())
  TextTweetsEnglish
  AnalisisTweetsLimpioEnglish = cleanTweetsAndRemoveNAs(TextTweetsEnglish)
  AnalisisTweetsLimpioEnglish
  
  #Corpus ingles
  opinion.lexicon.english.pos = scan('Data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
  opinion.lexicon.english.neg = scan('Data/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
  

  #Score inglés
  AnalisisTweetsScoreEnglish = getSentimentScore(AnalisisTweetsLimpioEnglish, opinion.lexicon.english.pos , opinion.lexicon.english.neg)
  AnalisisTweetsScoreEnglish
  
  
  #Medidas inglés
  mediaEnglish = median(AnalisisTweetsScoreEnglish$score)
  desvTipicaEnglish = sqrt(var(AnalisisTweetsScoreEnglish$score))

  
  return (AnalisisTweetsScoreEnglish)
}