source("Code/LimpiezaTweets.R", encoding="utf-8")

getTweets = function(tendencia, n, lang, countryInput){
  
  if(countryInput == "Todas"){
  AnalisisTweetsSpanish = searchTwitter(tendencia, n, lang = lang, resultType = "recent")
  AnalisisTweetsSpanish
  }else{
    paises = read.csv("Data/paises.csv")
    colnames(paises) <- c("iso3166", "latitude", "longitude", "country")
    paises = subset(paises, country == countryInput)
    geocode = paises$latitude[1]
    geocode = paste(geocode, paises$longitude[1], sep=',')
    geocode = paste(geocode, "500km", sep=", ")

    AnalisisTweetsSpanish = searchTwitter(tendencia, n, lang = lang, resultType = "recent", geocode = geocode)
    AnalisisTweetsSpanish
  }
  
  #AnalisisTweetsSpanish = strip_retweets(AnalisisTweetsSpanish, strip_manual = TRUE, strip_mt = TRUE)
  
  TextTweetsSpanish <- sapply(AnalisisTweetsSpanish, function(x) x$getText())
  TextTweetsSpanish
  TextTweetsSpanish = iconv(TextTweetsSpanish, "latin1", "UTF-8", sub="")
  AnalisisTweetsLimpioSpanish = cleanTweetsAndRemoveNAs(TextTweetsSpanish)
  return(AnalisisTweetsLimpioSpanish)
}

analisisSentimientoSpanishA = function(AnalisisTweetsLimpioSpanish){

#Corpus español
opinion.lexicon.spanish.pos = scan('Data/opinion-lexicon-Spanish/positive-words.txt', what='character', comment.char=';')
opinion.lexicon.spanish.neg = scan('Data/opinion-lexicon-Spanish/negative-words.txt', what='character', comment.char=';')


#Score español
AnalisisTweetsScoreSpanish = getSentimentScore(AnalisisTweetsLimpioSpanish, opinion.lexicon.spanish.pos , opinion.lexicon.spanish.neg)
AnalisisTweetsScoreSpanish


#Medidas español
#AnalisisTweetsScoreSpanish = subset(AnalisisTweetsScoreSpanish, !is.na(score))
#mediaSpanish = median(AnalisisTweetsScoreSpanish$score)
#desvTipicaSpanish = sqrt(var(AnalisisTweetsScoreSpanish$score))


return (AnalisisTweetsScoreSpanish)
}

getSentimentScore = function(sentences, words.positive, words.negative, .progress='none')
{
  library(plyr)
  library(stringr)
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, words.positive, words.negative) {
    
    # Let first remove the Digit, Punctuation character and Control characters:
    sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
    
    # Then lets convert all to lower sentence case:
    sentence = tolower(sentence)
    
    
    # Now lets split each sentence by the space delimiter
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    # Get the boolean match of each words with the positive & negative opinion-lexicon
    pos.matches = match(words, words.positive)
    neg.matches = match(words, words.negative)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # Comprobamos si el algoritmo ha logrado clasificar
    if (sum(pos.matches) == 0 && sum(neg.matches) == 0){
      return (NA);
    }else{
      # Now get the score as total positive sentiment minus the total negatives
      score = sum(pos.matches) - sum(neg.matches)
    }
    
    return(score)
  }, words.positive, words.negative, .progress=.progress )
  
  # Return a data frame with respective sentence and the score
  scores.df = data.frame(Tweet_procesado=sentences, score=scores)
  return(scores.df)
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

  while(cont < 7){
    
    AnalisisTweetsSpanish = searchTwitter(tendencia, 100, lang = "es", since=since, until=until, resultType = "recent")
    #AnalisisTweetsSpanish = strip_retweets(AnalisisTweetsSpanish, strip_manual = TRUE, strip_mt = TRUE)
    TextTweetsSpanish <- sapply(AnalisisTweetsSpanish, function(x) x$getText())
    TextTweetsSpanish = iconv(TextTweetsSpanish, "latin1", "ASCII", "byte")
    AnalisisTweetsLimpioSpanish = cleanTweetsAndRemoveNAs(TextTweetsSpanish)
    
    opinion.lexicon.spanish.pos = scan('Data/opinion-lexicon-Spanish/positive-words.txt', what='character', comment.char=';')
    opinion.lexicon.spanish.neg = scan('Data/opinion-lexicon-Spanish/negative-words.txt', what='character', comment.char=';')
    
    AnalisisTweetsScoreSpanish = getSentimentScore(AnalisisTweetsLimpioSpanish, opinion.lexicon.spanish.pos , opinion.lexicon.spanish.neg)
    AnalisisTweetsScoreSpanish = subset(AnalisisTweetsScoreSpanish, !is.na(score))
    media = median(AnalisisTweetsScoreSpanish$score)
    if(is.na(media)){
      media = 0
    }
    dias = paste0(dias, paste0(diaActual-cont, ";"))
    score = paste0(score, paste0(media, ";"))
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
  score = unlist(strsplit(score, ";"))
  dias = unlist(strsplit(dias, ";"))
  df = data.frame(dias=dias, score=score)
  return(df)
  }
analisisSentimientoInglesA = function(AnalisisTweetsLimpioEnglish){
  
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