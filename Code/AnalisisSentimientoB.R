#install.packages("Rstem", repos="http://cran.rstudio.com/", dependencies =TRUE)
#install.packages("Rstem", repos = "http://www.omegahat.org/R")
#devtools::install_github("cran/Rstem")
#install.packages("bin/Rstem_0.4-1.tar.gz",repos = NULL, type="source")
require(devtools)
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz") 
require(sentiment)
ls("package:sentiment")
library(sentiment)

analisisSentimientoEnglishBEmotion = function(AnalisisTweetsLimpio){
  
#Estimando sentimiento
AnalisisTweetsResultEmotion = classify_emotion(AnalisisTweetsLimpio, algorithm = "bayes", prior=1.0)
return(AnalisisTweetsResultEmotion)
}

analisisSentimientoeEnglishBPolarity = function(AnalisisTweetsLimpio){
  
  #Estimando sentimiento
  AnalisisTweetsResultPolarity = classify_polarity(AnalisisTweetsLimpio, algorithm = "bayes", pstrong = 0.5, pweak = 1, prior = 1)
  return(AnalisisTweetsResultPolarity)
}
