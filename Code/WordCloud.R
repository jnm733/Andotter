
library(graphics)
library(tm)
#install.packages("wordcloud")
library(wordcloud)


getWordCloud = function(tendencia, n){

#Obtención de Tweets español
AnalisisTweetsSpanish = searchTwitter(tendencia, n=n, lang = "es", resultType = "recent")

#Limpieza de Tweets español
TextTweetsSpanish <- sapply(AnalisisTweetsSpanish, function(x) x$getText())
AnalisisTweetsLimpioSpanish = cleanTweetsAndRemoveNAs(TextTweetsSpanish)



corpus = Corpus(VectorSource(AnalisisTweetsLimpioSpanish))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, c(stopwords("spanish"), tendencia))

tdm <- TermDocumentMatrix(corpus)
m = as.matrix(tdm)
wf <- sort(rowSums(m),decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

return(wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")))


}
