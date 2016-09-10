
library(graphics)
library(tm)
#install.packages("wordcloud")
library(wordcloud)


getWordCloud = function(AnalisisTweetsLimpioSpanish){


corpus = Corpus(VectorSource(AnalisisTweetsLimpioSpanish))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, c(stopwords("spanish")))
corpus = tm_map(corpus, removeWords, c(stopwords("english")))


tdm <- TermDocumentMatrix(corpus)
m = as.matrix(tdm)
wf <- sort(rowSums(m),decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

return(wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")))


}
