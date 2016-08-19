#install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")
require(devtools)
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz") 
require(sentiment)
ls("package:sentiment")
library(sentiment)

#Obtención y limpieza de tweets
AnalisisTweets = searchTwitter("rio 2016", n=50, lang = "en", resultType = "recent")
AnalisisTweets

AnalisisTweets <- sapply(AnalisisTweets, function(x) x$getText())
AnalisisTweets

AnalisisTweetsLimpio = cleanTweetsAndRemoveNAs(AnalisisTweets)
AnalisisTweetsLimpio


#Estimando sentimiento
AnalisisTweetsResult = classify_emotion(AnalisisTweetsLimpio, algorithm = "bayes", prior=1.0)
AnalisisTweetsResult = classify_polarity(AnalisisTweetsLimpio, algorithm = "bayes", pstrong = 0.5, pweak = 1, prior = 1)
