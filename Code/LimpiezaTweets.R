library(stringr)

#Función para convertir en minuscula
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # Try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

#Función para limpiar el tweet
cleanTweets = function(tweet){
  
  
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet = gsub("#\\w+", " ", tweet)
  tweet = gsub("@\\w+", " ", tweet)

  tweet = gsub("â", "¿", tweet)
  tweet = gsub("Ã¡", "a", tweet)
  tweet = gsub("Ã©", "e", tweet)
  tweet = gsub("Ã³", "o", tweet)
  tweet = gsub("Ã±", "ñ", tweet)
  tweet = gsub("[[:punct:]]", " ", tweet)
  tweet = gsub("ã", "i", tweet)
  tweet = gsub("Ã ", "i", tweet)
  
  tweet = gsub("[[:digit:]]", " ", tweet)
  
  tweet = gsub("\n", " ", tweet)
  
  
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet = chartr('áéíóú','aeiou', tweet)
  

  tweet = catch.error(tweet)
  tweet

}


cleanTweetsAndRemoveNAs = function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}
