#install.packages("plyr")
library(plyr)
#install.packages("stringr", repos='http://cran.us.r-project.org')
library(stringr)


getSentimentScore = function(sentences, words.positive, words.negative, .progress='none')
{
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
    
    # Now get the score as total positive sentiment minus the total negatives
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, words.positive, words.negative, .progress=.progress )
  
  # Return a data frame with respective sentence and the score
  scores.df = data.frame(Tweet_procesado=sentences, score=scores)
  return(scores.df)
  }