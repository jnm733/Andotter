library(stringr)
Locs <- availableTrendLocations()
trends = c()
#Obtención de trending topics
getTrendingTopics = function(){
#return data frame with name, country & woeid.
List = unique(as.list(Locs$country))
# Where woeid is a numerical identification code describing a location ID
return(List)
}

getTrendingTopicsState = function(countryIn){
  #Locs <- availableTrendLocations()
  LocsCountry = subset(Locs, country == countryIn)
  List = unique(as.list(LocsCountry$name))
  return(List)
}

selectTrendingTopics = function(countryIn, nameIn){
# Filter the data frame for Delhi (India) and extract the woeid of the same
LocsCountry = subset(Locs, country == countryIn)
LocsName = subset(LocsCountry, name == nameIn)$woeid

# getTrends takes a specified woeid and returns the trending topics associated with that woeid
trends = getTrends(woeid=LocsName)
trends = subset(trends, select = c(name, url))
colnames(trends) <- c("Tendencia", "URL")

return(trends)
}

