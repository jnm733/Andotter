#Carga de librerias
#install.packages(c("devtools", "rjson", "bit64", "httr"))
library(devtools)
#install.packages("twitteR")
library(twitteR)


#Credenciales Twitter
api_key<- "PyRGBHE4ZLk65LqQXMwhevAP1"
api_secret<- "dxZoDlqNVGv59jgsApFfSCPeFOllf91KgoFiIGylcwl6V65gSG"
access_token<- "514332205-NASGrkpioaIcz8QeqQz9Z1r6ni3sud6YhzUTCDq5"
access_token_secret<- "xSyAbnINaC1aGOWoQ5NBn4Ncxa9T79VlR4wz2anFWp9A7"

#Conexión OAuth
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

