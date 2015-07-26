#install.packages("ROAuth")

library(RCurl)
library(twitteR)
library(ROAuth)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = ""
consumerSecret = ""
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)
#The next command provides a URL which you will need to copy and paste into your favourite browser
#Assuming you are logged into Twitter you will then be provided a PIN number to type into the R command line
# must use https 
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,accessURL=accessURL,authURL=authURL)

twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# Checks that you are authorised
registerTwitterOAuth(twitCred)

library(RCurl) 

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


freecake <- searchTwitter('TOPIC' , n=100, catinfo="cacert.pem")
freecake.df <- twListToDF (freecake)
write.csv(freecake.df , file='C:\\Users\\cfrenzel\\Documents\\out.csv' , row.names=T)

getUser("UserName")$followersCount
