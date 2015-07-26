#http://skardhamar.github.io/rga/
#http://www.computerworld.com/s/article/9244124/How_to_extract_custom_data_from_the_Google_Analytics_API?taxonomyId=9&pageNumber=2

#install.packages("devtools")
library(devtools)

#install_github("rga", "skardhamar")
library(rga)

#install.packages("forecast")
library(forecast)
library(ggplot2)


options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
rga.open(instance="ga")

# get id 
ga$getProfiles()
id <- "33346114"

# list all avialable methods
# ga$getRefClass()

# use querys and filters in google API to get the data results you want
# not in R!

myresults <- ga$getData(33346114, start.date="2010-09-01", end.date="2010-09-30",
                        metrics = "ga:visits",
                        dimensions = "ga:source", #list my traffic by the source
                        sort = "-ga:visits",
                        start = 1, max = 10)
myresults

#lets add a lot more metrics !
myresults <- ga$getData(33346114, start.date="2010-07-01", end.date="2010-12-30",
                        metrics = "ga:visits ,ga:pageviews,ga:uniquePageviews,ga:entrances,ga:exits,ga:bounces,ga:timeOnPage",
                        dimensions = "ga:date", # listing it by the date
                        sort = "-ga:visits",
                        start = 1, max = 200, batch=T)
head(myresults)

# see overall number of visits exclude dimension and sort
myresults <- ga$getData(id, start.date="2010-09-01",
                        end.date="2010-09-30", metrics = "ga:visits")

# one metric at a time
myresultsPVsVisits <- ga$getData(id, start.date="2010-01-01", end.date="2010-12-31",
                                 metrics = "ga:visits, ga:pageviews",
                                 dimensions = "ga:month")

# using filters in the pull
# here I am pulling from facebook
myresultsGNvisits <- ga$getData(id, start.date = "2010-01-01", end.date = "2010-12-31",
                                metrics = "ga:visits",
                                filters = "ga:source=~facebook.com",
                                dimensions = "ga:month")


# visulaize it 
mydata <- ga$getData(id, start.date="2010-01-01", end.date="2010-12-31",
                     metrics = "ga:visits, ga:pageviews",
                     dimensions = "ga:month")

barplot(mydata$visits, main="Visits by month", xlab="Month",
        names.arg=mydata$month, las=1, col=rainbow(9))

# Time Seris analysis done with GA

#lets add a lot more metrics !
myresults <- ga$getData(33346114, start.date="2010-07-01", end.date="2010-12-30",
                        metrics = "ga:visits ,ga:pageviews,ga:uniquePageviews,ga:entrances,ga:exits,ga:bounces,ga:timeOnPage",
                        dimensions = "ga:date", # listing it by the date
                        sort = "-ga:visits",
                        start = 1, max = 200, batch=T)

head(myresults)

# now lets  try to do something GA cannot do 


MET <- "ga:pageviews"
DIM <- "ga:date"

stats = ga$getData(id,  start.date="2010-07-01", end.date="2010-12-30", walk = TRUE, batch = TRUE, metrics = MET, 
                   dimensions = DIM, sort = "", filters = "", segment = "")

fit = auto.arima(stats$pageviews)
plot(forecast(fit) , axes = F )

#Why should we care about forecasting? Let's say your team is about to launch a redesign of some, maybe all, 
#of the pages on your site. You might want to forecast the pageviews for your site over the next month. 
#When the data starts pouring in, you can compare the actual pageview stats to the predicted values in order to see how well 
#your new site is performing. If you are constantly beating estimates, you probably can conclude that the redesign worked!
