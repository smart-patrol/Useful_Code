#############################################################################
## Theis program will take the list of IPs down and run them throuh the API , and then submit out with
## a clean csv
#################################################################################
library("RCurl")


iplist<- c(
  '63.247.187.186',  '81.0.129.22',  '212.60.52.134',  '85.167.41.19',	'68.197.238.121',	'155.91.64.11',	'2.150.17.237',	'2.150.17.237',	'85.167.41.19',	'86.14.132.19'
)

##############################################

IPlist <- apply(IPList1, 1, function(r) paste(names(IPList1), r, sep=":", collapse=" "))
iplist<- gsub(pattern='V1:',replacement="",x=IPlist)


#function

geo_ip <- function(ip)
{
  site <-  "http://api.ipaddresslabs.com/iplocation/v1.7/locateip?key=demo&ip="
  xml <- "&format=XML"
  html <- getURLContent(paste(site,ip, xml, sep=""))
}


n <-10    # number of records

out= NULL
 #552
for (i in 1:5525) {
  out[i] = geo_ip(iplist[i]) 
}



x<- gsub(pattern='"<?xml version=\"1.0\" encoding=\"UTF-8\"',replacement="",x=out)
x<- gsub(pattern='response',replacement="",x=x)
x<- gsub(pattern='query_status',replacement="",x=x)
x<- gsub(pattern='query_status_code',replacement="",x=x)
x<- gsub(pattern='ip_address',replacement="",x=x)
x<- gsub(pattern='geolocation_data',replacement="",x=x)
x<- gsub(pattern='continent_code',replacement="",x=x)
x<- gsub(pattern='continent_name',replacement="",x=x)
x<- gsub(pattern='country_code_iso3166alpha2US',replacement="",x=x)
x<- gsub(pattern='country_code_fips10-4',replacement="",x=x)
x<- gsub(pattern="country_name",replacement="",x=x)
x<- gsub(pattern="region_code",replacement="",x=x)
x<- gsub(pattern="region_name",replacement="",x=x)
x<- gsub(pattern="city",replacement="",x=x)
x<- gsub(pattern="postal_code",replacement="",x=x)
x<- gsub(pattern="metro_code",replacement="",x=x)
x<- gsub(pattern="area_code",replacement="",x=x)
x<- gsub(pattern="latitude",replacement="",x=x)
x<- gsub(pattern="longitude",replacement="",x=x)
x<- gsub(pattern="isp",replacement="",x=x)
x<- gsub(pattern="organization",replacement="",x=x)
x<- gsub(pattern="gelocation_data",replacement="",x=x)
x<- gsub(pattern="<",replacement="",x=x)
x<- gsub(pattern=">",replacement="",x=x)

df <- as.data.frame(x)

df2 <- data.frame(do.call('rbind', strsplit(as.character(df$x),'\n',fixed=TRUE)))

x <- xmlParse(out)


### rename columns ##### 
library(plyr)
df3 <- rename(df2, c("X1"="IP","X2"="Hostname", "X3"="City", "X4"="Region", "X5"="Country", "X6"="LatLong", "X7"="Org", "X8"="IP2"))

library(xtable)


foo<- xtable(df3)

print.xtable(foo, type="html", file="C:\\Users\\cfrenzel\\Desktop\\out\\Batch3.html")

#############################################################################



varNames <- gsub(pattern="^f",replacement="freq",x=varNames)
varNames <- gsub(pattern="-?mean[(][)]-?",replacement="Mean",x=varNames)
varNames <- gsub(pattern="-?std[()][)]-?",replacement="Std",x=varNames)
varNames <- gsub(pattern="-?meanFreq[()][)]-?",replacement="MeanFreq",x=varNames)



#use the activity names to name the activities in the set
activityLabels <- read.table(kActivityLabelsFile,stringsAsFactors=FALSE)
colnames(activityLabels) <- c("activityID","activityLabel")
