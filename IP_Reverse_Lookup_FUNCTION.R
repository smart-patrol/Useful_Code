#############################################################################
## Theis program will take the list of IPs down and run them throuh the API , and then submit out with
## a clean csv
#################################################################################
library("RCurl")
library(plyr)
library(xtable)

##############################################

get.ip <- function (file, n , outfile)

{
  
file <- "  . csv"   #file name for readin
n <-10    # number of records
outfile <- " .html" # outputs as html 

iplist<- read.table( file , header=F, quote="\"")

#function

geo_ip <- function(ip)
{
  site <-  "http://ipinfo.io/"
  html <- getURLContent(paste(site,ip, sep=""))
}

out= NULL

for (i in 1:n) {
  out[i] = geo_ip(iplist[i]) 
}

# clean up the hmtl and xml tags

x<- gsub(pattern='-?\n  "ip": -?',replacement="",x=out)
x<- gsub(pattern='-? \"hostname\":-?',replacement="",x=x)
x<- gsub(pattern='-?\"No Hostname\":-?',replacement="",x=x)
x<- gsub(pattern='-? \"city\":-?',replacement="",x=x)
x<- gsub(pattern='-? \"region\":-?',replacement="",x=x)
x<- gsub(pattern='-? \"country\":-?',replacement="",x=x)
x<- gsub(pattern='-?  \"loc\": -?',replacement="",x=x)
x<- gsub(pattern='-?    \"org\": \ -?',replacement="",x=x)
x<- gsub(pattern='-?   "org": -?',replacement="",x=x)
x<- gsub(pattern="-?\n}-?",replacement="",x=x)

df <- as.data.frame(x)

df2 <- data.frame(do.call('rbind', strsplit(as.character(df$x),',\n',fixed=TRUE)))

### rename columns ##### 
df3 <- rename(df2, c("X1"="IP","X2"="Hostname", "X3"="City", "X4"="Region", "X5"="Country", "X6"="LatLong", "X7"="Org", "X8"="IP2"))

df.out<- xtable(df3)

print.xtable(df,out, type="html", file=outfile)

}