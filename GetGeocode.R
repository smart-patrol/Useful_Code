
#--------------------------------------------------------------------------------

#RCurl and RJSONIO are required libraries for the connection to the API 
library(RCurl)
library(RJSONIO)

APIkey <- ""


##############################
# Function: gGeoCode
# Use: sends an http request to the appropriate Google API for geocoding
# Argument(s):
#    1) vector of addresses to be geocoded (required)
#    2) Google API (maps or places) for query (optional, default=places)
#    3) verbose boolean to print each address during the geocode (optional, default=FALSE)
#    4) accuracy boolean for maps API (optional, default=FALSE); informs whether the geocode was approximate, interpolated or exact, see https://developers.google.com/maps/documentation/geocoding/#Results
#    5) partial match boolean for maps API (optional, default=FALSE); informs whether the address was completely or partially matched, see https://developers.google.com/maps/documentation/geocoding/#Results
# Return: data frame containing latitude(s), longitude(s), the formatted address(es) used by Google for the actual geocoding, plus any optional components (see arguments)
##############################
gGeoCode <- function(address,api="places",verbose=FALSE,accuracy=FALSE,partmatch=FALSE)
{
  #create empty data frame to store results
  resultDF <- NULL
  
  for (i in 1:length(address))
  {
    #print the parameters in verbose mode
    if (verbose==TRUE)
      cat("\ngGeoCode called with parameters\n-------------------------------\nRaw address:",address[i],"\nAPI: ",api,"\nVerbose:",verbose,"\nAccuracy:",accuracy,"\nPartial match:",partmatch,"\n-------------------------------\n\n")
    
    #create the geocode URL, dependent on the API
    if (tolower(api)=="places")
    {
      root <- "https://maps.googleapis.com/maps/api/place/textsearch/"
      sensor <- "false"
      u <- URLencode(paste(root, "json", "?query=", address[i], "&sensor=", sensor, "&key=", APIkey, sep = ""))
    }
    else if (tolower(api)=="maps")
    {
      root <- "http://maps.google.com/maps/api/geocode/"
      sensor <- "false"
      u <- URLencode(paste(root, "json", "?address=", address[i], "&sensor=", sensor, sep = ""))    
    }
    else
    {
      cat("\nUnrecognized API: ", api, "\nValid APIs are: maps, places\n")
      stop()
    }
    
    #connect to the geocode URL and retrieve results
    doc <- getURL(u, ssl.verifypeer = FALSE)
    
    #de-serialize the JSON object to an R object
    x <- fromJSON(doc,simplify = FALSE)
    
    #check the status of the geocode
    if(x$status=="OK")
    {
      #geocode successful, parse results
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      formatted_address <- x$results[[1]]$formatted_address
      
      #check for accuracy option for maps API
      if (tolower(api)=="maps" && accuracy==TRUE)
        loc_type <- x$results[[1]]$geometry$location_type
      else
        loc_type <- NA
      
      #check for partial match option for maps API
      if (tolower(api)=="maps" && partmatch==TRUE && !is.null(x$results[[1]]$partial_match))
        part_match <- x$results[[1]]$partial_match
      else
        part_match <- NA
      
      #return a vector containing latitude, longitude, the formatted address used by Google for the actual geocoding, plus any optional components (see arguments)
      resultDF <- rbind(resultDF,c(lat, lng, formatted_address, loc_type, part_match))
    }
    else
    {
      #geocode failed  
      resultDF <- rbind(resultDF,c(NA,NA,NA,NA,NA))
    }
    
    #sleep 1/10 of a second between requests per Google's terms of use
    Sys.sleep(.1)
  }
  
  #coerce to data frame, set column names, and return
  resultDF <- as.data.frame(resultDF)
  colnames(resultDF) <- c("Latitude","Longitude","Formatted_Address","Accuracy","Partial_Match")
  return(resultDF)
}

#--------------------------------------------------------------------------------

mykey = "dj0yJmk9dmZndlFQa3lSeUZYJmQ9WVdrOWRIQkdaMFZoTjJrbWNHbzlNQS0tJnM9Y29uc3VtZXJzZWNyZXQmeD0xNw--"
mysecret = "ed17c7ecdaa99a224cf306ed1266bfdc428e356a"  



find_place <- function(location=NULL,
                       name=NULL,
                       line1=NULL,
                       line2=NULL,
                       line3=NULL,
                       house=NULL,
                       street=NULL,
                       unittype=NULL,
                       unit=NULL,
                       xstreet=NULL,
                       postal=NULL,
                       neighborhood=NULL,
                       city=NULL,
                       county=NULL,
                       state=NULL,
                       country=NULL,
                       woeid=NULL,
                       reverse=FALSE,
                       locale="en_US",
                       flags="",
                       gflags="",
                       commercial=FALSE,
                       key=getOption("RYDN_KEY"),
                       secret=getOption("RYDN_SECRET")){
  if (sum(missing(location), missing(woeid), missing(name)) < 2){
    stop("Can only provide either location, woeid, or name.")
  }
  
  if (!missing(location) || !missing(name) || !missing(woeid)){
    if (!missing(line1) || !missing(line2) || !missing(line3)){
      stop("Cannot provide both location and line parameters.")
    }
    if(!missing(house) || !missing(street) || !missing(unittype) || 
       !missing(unit) || !missing(xstreet) || !missing(postal) || 
       !missing(neighborhood) || !missing(city) || !missing(county) ||
       !missing(state) || !missing(country)){
      stop("Cannot provide both a location string and specific fields such as 'street' or 'city'.")
    }
  }
  
  if (!missing(line1)){
    if(!missing(house) || !missing(street) || !missing(unittype) || 
       !missing(unit) || !missing(xstreet) || !missing(postal) || 
       !missing(neighborhood) || !missing(city) || !missing(county) ||
       !missing(state) || !missing(country)){
      stop("Cannot provide both a multi-line (line1, etc.) address and specific fields such as 'street' or 'city'.")
    }
  }
  
  if (is.null(key) || is.null(secret)){
    stop("key and secret must both be provided")
  } 
  
  if (reverse){
    gflags <- paste0(gflags, "R")
  }
  
  flags <- paste0(flags, "J")
  
  qp <- list(
    location = location,
    name = name,
    line1 = line1,
    line2 = line2,
    line3 = line3,
    house = house,
    street = street,
    unittype = unittype,
    unit = unit,
    xstreet = xstreet,
    postal = postal,
    level4 = neighborhood,
    level3 = city,
    level2 = county,
    level1 = state,
    level0 = country,
    woeid = woeid,
    gflags = gflags,
    flags = flags
  )
  
  if (!commercial){
    qpOpen <- qp
    qpOpen["text"] <- qp$location
    qpOpen["location"] <- NULL
    qpOpen["flags"] <- NULL
    qs <- listToYQL(qpOpen, "geo.placefinder")
    url <- "https://query.yahooapis.com/v1/public/yql"
  } else{
    qs <- listToQS(qp)
    url <- "https://yboss.yahooapis.com/geo/placefinder"
  }
  
  oa <- httr::oauth_app("YDN", key, secret)
  
  urlq <- paste0(url, "?", qs)
  
  sig <- httr::oauth_signature(urlq, app=oa)
  
  oaqstr <- ""
  for (i in 1:length(sig)) {
    val <- sig[[i]]
    oaqstr <- paste0(oaqstr, "&", names(sig)[i], "=", val)
  }
  urlqa <- paste0(urlq, oaqstr)
  
  q <- httr::GET(urlqa)
  httr::stop_for_status(q)
  
  # Create factor-less DF
  saf <- getOption("stringsAsFactors")
  options(stringsAsFactors=FALSE)
  
  if (!commercial){
    con <- httr::content(q)
    res <- con$query$results$Result
    
    if (con$query$count > 1){
      res <- transformResults(res)
      df <- do.call(rbind.data.frame, res)
    } else{
      res[sapply(res, is.null)] <- NA
      df <- as.data.frame(res)
    }
  } else{
    res <- httr::content(q)$bossresponse$placefinder$results
    res <- transformResults(res)
    df <- do.call(rbind.data.frame, res)
  }
  
  options(stringsAsFactors=saf)
  
  df
}

transformResults <- function(res){
  # Get all col names
  nms <- unique(unlist(sapply(res, names)))
  
  for (i in 1:length(res)){
    # ensure they all have the same columns
    res[[i]] <- res[[i]][nms]
    names(res[[i]]) <- nms
    
    # Transform NULLs
    res[[i]][sapply(res[[i]], is.null)] <- NA
  }
  res
}

listToQS <- function(qp){
  qs <- ""
  for (n in names(qp)){
    val <- qp[[n]]
    if (!is.null(val)){
      qs <- paste0(qs, n, "=", RCurl::curlEscape(val), "&")  
    }
  }
  
  # Trim last &
  substr(qs, 0, nchar(qs)-1)
}

listToYQL <- function(qp, table){
  qs <- paste0("SELECT * FROM ", table, " WHERE ")
  for (n in names(qp)){
    val <- qp[[n]]
    if (!is.null(val)){
      qs <- paste0(qs, n, "=", paste0('"', val, '"'), " AND ")
    }
  }
  
  # Trim last ' AND '
  qs <- substr(qs, 0, nchar(qs)-5)
  
  qs <- RCurl::curlEscape(qs)
  
  paste0("q=", qs, "&format=json")
}

#-----------------------------------------------------------------------------------

# Wrapper Function so that Both are Run

getGeo <- function(addresses) {
  
  ad_yahoo = lapply(addresses, function(x) find_place(line1 = x, key = mykey ,secret = mysecret))
  
  ad_google = lapply(addresses, function(x) gGeoCode(x, api="places", accuracy = TRUE, partmatch = TRUE))
  
  y_df <- as.data.frame(do.call(rbind, ad_yahoo))
  g_df <- as.data.frame(do.call(rbind, ad_google))
  
  names(y_df) <- paste0("yahoo", "_",names(y_df)) 
  names(g_df) <- paste0("google", "_",names(g_df))
  
  geo_out <- data.frame(y_df,g_df)
  
  out <- geo_out[ , !names(geo_out) %in% c("yahoo_quality", "yahoo_addressMatchType", "yahoo_name",
                                           "yahoo_line4", "yahoo_line3" ,"yahoo_street", "yahoo_xstreet",
                                           "yahoo_unittype", "yahoo_unit","yahoo_country","yahoo_state"
                                           ,"yahoo_uzip", "yahoo_hash", "yahoo_woeid", "yahoo_woetype",
                                           "yahoo_countycode")]
  
  out <- out[, c(6,7,11,9,14,13,12,1,2,3,4,5,8,17,15,16,18,19)]
  
  return(out)
}
