library(RCurl)
library(jsonlite)

# column drop list
drops <- c("_id", "date", "trend", "direction", "device", "type")

# initial data GET
fullURL <- "https://your-nightscout-url.herokuapp.com/api/v1/entries/sgv.json"
urldata <- getURL(fullURL)
urldata <- fromJSON(urldata)
urldata <- urldata[ , !(names(urldata) %in% drops)]
alldata <- head(urldata,-1)

# get all previous data
while (length(urldata) != 0)
{
  dateString <- urldata[nrow(urldata), which(names(urldata) == "dateString")]
  
  if (dateString != urldata[1, which(names(urldata) == "dateString")])
  {
    fullURL <- paste("http://your-nightscout-url-cgm.herokuapp.com/api/v1/entries/sgv.json?find[dateString][$lte]=", dateString, sep="")
    urldata <- getURL(fullURL)
    urldata <- fromJSON(urldata)
    urldata <- urldata[ , !(names(urldata) %in% drops)]
    alldata <- rbind(alldata, head(urldata, -1))
    #print(urldata[nrow(urldata), 1])
  }
  else
  {
    print("Done")
    break 
  }
}


