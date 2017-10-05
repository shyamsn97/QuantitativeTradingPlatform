
#@author Shyam Sudhakaran
#getNews takes in a ticker symbol, a number of articles, and a start date. It parses XML of googlefinances' company news page for the particular company and extracts articles starting from the most recent
#get_recent_news looks for recent developments in a company, such as lawsuits or any other "ALERT"s.

getNews <- function(symbol,number,startdate) {
	# Warn about length
	if (number>300) {
		warning("May only get 300 stories from google")
	}
	# load libraries
	library(XML); library(plyr); library(stringr); library(lubridate);
	library(xts); library(RDSTK)
  
	# construct url to news feed rss and encode it correctly
	url.b1 = 'http://www.google.com/finance/company_news?q='
	url    = paste(url.b1, symbol, '&output=rss', "&start=", 1,
	               "&num=", number,"&startdate=",startdate,"&enddate=",(startdate+31), sep = '')
	url    = URLencode(url) ###encode the url
	
	# parse xml tree, get item nodes, extract data and return data frame
	doc   = xmlTreeParse(url, useInternalNodes = TRUE) #parse the tree
	nodes = getNodeSet(doc, "//item") #get html <item>
	mydf <- xmlToDataFrame(nodes) #convert all of the <items> into dataframes
	names(mydf) = str_replace_all(names(mydf), "value\\.", "")
	# convert pubDate to date-time object and convert time zone
	mydf$pubDate = strptime(mydf$pubDate, 
						format = '%a, %d %b %Y %H:%M:%S', tz = 'GMT')
	#mydf$pubDate = with_tz(pubDate, tz = 'America/New_york')
	#list of dates
  sort(mydf$pubDate,decreasing = TRUE)
  mydf$date <- format(mydf$pubDate, format="%Y-%m-%d")
	#Parse the description field
	mydf$description <- as.character(mydf$description) #make descriptions strings
	parseDescription <- function(x) {
	  success <- FALSE
	  while(success==FALSE){
		  out <- html2text(x)$text
		  if(length(out) > 0){
		    success <- TRUE
		  }
	  }
		out <- strsplit(out,'\n|--|\n-|\n- ')[[1]] #split
		#Find Lead
		Site <- out[3] #get the site
		text <- sapply(out,nchar) #count chars
		text <- sort(text,TRUE)
	  out <- out[nchar(out)==text[[1]]|(nchar(out)==text[[2]])] #get top 2 longest strings
	  Lead <- paste(out, collapse = "||") #paste them together
		out <- c(Site,Lead)
		names(out) <- c('Site','Lead')
		return(out)
	}
	description <- lapply(mydf$description,parseDescription)#apply to everything in description
	description <- do.call(rbind,description)
	titles <- as.character(mydf$title)
	sites <- description[,"Site"]
	descriptions <- description[,"Lead"]
	finaldf <- data.frame(mydf$pubDate,titles)
  finaldf <- cbind(finaldf,descriptions)
  finaldf <- cbind(finaldf,sites)
  colnames(finaldf) <- c("date","title","description","site")
	return(finaldf) 
}

get_recent_news <- function(symbol,number,date_range,startdate) {
  recent_news <- 0
  news_list <- list()
  try(symbol_news <- getNews(symbol,number,startdate)) #symbols, amount of articles
  try(recent_news <- symbol_news[symbol_news[,1] >= (Sys.Date()-date_range),]) #returns news articles from date_range earliest days ago
  try(recent_news <- recent_news[order(recent_news$date),])
  try(recent_news <- recent_news[rev(order(recent_news$date)),])
  recent_news_lawsuit <- recent_news[grepl("ALERT",recent_news$title) | grepl("LLP",recent_news$title),]
  news_list[[1]] <- recent_news
  news_list[[2]] <- recent_news_lawsuit
  return(news_list)
}
