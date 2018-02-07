
#@author Shyam Sudhakaran
#Imports the files valueanalysis.R, probvolprice.R, GetNews.R,  and graphstockr.R
#User can call functions from the various files freely
#####################STARTUP##############################
#replace this with personal directory
maind <- "directory"
#filenames
filenames <- c("valueanalysis.R","GetNews.R","graphstockr.R","probvolprice.R")
get_funct <- function(directory,filenames,libraries) {
  setwd(directory)
  source("sourcefiles.R")
  source_files(filenames)
  functions <- function_names(filenames)
  return(functions)
}
#function names in each source file
functionlist <- get_funct(maind,filenames)
functionlist
####################STOCKSEARCH###########################
#search index for bargain stocks - enter index name
NYSE <- index_stock_search("NYSE")
NASDAQ <- index_stock_search("NASDAQ")
#plotstocks
plot1 <- stockplotprice("NWPX",100) #daterange
#search the news for articles
recent_news_list <- get_recent_news("TAHO",100,50,as.Date("2017-07-01")) #symbol,max news articles, date range, start date
recent_news_list[[1]]
lawsuitsCTL <- recent_news_list[[2]]
lawsuitsCTL
