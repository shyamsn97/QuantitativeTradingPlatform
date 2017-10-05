
#@author Shyam Sudhakaran
##This program searches for "bargain" stocks based on classic value analysis methods (evaluating P/E,P/B, comparing to other companies in the sector)
#It exports the list of bargain stocks into a csv file

#############################################PICK INDEX#########################################
index_average <- function(indexname) {
  library(tidyquant)
  #calculates average price to earnings ratio using ebitda and market cap of each company
  indexmain <-tq_exchange(indexname)
  index <- split(indexmain,indexmain$sector)
  sectors <- names(index)
  #subset <- index[[6]]
  averages <- c()
  belowpecomps <- c()
  for(k in 1:length(sectors)) {
    sectorname <- sectors[k]
    
    subset_by_sector <- index[[k]] #get sector
   
    names_stocks <- subset_by_sector[["symbol"]] #symbols of sector
    
    keystats <- try(names_stocks %>% tq_get(get="key.stats")) #keystats for each stock
    keystatsv <- keystats[!is.na(keystats["PE.Ratio"]),] #eliminates those with na ebitda
    avg_pe <- sum(keystatsv[["PE.Ratio"]])/nrow(keystats)
    
    newcomps <- keystatsv %>% filter(PE.Ratio < avg_pe)
    newcompsnames <- newcomps[["symbol"]]
    belowpecomps <- c(belowpecomps,newcompsnames)
    averages <- c(averages,avg_pe) #adds to averages
  }
  average_by_sector <- data.frame(sectors,averages)
  listl <- list(average_by_sector,belowpecomps)
  #listl
  return(listl)
}

#average_by_sector
#belowpecomps
##################################FIND SPECIFIC COMPANIES##########################################
findvalue <- function(companies) {
  
  viewstock <- companies %>% tq_get(get="key.stats")
  #viewratios <- "AAPL" %>% tq_get(get="key.ratios")
  #v <- viewratios[[2]][[7]] %>% filter(date == max(date))
  #v
  marketcapital <- viewstock[["Market.Capitalization"]]
  viewstock <- viewstock %>% filter(Price.to.Book < 1.33)
  
  rsival <- c()
  stockysymbols <- c()
  for(i in 1:length(viewstock[["symbol"]])) {
    try(viewstockret <- viewstock[["symbol"]][i] %>% tq_get(get="stock.prices",from="2016-01-04"))
    try(viewstockret <- na.omit(viewstockret))
    rsi <- 0
    try( rsi <-viewstockret %>% tq_transmute(select = close,
                              mutate_fun = RSI,
                              col_rename = "RSI"))
  
    try(rsi <- rsi[[nrow(rsi),2]])
    if(rsi > 0) {
      stockysymbols <- c(stockysymbols,viewstock[["symbol"]][i])
    }
    try(rsival <- c(rsival,rsi))
  }
  #stockysymbols <- viewstock[["symbol"]]
  
  try(rsieval <- data.frame(stockysymbols,rsival))
  
  colnames(rsieval) <- c("stockies","rsivalues")
  
  rsieval <- rsieval[rsieval$rsivalues<=30,]
  
  stockns <- as.character(rsieval[,1])
  
  filtered_by_stock <- viewstock %>% filter(symbol %in% stockns)
  
  pricechange <- filtered_by_stock[["Change.in.Percent"]]
  price_ofday <- filtered_by_stock[["Last.Trade.Price.Only"]]
  p2b <- filtered_by_stock[["Price.to.Book"]]
  p2e <- filtered_by_stock[["PE.Ratio"]]
  
  keyrats <- indexmain[(indexmain$symbol %in% stockns),]
 
  finalsecs <- keyrats[["sector"]]
  
  avgsect <- c()
  for(i in 1:length(finalsecs)) {
    sectval <- average_by_sector[average_by_sector$sectors == finalsecs[i],2]
    avgsect <- c(avgsect,sectval)
  }

  
  rsieval <- cbind(rsieval,pricechange)
  rsieval <- cbind(rsieval,price_ofday)
  rsieval <- cbind(rsieval,p2b)
  rsieval <- cbind(rsieval,p2e)
  rsieval <- cbind(rsieval,avgsect)
  rsieval <- cbind(rsieval,finalsecs)
  rsieval <- cbind(rsieval,Sys.Date())
  return(rsieval)
  #rsieval <- rsieval[,1:2]
  #viewstockbal <- "AAPL" %>% tq_get(get="financials")
  #balance_sheet <- viewstockbal[[3]][[1]] %>% filter(date == max(date))
  #balance_sheet[is.na(balance_sheet)] <- 0
  #netassval <- balance_sheet[[17,4]] - balance_sheet[[14,4]] - balance_sheet[[13,4]]
  #marketcapital/netassval*1000000
}
#############NYSE#################################################
#char <- c("AMRC","MT","AAPL")
#findvalue(char)
index_stock_search <- function(x) {
  valueindex <- index_average(x)
  NAME_INDEX <- x
  stockpickers <- findvalue(valueindex[[2]])
  stockpickers <- cbind(stockpickers,NAME_INDEX)
  return(stockpickers)
}
#n <- index_average("NASDAQ")
############################WRITE CSV#######################################################################
write_to_csv <- function(stockpicks1,stockpicks2) {
write.table(stockpicks1, "/home/shyam/Desktop/StockShit/R_programming/schedules/anomalies_predictions/value.csv", sep = ",",row.names = FALSE, col.names = TRUE, append = T)
write.table(stockpicks2, "/home/shyam/Desktop/StockShit/R_programming/schedules/anomalies_predictions/value.csv", sep = ",",row.names = FALSE, col.names = TRUE, append = T)
}
#stockns
# stocknsprice <- stockns %>% tq_get(get="stock.prices")
# stockindprice <- split(stocknsprice,stocknsprice$symbol)
# ###############################PLOT STOCKS#################################################################
# end <- as_date(Sys.Date() -1)
# names_stockpickers <- as.character(stockpickers[["stockies"]])
# names_stockpickers
# 
# start <- end - weeks(10)
# 
# names_stockprices <- names_stockpickers %>% tq_get(get="stock.prices", from=start) %>% filter(date >= start + days(10)) %>%
#   ggplot(aes(x = date, y = close, group = symbol)) +
#   geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
#   labs(title = "Value Stocks", 
#        y = "Closing Price", x = "DATE") + 
#   coord_x_date(xlim = c(start+10, end)) +
#   facet_wrap(~ symbol, ncol = 2, scale = "free_y") + 
#   theme_tq()
# names_stockprices
# 
# TAHO <- "TAHO" %>% tq_get(get="key.stats")
# TAHO["Price.to.Book"]
# TAHO["Last.Trade.With.Time"]
