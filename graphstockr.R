
#@author Shyam Sudhakaran
#This program takes in a ticker symbol, directory, and daterange. It plots the corresponding candlestick chart and RSI for the stock on the interval specified.
#daterange defines how many days are included, success is so function works until completion 
stockplotprice <- function(symbol,daterange,success=FALSE,directory,number=0) {
library(tidyquant)
setwd(directory)
source("multiplot.R")
window <- Sys.Date() - daterange - 1
if(number >= 20){stop()}
try(name <- symbol %>% tq_get(get = "stock.prices",from=window)
    %>% tq_mutate(select = close,
                     mutate_fun = RSI,
                     n = 14,
                     col_rename = "rsi"))
end <- as.Date(name[nrow(name),1][[1]])
if(nrow(name) > 1) {
  success <- TRUE
}
if(success == FALSE) {
  number <- number + 1
  stockplotprice(symbol)
}
graphdate <- end - daterange
main_title <-paste(symbol,"Candlestick Chart",graphdate,"-",end,sep = " ")
rsi_title <-paste(symbol,"RSI Chart",graphdate,"-",end,sep = " ")
p1 <-
  name %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) + 
  geom_point() +
  #geom_line(aes(x=date,y=rsi,color="RSI")) +
  #geom_point() + geom_point() +
  labs(title = main_title)  +
  coord_x_date(xlim = c(graphdate, end))
  p2 <- name %>%
  ggplot(aes(x = date, y = rsi)) +
  geom_line(aes(color="red")) +
  geom_point() + geom_label(aes(label=round(rsi,2)),label.size = 0.5) +
  #geom_line(aes(x=date,y=rsi,color="RSI")) +
  #geom_point() + geom_point() +
  labs(title = rsi_title)  +
  coord_x_date(xlim = c(graphdate, end))
multiplot(p1,p2)
}
