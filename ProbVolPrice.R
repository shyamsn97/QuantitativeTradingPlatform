
#@author Shyam Sudhakaran
#This programs searches an index and finds stocks that have a high probality of a reversal, within a time range specified by the user.
#The probability is calculated using a user specified date-presenttime, for example the date has been initialized at "2016-01-04".

library(tidyquant)
library(plyr)

startprob <- function(name,range,StartDate) {
  #stockprice returns
  priceret <- name %>% tq_get(get  = "stock.prices",from = StartDate)
  ret <- priceret %>% tq_transmute(select     = adjusted,
                            mutate_fun = periodReturn,
                            period     = "daily",
                            type       = "log",
                            col_rename = "logreturns")
  d <- data.frame(ret)
  date <- d[nrow(ret),1]
  #date
  returns <- ret[[2]]
  returns <- na.omit(returns)
  #recent return
  latest <- returns[length(returns)]
  latestdate <- priceret[length(priceret),1]
  ###PRICE################################################################
  priceprobabilities <- c()
  #positives
  positives <- c()
  for(i in 1:length(returns)) {
    if(returns[i] > 0) {
      positives <- c(positives,returns[i])
    }
  }
  meanup <- mean(positives)
  #negatives
  negatives <- c()
  for(i in 1:length(returns)) {
    if(returns[i] < 0) {
      negatives <- c(negatives,returns[i])
    }
  }
  meandown <- mean(negatives)
  #mean of positives
  
######################Probabilities#####################
  
  #above 0 reversal
  abovezero <- c()
  abovezeronext <- c()
  countabove <- 0
  for(i in 1:(length(returns)-range)) {
    if(returns[i] > 0) {
      abovezero <- c(abovezero,returns[i])
      if((sum(returns[i:i+range])-returns[i]) < 0) {
        abovezeronext <- c(abovezeronext, (sum(returns[i:i+range])-returns[i]))
        countabove <- countabove+1
      }
    }
  }
  abovezeroprob <- length(abovezeronext)/length(abovezero)
  
  #below zero 
  belowzero <- c()
  belowzeronext <- c()
  countbelow <- 0
  for(i in 1:(length(returns)-range)) {
    if(returns[i] < 0) {
      belowzero <- c( belowzero,returns[i])
      if((sum(returns[i:i+range])-returns[i]) > 0) {
        belowzeronext <- c( belowzeronext, (sum(returns[i:i+range])-returns[i]))
        countbelow <- countbelow + 1
      }
    }
  }
  belowzeroprob <- length( belowzeronext)/length(belowzero)
  
  #above mean
  abovemean <- c()
  abovemeannext <- c()
  countmeanup <- 0
  for(i in 1:(length(returns)-range)) {
    if(returns[i] > meanup) {
      abovemean <- c(abovemean,returns[i])
      if((sum(returns[i:i+range])-returns[i]) < 0) {
        abovemeannext <- c(abovemeannext, (sum(returns[i:i+range])-returns[i]))
        countmeanup <- countmeanup + 1
      }
    }
  }
  abovemeanprob <- length(abovemeannext)/length(abovemean)
  
  #belowmean
  belowmean <- c()
  belowmeannext <- c()
  countmeandown <- 0
  for(i in 1:(length(returns)-range)) {
    if(returns[i] < meandown) {
      belowmean <- c(belowmean,returns[i])
      if((sum(returns[i:i+range])-returns[i]) > 0) {
        belowmeannext <- c( belowmeannext, (sum(returns[i:i+range])-returns[i]))
        countmeandown <- countmeandown + 1
      }
    }
  }
  belowmeanprob <- length( belowmeannext)/length(belowmean)
  
  vars <- c("abovezeroprob","abovemeanprob","belowzeroprob","belowmeanprob")
  probabilities <- c(abovezeroprob,abovemeanprob,belowzeroprob,belowmeanprob)
  probabilities <- probabilities*100
  counts <- c(countabove,countmeanup,countbelow,countmeandown)
  #dataframe of counts & probabilities
  origframe <- data.frame(vars,probabilities,counts)
  frame <- filter(origframe,counts > 60)
  #frame <- cbind(frame,name)
  anomalies <- filter(frame,probabilities >= 93)
###################Prediction For Tomorrow###########################
  prediction <- 0
  row <- 0
  change <- 0
  reaction <- 0
  if(nrow(frame) > 0) {
    if((latest > 0) & "abovezeroprob"%in%frame[,1] ) {
      row <- frame[frame$vars == "abovezeroprob",]
      prediction <- row[,2]
      change <- -1
      reaction <- paste(row[,2],"to go down",sep = " ")
    }
    else if((latest > meanup) & "abovemeanprob"%in%frame[,1] ) {
      row <- frame[frame$vars == "abovemeanprob",]
      prediction <- row[,2]
      change <- -1
      reaction <- paste(row[,2],"to go down",sep = " ")
    }
    else if((latest < 0) & "belowzeroprob"%in%frame[,1] ) {
      row <- frame[frame$vars == "belowzeroprob",]
      prediction <- row[,2]
      change <- 1
      reaction <- paste(row[,2],"to go up",sep = " ")
    }
    else if((latest < meandown) & "belowmeanprob"%in%frame[,1] ) {
      row <- frame[frame$vars == "belowmeanprob",]
      prediction <- row[,2]
      change <- 1
      reaction <- paste(row[,2],"to go down",sep = " ")
    }
  }
  output <- list(name,frame,anomalies,prediction,latest,date,change,reaction)
  return(output)
}

################SEARCH#######################
indexsearch <- function(x){
  anomalyStocks <- c()
  c <- length(x)
  for(i in 1:length(x)) {
    c <- c-1
    print(c)
    try(list <- startprob(x[i],1,"2016-01-04"))
    try(if(nrow(list[[3]]) > 0) {
      anomalyStocks <- c(anomalyStocks,list[[1]])
    })
  }
  return(anomalyStocks)
}

#example
#index <- tq_exchange("NASDAQ")
#stockpicks <- indexsearch(index$symbol)

######################PICKSFORTMRW###################
stockpicker <- function(y) {
  finalpicks <- c()
  final <- 0
  xy <- 0
  for(i in 1:length(y)) {
    try(j <- startprob(y[i],1,"2016-01-04"))
    try(if(j[[4]] > 90) {
      finalpicks <- c(finalpicks,y[i])
      xy <- j[[8]]
      d <- j[[6]]
    })
  }
  finalend <- data.frame(finalpicks,xy,d)
  return(finalend)
}
#example
#endresult <- stockpicker(stockpicks)


######################CAPTUREOUTPUT##############################
captur_out <- function(endresult,directory){
txt <- capture.output(for(i in 1:length(endresult)) {
  try(print(startprob(endresult[i],1,"2016-01-04")))
  cat("\n")
  cat("=======================================================", "\n")
})
write.csv(txt,  "directory")
}
###########################CHECKUPORDOWN#####################################
#check if predictions were right
checkstocks <- function(x) {
  bool <- c()
  names <- c()
  for(i in 1:length(x)) {
    m <- startprob(x[i],1,"2016-01-04")
    STOCK <- x[i] %>% tq_get(get  = "stock.prices",from = "2017-01-04")
    STOCK <- data.frame(STOCK)
    price2 <- STOCK[nrow(STOCK),5]
    date2 <- STOCK[nrow(STOCK)-1,1]
    names <- c(names,m[[1]])
    if(m[[7]]*price2 < 0) {
      bool <- c(bool,FALSE)
    } else if(m[[7]]*price2 > 0) {bool <- c(bool,TRUE)}
    else {bool <- c(bool,"NULL PREDICTION")}
  }
  data <- data.frame(names,bool)
  data <- cbind(data,date2)
  return(data)
}

##############convert to string###############################
final_check_output <- function(endresult,directory){
stringstocks <- c()
for(i in 1:length(endresult)) {
  stringstocks <- c(stringstocks,toString(endresult[i,1]))
}
#check results
rightorwrong <- checkstocks(stringstocks)

write.table(rightorwrong, directory, sep = ",", col.names = FALSE, append = T)
write.table(endresult, directory, sep = ",", col.names = TRUE, append = T)
}

