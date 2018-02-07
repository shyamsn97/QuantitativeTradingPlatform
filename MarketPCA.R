
#@author Shyam Sudhakaran
#This program uses PCA to find the sector, within an index, that results in the most variance of returns.
library(tidyquant)
library(plyr)
index <- tq_exchange("NASDAQ")
index <- split(index,index$sector)
index
names <- names(index)
names
retmeans

for(i in 1:length(index)) {
  symbols <- c()
  symbols <- data.frame(index[i])
  symbols <- symbols[,1]
  j <-c()
  j <- symbols %>% 
    tq_get(get  = "stock.prices",from = "2016-01-04") %>% group_by(symbol)
    ret <- j %>% tq_transmute(select     = adjusted,
                            mutate_fun = periodReturn,
                            period     = "daily",
                            type       = "log",
                            col_rename = "returns") %>%
                      spread(key = symbol, value = returns)
  retmeans <- rowMeans(ret[,2:ncol(ret)],na.rm=TRUE)
  if(i == 1) {
    frame <- data.frame(retmeans)
  }
  else {
    length(retmeans) = nrow(frame)
    frame <- cbind(frame,retmeans)
  }
}

frame[is.na(frame)] <- 0
frame
colnames(frame) <- names
frame

prin_comp <- prcomp(frame,scale. = TRUE)
prin_comp
prin_comp$rotation
dim(prin_comp$x)
names(prin_comp)
biplot(prin_comp, scale = 0)
std_dev <- prin_comp$sdev
std_dev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
prop_varex
summary <- summary(prin_comp)


portions <- prin_comp$sdev^2/sum(prin_comp$sdev^2)


#loadings(prin_comp)
aload <- abs(prin_comp$rotation)
final <- sweep(aload, 2, colSums(aload), "/")
#PROPORTION OF VARIABLES
final
summary
portions
#PROPORTION OF VARIANCE
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
weights <-c()
final <- data.frame(final)
length(final)

for(i in 1:length(final)) {
  weight <- final[i,]*portions
  weight <- sum(weight)
  weights <- c(weights,weight)
}

end <- data.frame(names,weights)
end <- end[order(weights),]

