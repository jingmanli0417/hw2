library(tidyverse)
library(sandwich)
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("Hmisc")
library(Hmisc)

#Use quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2020-02-02")
end <- as.Date("2020-06-02")

getSymbols("GS", src = "yahoo", from = start, to = end)
getSymbols("MRK", src = "yahoo", from = start, to = end)
getSymbols("AZN", src = "yahoo", from = start, to = end)
getSymbols("MS", src = "yahoo", from = start, to = end)
getSymbols("MSFT", src = "yahoo", from = start, to = end)
getSymbols("V", src = "yahoo", from = start, to = end)
getSymbols("PG", src = "yahoo", from = start, to = end)
getSymbols("MA", src = "yahoo", from = start, to = end)
getSymbols("INTC", src = "yahoo", from = start, to = end)
getSymbols("ZM", src = "yahoo", from = start, to = end)

df = data.frame(GS = GS$Close,MRK=MRK$Close,AZN=AZN$Close,MS=MS$Close,MSFT=MSFT$Close,
                V=V$Close,PG=PG$Close,MA=MA$Close,INTC=INTC$Close,ZM=ZM$Close,
                ncon=globaldata$newconfirmed,ndeath=globaldata$newdeath)

#analyze which stock correlation with, new confirmed cases of COVID-19, new deaths from COVID-19
M<-cor(df)
corrplot(M, method="number")
#from the correlation matrix, we can see that ZM (ZOOM) stock prices correlates with new cases & new deaths the best

#Plot the visualization (scatter plot of Zoom stock)
cor(df$ZM, df$ncon, use = "complete.obs", method = "pearson")
ggplot(df, aes(x= ZM, y = ncon)) + geom_point()

cor(df$ZM, df$ndeath, use = "complete.obs", method = "pearson")
ggplot(df, aes(x= ZM, y = ndeath)) + geom_point()


#Multiple variable correlation among stocks prices 
data = data.frame(GS = GS$Close,MRK=MRK$Close,AZN=AZN$Close,MS=MS$Close,MSFT=MSFT$Close,
                 V=V$Close,PG=PG$Close,MA=MA$Close,INTC=INTC$Close,ZM=ZM$Close)

cormat <- rcorr(as.matrix(data))
heatmap(x = cormat$r, symm = TRUE)

#Hierarchical clustering to stocks and visualize as heatmap
d <- dist(data, method = "euclidean")
heatmap(as.matrix(d))
