# Replicating Figure 4
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape)

####Figure 4
rm(list=ls())
crossrefs <- rio::import("Data/crossrefs.csv")
dim(crossrefs)
colnames(crossrefs)

# Normalize by year/ journal
crossrefs2 <- cbind(crossrefs[,c(1:2)],sweep(crossrefs[,3:9],1,crossrefs[,11],`/`)*100) # definition of new variable crossrefs2
view(crossrefs2)

#Reshape
m_crossrefs <- melt(crossrefs2, id =c("CITING", "YEAR")) #does not change much because it is just a transformation from the entries of all journals and their columns to two new columns 
colnames(m_crossrefs) <- c("CITING", "YEAR", "CITED", "VALUE") #naming new columns CITED and VALUE

#Scaling x- and y-axis
x=seq(1950,2010, by=2)
y=seq(1951,2011, by=2)

#Using for loop function
file <- NULL 

for (i in 1:length(x)){
  TAB <- m_crossrefs[m_crossrefs$YEAR %in% c(x[i]:y[i]),] #dim(TAB)	
  head(TAB)
  mat <- xtabs(VALUE~CITING+CITED, data = TAB)#Present data as a matrix
  mat <- mat[,rownames(mat)]
  diag(mat) <- 0 #Remove self-citations
  cited <- addmargins(mat,1)[8,] #Calculate sum citations received 
  citing <- addmargins(mat,2)[,8] #Calculate sum citations made
  mat <- sweep(mat[,1:7],1,citing,`/`)*100
  page <- cbind(melt(mat), YEAR = rep(paste(x[i],"-",y[i], sep =""), nrow(melt(mat))))
  file <- rbind(file, page)
}
View(mat)
#Remove self-citations	
noself <- file[file$CITING != file$CITED,]
head(noself)
noself2<-aggregate(noself$value, by=list(noself$YEAR, noself$CITED), FUN=mean)
colnames(noself2)<-c("PERIOD", "CITED", "VALUE")
head(noself2, 10)
noself2$linetype<-as.character(noself2$CITED)
head(noself2)
tail(noself2, 200)

noself2$linetype[noself2$linetype == "AER"]<-"solid"
noself2$linetype[noself2$linetype == "ECTRA"]<-"dashed"
noself2$linetype[noself2$linetype == "EJ"]<-"dotted"
noself2$linetype[noself2$linetype == "JFIN"]<-"dotdash"
noself2$linetype[noself2$linetype == "JPE"]<-"longdash"
noself2$linetype[noself2$linetype == "QJE"]<-"F1"
noself2$linetype[noself2$linetype == "RES"]<-"12345678"

p <- ggplot(noself2, aes(x=PERIOD, y=VALUE, group=CITED))
p + geom_smooth(aes(group=CITED), se=F, span=.5, linetype=noself2$linetype, color="black") + 
  ggtitle("\n") + 
  scale_shape_manual(values=c(1:7)) + 
  theme(axis.text.x = element_text(size = 6)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "\nPeriod", y = "% of citations received\n")+  
  theme(plot.title = element_text(family="Arial", face = "bold", size = 14)) + 
  theme(panel.background = element_rect(fill='white', colour='grey')) + 
  geom_point(aes(shape = CITED), alpha=7/10)

