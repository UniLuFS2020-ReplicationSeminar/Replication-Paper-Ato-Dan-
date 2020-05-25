library("ggplot2")
library("tidyverse")
library("rio")
library("here")

####Figure 2
##Import data set
here::here("Data")
list.files()
orgs <- rm(list=ls())
orgs <- rio::import("Data/Org-Affiliations.csv")
View(orgs)

#
props.org <- as.data.frame(prop.table(table(orgs$RANK, orgs$ORGANIZATION),2)*100)
colnames(props.org) <- c("Rank", "Organization", "Freq")

props.org$Rank<-factor(props.org$Rank, levels = c("Top 5","Top 6-10","Top 11-20","Top 21-50","Top 51-100","Not ranked"))

p<-ggplot(props.org, aes(x=Rank, y=Freq, fill=Organization)) +
  scale_fill_brewer(palette = "Set1")
p+geom_bar(position="dodge", stat="identity")+ 
  xlab("\n\nRanking\n")+ 
  ylab("Council Members in (%)\n\n")+
  theme_bw() + 
  theme(axis.text.x  = element_text(size=06), legend.text = element_text(size = 12), legend.title = element_text(size=12)) +
  ggtitle("Academic background of counsil members 
          (AEA, APSA and ASA)\n") 
