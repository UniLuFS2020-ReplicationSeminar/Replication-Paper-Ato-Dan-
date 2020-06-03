library("ggplot2")
library("tidyverse")
library("rio")
library("here")

####Figure 2
##This is not the original code as we use our own import functions###
here::here("Data/Org-Affiliations.csv") 
orgs <- rm(list=ls()) 
orgs <- rio::import("Data/Org-Affiliations.csv") 

##Original code starts from here##
props.org<-as.data.frame(prop.table(table(orgs$RANK, orgs$ORGANIZATION),2)*100)
colnames(props.org)<-c("Rank", "Organization", "Freq")
props.org$Rank<-factor(props.org$Rank, levels = c("Top 5","Top 6-10","Top 11-20","Top 21-50","Top 51-100","Not ranked"))

p<-ggplot(props.org, aes(x=Rank, y=Freq, fill=Organization))+scale_fill_grey()
p+geom_bar(position="dodge", stat="identity")+ xlab("\n\nRanking\n")+ ylab("Members of the Councils (%)\n\n")+theme_bw() + theme(axis.text.x  = element_text(size=13), legend.text = element_text(size = 12), legend.title = element_text(size=12))