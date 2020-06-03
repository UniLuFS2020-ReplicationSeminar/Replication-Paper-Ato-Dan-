library("ggplot2")
library("tidyverse")
library("rio")
library("here")

####Figure 2
##Import data set
here::here("Data/Org-Affiliations.csv") #Use here::here to set working directory
orgs <- rm(list=ls()) #Check available files in current working directory
orgs <- rio::import("Data/Org-Affiliations.csv") #Import actual dataset

##Data processing
props.org <- as.data.frame(prop.table(table(orgs$RANK, orgs$ORGANIZATION),2)*100) #making a prop table with the variables RANK and ORGANISATION
colnames(props.org) <- c("Rank", "Organization", "Freq") #creating vector of RANK, ORGANISATION and their Frequencies 
class(orgs$RANK) #we have a nominally scaled variable, thats why they transform it into a factor variable (see next line)
props.org$Rank <- factor(props.org$Rank, levels = c("Top 5","Top 6-10","Top 11-20","Top 21-50","Top 51-100","Not ranked")) #ordering the new factor variable in an ascending order

##Plotting the data 
p <- ggplot(props.org, aes(x = Rank, y = Freq, fill = Organization)) + scale_fill_grey()
p + geom_bar(position="dodge", stat = "identity") + 
  xlab("\n\nRanking\n") + ylab("Members of the Councils (%)\n\n") + 
  theme_bw() + 
  theme(axis.text.x  = element_text(size = 13), legend.text = element_text(size = 12), legend.title = element_text(size=12))