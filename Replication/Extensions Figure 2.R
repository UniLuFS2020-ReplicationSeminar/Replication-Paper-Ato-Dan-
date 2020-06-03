library("ggplot2")
library("tidyverse")
library("rio")
library("here")
library("RColorBrewer")

####Figure 2
##Import data set
here::here("Data")
list.files()
orgs <- rm(list=ls())
orgs <- rio::import("Data/Org-Affiliations.csv")
View(orgs)

props.org <- as.data.frame(prop.table(table(orgs$RANK, orgs$ORGANIZATION),2)*100)
colnames(props.org) <- c("Rank", "Organization", "Freq")

props.org$Rank<-factor(props.org$Rank, levels = c("Top 5","Top 6-10","Top 11-20","Top 21-50","Top 51-100","Not ranked"))

p <- ggplot(props.org, aes(x = Rank, y = Freq, fill = Organization)) 
p + 
  geom_bar(position="dodge", stat="identity") +  
  #Axis + Title modifications
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8)) + 
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12)) +
  theme(title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 10)) +
  labs(x = "\nRanking", y = "Council Members in (%)\n\n", title = "Academic Background of Council Members",  
       subtitle = "(AEA, APSA and ASA)") + 
  #Background modifications
  theme(panel.background = element_rect(fill='white', color = "grey")) + 
  theme(panel.grid.major = element_line(size = 0.1, colour = "grey")) +
  #Legend modifications
  theme(legend.title=element_blank()) +
  theme(legend.key=element_rect(fill = "White")) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  #Color modifications
  scale_fill_manual(values = brewer.pal(3, "Blues")[3:1])
  

