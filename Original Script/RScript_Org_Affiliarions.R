library(tidyverse)

data1 <- Org_Affiliations 

view(data1)
summary(data1)
class(data1)

props.org<-as.data.frame(prop.table(table(data1$RANK, data1$ORGANIZATION),2)*100)
colnames(props.org)<-c("Rank", "Organization", "Freq")
props.org$Rank<-factor(props.org$Rank, levels = c("Top 5","Top 6-10","Top 11-20","Top 21-50","Top 51-100","Not ranked"))

p<-ggplot(props.org, aes(x=Rank, y=Freq, fill=Organization))+scale_fill_grey()
p+geom_bar(position="dodge", stat="identity")+ xlab("\n\nRanking\n")+ ylab("Members of the Councils (%)\n\n")+theme_bw() + theme(axis.text.x  = element_text(size=13), legend.text = element_text(size = 12), legend.title = element_text(size=12))
