library(ggplot2)
library(tidyverse)

####Figure 3
##who do economists cite?

disjunct<-read.csv("Base5j_Disj_Redux.csv")

##dimensions of the data set:
dim(disjunct) 
table(disjunct$JOURNAL, disjunct$YEAR)

##plotting the year and the number of references in percentage:
p <- ggplot(disjunct, aes(factor(YEAR), NUMB_REF))
p + geom_boxplot(outlier.size = NA) + #aren't the outliers still present in the plot??
  ggtitle("Number of references in 5 Top Economics Journals\nAER, QJE, JPE, RES, ECTRCA\n") + 
  theme(plot.title=element_text(face="bold", size=14))+ 
  labs(x = "\nYear", y = "Percentage\n")+ #why is the description in the quotation marks not equal to the description of the layers in the plot?
  scale_x_discrete(breaks = seq(1900,2012,10), labels = seq(1900,2010,10)) + #the labels are going til 2010, but the data reaches up to 2012
  ylim(0,100)+
  theme_bw() + #white background
  stat_summary(fun.y="median", geom="point", shape=23, size=2, fill="brown4") #fun.y is an outdated function; shows the median trend in the plot


##creating a new variable: "two year period" 
disjunct$TYP <- disjunct$YEAR
sin_labs <- paste(seq(1900,2010, by=2), seq(1902,2012, by=2),sep="-") #labs is not a good choice for a new character name because it is also the name of the function "labels". Better: sin_labs. 
disjunct$TYP <- cut(disjunct$TYP, breaks = seq(1900,2012, by = 2), labels = sin_labs, include.lowest = T) #why is there a seq interval of 2?


##unaccounted citations of fields that are not relevant for figure 3 (DK + OTHERS)
#here is not clear. what DK means
colnames(disjunct)
disjunct$UNACC<-apply(disjunct[,c(9,25)],1,sum) #why do they choose this values of c()?
disjunct$TOTAL_CITES<-apply(disjunct[,c(5:31)], 1, sum) 
disjunct$ACCOUNTED<-disjunct$TOTAL_CITES-disjunct$UNACC #makes perfectly sense, but one could also calculate it in positive way
disjunct$UNACC_RATE <- (disjunct$UNACC/disjunct$TOTAL_CITES)*100 

##Appendix
##plotting the Unaccounted-rate
p <- ggplot(disjunct, aes(factor(YEAR), UNACC_RATE)) + stat_boxplot(outlier.size = NA) #why year with the factor function?

p + geom_boxplot(outlier.size = NA) + 
    theme(plot.title=element_text(face="bold", size=14)) + 
    labs(x = "\nYear", y = "Percentage\n") + 
    scale_x_discrete(breaks = seq(1900,2012,10), labels = seq(1900,2010,10)) + #again (like in line 19)
    theme_bw()  +  #white background
    stat_summary(fun.y="median", geom="point", shape=23, size=2, fill="darkgoldenrod")


#######Standardization. Compute for each article the percentage of each discipline relative to the number of citations accounted for
#######Reorder the dataframe for the sake of clarity
##for the definition of a base year (1900)
colnames(disjunct)
disjunct1 <- disjunct[,c(1,2,4,5:8,10:24,26:31,36,33,34)] #DK,OTHER,NR,UNACC_RATE,TOTAL_CITES are excluded
colnames(disjunct1)
head(disjunct1,1)
dim(disjunct1) 


######Percentage of each discipline (relative to the number of articles *accounted for*), per discipline
#First, aggregate by a period of 2 years
colnames(disjunct1)
disjunct1_agg <- aggregate(disjunct1[,c(4:29)], by = list(disjunct1$TYP, disjunct1$JOURNAL), FUN=sum) #aggregating the study programmes (and ACCOUNTED) by listing through two year period variable and Journal. Unclear why ACCOUNTED is included here


dim(disjunct1_agg) # head(disjunct1_agg)
disjunct_percent<-cbind(PERIOD=disjunct1_agg[,1], sweep(disjunct1_agg[,3:28],1,disjunct1_agg[,28],`/`)*100)

dim(disjunct_percent)
disjunct_percent<-aggregate(disjunct_percent[,c(2:27)], by=list(disjunct_percent$PERIOD), FUN=mean)

######## Graphs
#With economics (1950:2012)
disjunct_percent1<-disjunct_percent[26:56,c(1:6,9,12,13)]
m_disjunct_percent<-melt(disjunct_percent1, id="Group.1")
colnames(m_disjunct_percent)<-c("PERIOD", "DISCIPLINE", "FREQUENCY")

p<-ggplot(m_disjunct_percent, aes(PERIOD,  FREQUENCY))
p +geom_bar(stat="identity") + facet_grid(DISCIPLINE~., scale='free_y') + theme(legend.position="none")  + ggtitle("Who do Economists Cite?\n Most cited disciplines cited in Five top journals (1900-2012)\n") + theme(axis.text.x = element_text(size = 6)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "\nYear", y = "% of citations to this discipline\n")+  theme(plot.title=element_text(family="Arial", face="bold", size=14))  + theme(strip.text.y = element_text(size = 5))+ theme(panel.background = element_rect(fill='white', colour='grey'))

#Without economics (1950:2012)
colnames(disjunct_percent)
disjunct_percent2<-disjunct_percent[26:56,c(1,3:6,9,11,12,13,16)]

m_disjunct_percent<-melt(disjunct_percent2, id="Group.1")
m_disjunct_percent$linetype<-as.character(m_disjunct_percent$variable)
colnames(m_disjunct_percent)<-c("PERIOD", "DISCIPLINE", "FREQUENCY", "linetype")
head(m_disjunct_percent)
tail(m_disjunct_percent,200)
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "FINANCE"]<-"solid"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "MATHEMATICS"]<-"dotdash"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "STATISTICS"]<-"12345678"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "SOCIOLOGY"]<-"dotted"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "BUSINESS"]<-"longdash"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "POLSCIENCE"]<-"F1"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "LAW"]<-"1F"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "PSYCHOLOGY"]<-"twodash"
m_disjunct_percent$linetype[m_disjunct_percent$linetype == "HEALTH"]<-"dashed"

q<-ggplot(m_disjunct_percent, aes(x=PERIOD,  y=FREQUENCY, group=DISCIPLINE))
q + geom_smooth(aes(group=DISCIPLINE), se=F, span=.4, linetype=m_disjunct_percent$linetype, color="black") +geom_point(aes(shape=DISCIPLINE, size = 3), alpha=7/10)+ ggtitle("\n") + theme(axis.text.x = element_text(size = 6)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "\nPeriod", y = "% of citations\n")+  theme(plot.title=element_text(family="Arial", face="bold", size=14)) + theme(panel.background = element_rect(fill='white', colour='grey')) + scale_shape_manual(values=c("F", "B", "S", "P", "L", "p", "M", "s", "H")) +theme(legend.position="none") 







