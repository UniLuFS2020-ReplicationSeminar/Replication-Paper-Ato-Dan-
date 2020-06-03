library("ggplot2")
library("tidyverse")
library("rio")
library("reshape")
library("RColorBrewer")

####Figure 3
##who do economists cite?
here::here("Data")
list.files()
disjunct <- rio::import("Data/Base5j_Disj_Redux.csv")
View(disjunct)

##dimensions of the data set:
dim(disjunct) 
table(disjunct$JOURNAL, disjunct$YEAR)

##creating a new variable: "two year period" 
disjunct$TYP <- disjunct$YEAR
TYP_labs <- paste(seq(1900,2010, by=2), seq(1902,2012, by=2),sep="-") #labs is not a good choice for a new character name because it is also the name of the function "labels". Better: TYP_labs. 
disjunct$TYP <- cut(disjunct$TYP, breaks = seq(1900,2012, by = 2), labels = TYP_labs, include.lowest = T) #There is a seq intercal of two, because the plot in the paper visualizes two year period

##unaccounted citations of fields that are not relevant for figure 3 (DK + OTHERS)
#here is not clear. what DK means
colnames(disjunct)
disjunct$UNACC<-apply(disjunct[,c(9,25)],1,sum) #they indicate all rows in a vector of the columns "OTHER" and "DK" because they will not be accounted
disjunct$TOTAL_CITES<-apply(disjunct[,c(5:31)], 1, sum) 
disjunct$ACCOUNTED <- disjunct$TOTAL_CITES-disjunct$UNACC #makes perfect sense, but one could also calculate it in positive way
disjunct$UNACC_RATE <- (disjunct$UNACC/disjunct$TOTAL_CITES)*100 #what exactly do we calculate and for what reason?
View(disjunct)

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
disjunct_percent<-cbind(PERIOD=disjunct1_agg[,1], sweep(disjunct1_agg[,3:28],1,disjunct1_agg[,28],`/`)*100) #why didnt they exclude ACCOUNTED in the previous definition?

dim(disjunct_percent)
disjunct_percent<-aggregate(disjunct_percent[,c(2:27)], by=list(disjunct_percent$PERIOD), FUN=mean)

######## Graphs
#With economics (1950:2012)
disjunct_percent1<-disjunct_percent[26:56,c(1:6,9,12,13)]
m_disjunct_percent<-melt(disjunct_percent1, id="Group.1")
colnames(m_disjunct_percent)<-c("PERIOD", "DISCIPLINE", "FREQUENCY")

#Without economics (1950:2012)
colnames(disjunct_percent)
disjunct_percent2 <- disjunct_percent[26:56,c(1,3:6,9,11,12,13,16)]

#renaming columns (Eliminating capital letters):
disjunct_percent2 <- disjunct_percent2 %>% select(Group.1 = Group.1, Finance = FINANCE, Business = BUSINESS, Statistics = STATISTICS, Polscience = POLSCIENCE, Law = LAW, Psychology = PSYCHOLOGY, Mathematics = MATHEMATICS, Sociology = SOCIOLOGY, Health = HEALTH)

colnames(disjunct_percent2)
View(disjunct_percent2)

m_disjunct_percent<-melt(disjunct_percent2, id="Group.1")
m_disjunct_percent$linetype<-as.character(m_disjunct_percent$variable)
colnames(m_disjunct_percent)<-c("PERIOD", "DISCIPLINE", "FREQUENCY", "linetype")
head(m_disjunct_percent)
tail(m_disjunct_percent,200)


q <- ggplot(m_disjunct_percent, aes(x=PERIOD,  y = FREQUENCY, group = DISCIPLINE))
q + 
  geom_smooth(aes(group=DISCIPLINE, color = DISCIPLINE), se = FALSE, span = .5) + 
  geom_point(aes(color = DISCIPLINE), size = 0.8, alpha=7/10) + 
  #Axis + Title modifications
  theme(axis.text.x = element_text(size = 8, angle = -60, hjust = -0.1)) + 
  theme(axis.text.y = element_text(size = 8)) + 
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12)) +
  theme(title = element_text(size = 12)) +
  labs(x = "\nPeriod", y = "% of citations\n", title = "Extradisciplinary Citation in Five Top Economics Journals") +
  scale_colour_manual(values = brewer.pal(9, "Paired")[1:9]) +
  #Bckground Modifications 
  theme(panel.background = element_rect(fill='white', color = "grey")) + 
  theme(panel.grid.major = element_line(size = 0.1, colour = "grey")) +
  #Legend modifications
  theme(legend.title=element_blank()) +
  theme(legend.key=element_rect(fill = "White")) +
  guides(colour = guide_legend(override.aes = list(size = 1)))+
  theme(legend.text = element_text(size = 10)) +
  #Color Modifications
  scale_color_brewer(palette = "Paired")


