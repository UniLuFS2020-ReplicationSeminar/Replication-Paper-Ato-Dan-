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







