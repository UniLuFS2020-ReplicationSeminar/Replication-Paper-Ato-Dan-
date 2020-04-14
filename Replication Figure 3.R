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
disjunct$TYP <- cut(disjunct$TYP, breaks = seq(1900,2012, by = 2), labels = sin_labs, include.lowest = T)

hedhehehedhedhdhedh


hedhedh



