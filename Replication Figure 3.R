library(ggplot2)
library(tidyverse)

# Figure 3
# who do economists cite?

disjunct<-read.csv("Base5j_Disj_Redux.csv")
#dimensions of the data set:
dim(disjunct) 
table(disjunct$JOURNAL, disjunct$YEAR)

#plotting the year and the number of references in percentage:
p <- ggplot(disjunct, aes(factor(YEAR), NUMB_REF))
p + geom_boxplot(outlier.size = NA) + 
  ggtitle("Number of references in 5 Top Economics Journals\nAER, QJE, JPE, RES, ECTRCA\n") + 
  theme(plot.title=element_text(face="bold", size=14))+ 
  labs(x = "\nYear", y = "Percentage\n")+ 
  scale_x_discrete(breaks = seq(1900,2012,10), labels = seq(1900,2010,10)) + 
  ylim(0,100)+theme_bw() + 
  stat_summary(fun.y="median", geom="point", shape=23, size=2, fill="brown4")

