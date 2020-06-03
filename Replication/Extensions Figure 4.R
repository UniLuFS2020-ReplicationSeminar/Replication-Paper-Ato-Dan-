####Figure 4
##Import data
rm(list=ls())
crossrefs <- rio::import("Data/crossrefs.csv")
View(crossrefs)
dim(crossrefs)

# Normalize by year/ journal
crossrefs2<-cbind(crossrefs[,c(1:2)],sweep(crossrefs[,3:9],1,crossrefs[,11],`/`)*100) # definition of new variable crossrefs2: # all vectors including all rows of journals / the vector of total cites 

#Reshape
m_crossrefs <- melt(crossrefs2, id=c("CITING", "YEAR")) #does not change much, because it is just a transformation 
colnames(m_crossrefs)<-c("CITING", "YEAR", "CITED", "VALUE")

################################## Figure 4: Citations received from the 6 other journals (Figure 4)
x=seq(1950,2010, by=2)
y=seq(1951,2011, by=2)

file<-NULL 

for (i in 1:length(x)){
  TAB <- m_crossrefs[m_crossrefs$YEAR %in% c(x[i]:y[i]),] #dim(TAB)	
  head(TAB)
  mat<-xtabs(VALUE~CITING+CITED, data=TAB)#Present data as a matrix
  mat<-mat[,rownames(mat)]
  diag(mat)<-0 # Remove self-citations
  cited<-addmargins(mat,1)[8,] #Calculate sum citations received 
  citing<-addmargins(mat,2)[,8] #Calculate sum citations made
  mat<-sweep(mat[,1:7],1,citing,`/`)*100
  page<-cbind(melt(mat), YEAR=rep(paste(x[i],"-",y[i], sep=""), nrow(melt(mat))))
  file<-rbind(file, page)
}
#Remove self-citations	
noself<-file[file$CITING != file$CITED,]
head(noself)
noself2<-aggregate(noself$value, by=list(noself$YEAR, noself$CITED), FUN=mean)
colnames(noself2)<-c("PERIOD", "CITED", "VALUE")
head(noself2, 10)
noself2$linetype<-as.character(noself2$CITED)
head(noself2)
tail(noself2, 200)

p <-ggplot(noself2, aes(x = PERIOD, y = VALUE, group = CITED))
p + geom_smooth(aes(group = CITED, color = CITED), se = FALSE, span = .5) +
  #Labs + x and y-axis modifications
  labs(title = "\nCitations among Six Economics Journals and One Finance Journal",
       subtitle = "(excluding self-citations)") +  
  theme(title = element_text(size = 12)) +
  scale_shape_manual(values=c(1:7)) + 
  theme(axis.text.x = element_text(size = 8, angle = -60, hjust = -0.1)) + 
  theme(axis.text.y = element_text(size = 8)) + 
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12)) + 
  labs(x = "\nPeriod", y = "% of citations received\n") +  
  #Background modifications
  theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
  theme(panel.grid.major = element_line(size = 0.1, colour = "grey")) +
  geom_point(aes(color = CITED), alpha = 7/10, size = 0.8) +
  #Legend modifications
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.key = element_rect(fill = "White")) +
  guides(colour = guide_legend(override.aes = list(size = 1)))
#Color modifications
scale_color_brewer(palette = "Paired")


