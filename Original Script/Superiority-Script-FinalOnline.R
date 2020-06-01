#THE SUPERIORITY OF ECONOMISTS, 
# Journal of Economic Perspectives 2015
# Fourcade, Ollion, Algan

rm(list=ls())
####Load required libraries
require(ggplot2)
require(plyr)
library(stringr)
library(questionr)
library(reshape)

setwd("PATH_TO_DIRECTORY")

##################################################################################################################################################
#######################################Cross-citations between disciplinary journals (Table 1)###############################################################
rm(list=ls())
journals<-read.csv("DB_NOT_PROVIDED")#See readme.txt for details
dim(journals) # head(journals,1)
addmargins(table(journals$JOURNAL, journals$YEAR))

#Load Regex Patterns for 3*25 journals
patterns<-read.csv("Cocites_journals.csv") #head(patterns)

# Create a generic function that counts the presence of a given pattern
is.cited<-function(citations, pattern){
  vec1<-c(as.matrix(citations[,5:354]))   #Vectorizes all references (from col 5 to col 354)
  vec2<-vec1[!is.na(vec1)]#length(vec2) #Removes NAs
  vec2<-str_trim(vec2) #Remove trailing whitespaces #head(vec2)
  total<-length(vec2) #Compute the length (total number of references)
  cited<-length(grep(pattern, vec2, perl=T))#Determines the number of citations to the given pattern
  ratio<-(cited/total)*100# Calculates the ratio 
  page<-cbind(pattern, cited, total, ratio)# Summarize the results for this pattern
  return(page)
}

#Define patterns of recognition for 75 journals
REGX<- as.character(patterns[,2])

##Apply function to AER database
AER<-journals[journals$JOURNAL == "AMERICAN ECONOMIC REVIEW",]
page<-NULL# Empty cache
RES.AER<-NULL# Create structure

for (i in 1:length(REGX)){
  page<-is.cited(AER, REGX[i])
  RES.AER<-rbind(RES.AER, page)
}

##Apply function to APSR database
APSR<-journals[journals$JOURNAL == "AMERICAN POLITICAL SCIENCE REVIEW",]
dim(APSR)
page<-NULL# Empty cache
RES.APSR<-NULL# Create structure

for (i in 1:length(REGX)){
  page<-is.cited(APSR, REGX[i])
  RES.APSR<-rbind(RES.APSR, page)
}

##Apply function to ASR database
ASR<-journals[journals$JOURNAL == "AMERICAN SOCIOLOGICAL REVIEW",]
dim(ASR)
page<-NULL# Empty cache
RES.ASR<-NULL# Create structure

for (i in 1:length(REGX)){
  page<-is.cited(ASR, REGX[i])
  RES.ASR<-rbind(RES.ASR, page)
}

#Bind together the 3 files
RES_3J<-as.data.frame(rbind(RES.AER, RES.APSR, RES.ASR))

#Add the citing journal in the df
publications<-c(rep("AER", 75), rep("APSR", 75), rep("ASR", 75))
RES_3J<-cbind(publications, RES_3J)

#Add the cited discipline in the df
dim(RES_3J)
head(RES_3J)
RES_3J$DISCIPLINE<-as.character(RES_3J$pattern)
RES_3J$DISCIPLINE<-patterns[match(RES_3J$DISCIPLINE, patterns[,2]),3]

#Add colnames
colnames(RES_3J)<-c("CITING", "CITED", "NUMBER", "TOTAL_JOURNAL", "RATIO_JOURNAL", "DISCIPLINE_CITED")

####Compute freq & percentages per disciplinary journal
RES_3J$NUMBER<-as.numeric(as.character(RES_3J$NUMBER))
RES_3J$RATIO_JOURNAL<-as.numeric(as.character(RES_3J$RATIO_JOURNAL))
cross_cit<-aggregate(RES_3J$NUMBER, by=list(RES_3J$CITING, RES_3J$DISCIPLINE_CITED), sum)
cross_cit<-cbind(cross_cit, ratio=aggregate(RES_3J$RATIO_JOURNAL, by=list(RES_3J$CITING, RES_3J$DISCIPLINE_CITED), sum)$x)

cross_cit

################################################################################################################################################################################Author's Affiliations in several journals  (Section "Getting Published")###########################################################
rm(list=ls())
affil<-read.csv("Affil&Phd-3j-2003-2012.csv", sep=";")
dim(affil)  # head(affil)
addmargins(table(affil$JOURNAL, affil$YEAR))
colnames(affil)

#Create two subsets
current<-affil[,c(2,5,8,11,14,17, 20, 23, 26)]
phd<-affil[,c(2,6,9,12,15,18,21,24,27)]

####Prepare data for: Institution of current affiliation
current$sum<-rowSums(!is.na(current[,2:9])) #Determine number of authors per paper
current2<-melt(current, c("JOURNAL","sum"), na.rm=T)[,c(1,2,4)]
#Create weighting coefficient for co-authored articles (each institution receives 1/n, n being the number of authors in a paper)
current2$sum_inv<-1/current2$sum

#Subset for each journal
current2.AJS<-current2[current2$JOURNAL == "AMERICAN JOURNAL OF SOCIOLOGY",]
current2.JPE<-current2[current2$JOURNAL == "JOURNAL OF POLITICAL ECONOMY",]
current2.QJE<-current2[current2$JOURNAL == "QUARTERLY JOURNAL OF ECONOMICS",]

#Make weights sum to 100 for each journal
current2.AJS$sum_inv <- 100 * current2.AJS$sum_inv / sum(current2.AJS$sum_inv)
current2.JPE$sum_inv <- 100 * current2.JPE$sum_inv / sum(current2.JPE$sum_inv)
current2.QJE$sum_inv <- 100 * current2.QJE$sum_inv / sum(current2.QJE$sum_inv)

head(current2.AJS)

####Prepare data for: Institution of PhD
phd$sum<-NULL
phd$sum<-current$sum #Determine number of authors per paper (some phd data missing)
phd2<-melt(phd, c("JOURNAL","sum"), na.rm= T)[,c(1,2,4)]
#Create weighting coefficient for co-authored articles (each institution receives 1/n, n being the number of authors in a paper)
phd2$sum_inv<-1/phd2$sum

#Subset for each journal
phd2.AJS<-phd2[phd2$JOURNAL == "AMERICAN JOURNAL OF SOCIOLOGY",]
phd2.JPE<-phd2[phd2$JOURNAL == "JOURNAL OF POLITICAL ECONOMY",]
phd2.QJE<-phd2[phd2$JOURNAL == "QUARTERLY JOURNAL OF ECONOMICS",]

#Make weights sum to 100 for each journal
phd2.AJS$sum_inv <- 100 * phd2.AJS$sum_inv / sum(phd2.AJS$sum_inv)
phd2.JPE$sum_inv <- 100 * phd2.JPE$sum_inv / sum(phd2.JPE$sum_inv)
phd2.QJE$sum_inv <- 100 * phd2.QJE$sum_inv / sum(phd2.QJE$sum_inv)

#Values
sum(round(sort(wtd.table(current2.AJS$value, weights=current2.AJS$sum_inv),decreasing=T),2)[1:5])#Sum top 5 current AJS
tail(round(sort(wtd.table(current2.AJS$value, weights=current2.AJS$sum_inv)),2))

sum(round(sort(wtd.table(current2.JPE$value, weights=current2.JPE$sum_inv),decreasing=T),2)[1:5])#Sum top 5 current JPE
tail(round(sort(wtd.table(current2.JPE$value, weights=current2.JPE$sum_inv)),2))	

sum(round(sort(wtd.table(current2.QJE$value, weights=current2.QJE$sum_inv),decreasing=T),2)[1:5])#Sum top 5 current QJE
tail(round(sort(wtd.table(current2.QJE$value, weights=current2.QJE$sum_inv)),2))

sum(sort(wtd.table(phd2.AJS$value, weights=phd2.AJS$sum_inv),decreasing=T)[1:5])#Sum top 5 PhD AJS
tail(round(sort(wtd.table(phd2.AJS$value, weights=phd2.AJS$sum_inv)),2))

sum(sort(wtd.table(phd2.JPE$value, weights=phd2.JPE$sum_inv),decreasing=T)[1:5]) #Sum top 5 PhD JPE
tail(round(sort(wtd.table(phd2.JPE$value, weights=phd2.JPE$sum_inv)),2))

sum(sort(wtd.table(phd2.QJE$value, weights=phd2.QJE$sum_inv),decreasing=T)[1:5])#Sum top 5 PhD QJE
tail(round(sort(wtd.table(phd2.QJE$value, weights=phd2.QJE$sum_inv)),2))

#################################################################################################################################################
###################################################The organization of the disciplines  (Figure 2)###########################################################
rm(list=ls())
orgs<-read.csv("Org-Affiliations.csv")

props.org<-as.data.frame(prop.table(table(orgs$RANK, orgs$ORGANIZATION),2)*100)
colnames(props.org)<-c("Rank", "Organization", "Freq")
props.org$Rank<-factor(props.org$Rank, levels = c("Top 5","Top 6-10","Top 11-20","Top 21-50","Top 51-100","Not ranked"))

p<-ggplot(props.org, aes(x=Rank, y=Freq, fill=Organization))+scale_fill_grey()
p+geom_bar(position="dodge", stat="identity")+ xlab("\n\nRanking\n")+ ylab("Members of the Councils (%)\n\n")+theme_bw() + theme(axis.text.x  = element_text(size=13), legend.text = element_text(size = 12), legend.title = element_text(size=12))


#######################################################################################################################################################
################################################## (When not themselves) Who do economists cite?  (Figure 3)#######################################################
rm(list=ls())
disjunct<-read.csv("Base5j_Disj_Redux.csv")
dim(disjunct)
table(disjunct$JOURNAL, disjunct$YEAR)#This shows that we will need to normalize (by articles/ period)

#Appendix: Evolution of citations
p <- ggplot(disjunct, aes(factor(YEAR), NUMB_REF))
p + geom_boxplot(outlier.size = NA) + ggtitle("Number of references in 5 Top Economics Journals\nAER, QJE, JPE, RES, ECTRCA\n") + theme(plot.title=element_text(face="bold", size=14))+ labs(x = "\nYear", y = "Percentage\n")+ scale_x_discrete(breaks = seq(1900,2012,10), labels = seq(1900,2010,10)) + ylim(0,100)+theme_bw() + stat_summary(fun.y="median", geom="point", shape=23, size=2, fill="brown4")

#Adding variables
#Creates a Two year period variable
disjunct$TYP <- disjunct$YEAR
labs<-paste(seq(1900,2010, by=2), seq(1902,2012, by=2),sep="-")
disjunct$TYP<-cut(disjunct$TYP, breaks=seq(1900,2012, by=2), labels=labs, include.lowest=T)

#Calculate number of citations unaccounted for (DK + OTHERS)
colnames(disjunct)
disjunct$UNACC<-apply(disjunct[,c(9,25)],1,sum)
disjunct$TOTAL_CITES<-apply(disjunct[,c(5:31)], 1, sum)
disjunct$ACCOUNTED<-disjunct$TOTAL_CITES-disjunct$UNACC
disjunct$UNACC_RATE<-(disjunct$UNACC/disjunct$TOTAL_CITES)*100

#Appendix: Evolution of unaccounted citations 
p <- ggplot(disjunct, aes(factor(YEAR), UNACC_RATE))+stat_boxplot(outlier.size = NA)
p+geom_boxplot(outlier.size = NA) + theme(plot.title=element_text(face="bold", size=14))+ labs(x = "\nYear", y = "Percentage\n")+ scale_x_discrete(breaks = seq(1900,2012,10), labels = seq(1900,2010,10)) +theme_bw()  + stat_summary(fun.y="median", geom="point", shape=23, size=2, fill="darkgoldenrod")

#######Standardization. Compute for each article the percentage of each discipline relative to the number of citations accounted for
#Reorder the dataframe for the sake of clarity
colnames(disjunct)
disjunct1<-disjunct[,c(1,2,4,5:8,10:24,26:31,36,33,34)]
colnames(disjunct1)
head(disjunct1,1)
dim(disjunct1)

######Percentage of each discipline (relative to the number of articles *accounted for*), per discipline
#First, aggregate by a period of 2 years
colnames(disjunct1)
disjunct1_agg <- aggregate(disjunct1[, c(4:29)], by = list(disjunct1$TYP, disjunct1$JOURNAL), FUN=sum)

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
q + geom_smooth(aes(group=DISCIPLINE), se=F, span=.4, linetype=m_disjunct_percent$linetype, color="black") + geom_point(aes(shape=DISCIPLINE, size = 3), alpha=7/10)+ ggtitle("\n") + theme(axis.text.x = element_text(size = 6)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "\nPeriod", y = "% of citations\n")+  theme(plot.title=element_text(family="Arial", face="bold", size=14)) + theme(panel.background = element_rect(fill='white', colour='grey')) + scale_shape_manual(values=c("F", "B", "S", "P", "L", "p", "M", "s", "H")) +theme(legend.position="none") 

##################################################################################################################################################
##########################################################     Citations between Econ Journals (appendix) ##################################################
rm(list=ls())
crossrefs<-read.csv("crossrefs.csv")
head(crossrefs)
dim(crossrefs)
#Number of references cited in a given journal, over time
p<-ggplot(crossrefs, aes(x=YEAR, y=TOTAL_REFS, colour=CITING))
p+ ggtitle("Number of references in 7 Economics Journals (1900-2012)\n")+geom_smooth(se = FALSE, span=.3)  + scale_x_continuous(breaks=seq(1900,2010,10)) +theme_bw() + ylab("Number of references\n") + xlab("\nYear")

colnames(crossrefs)

# Normalize by year/ journal
crossrefs2<-cbind(crossrefs[,c(1:2)],sweep(crossrefs[,3:9],1,crossrefs[,11],`/`)*100)
#Reshape
m_crossrefs<-melt(crossrefs2, id=c("CITING", "YEAR"))
colnames(m_crossrefs)<-c("CITING", "YEAR", "CITED", "VALUE")


################################## Figure 4: Citations received from the 6 other journals (Figure 4)
x=seq(1950,2010, by=2)
y=seq(1951,2011, by=2)

file<-NULL

for (i in 1:length(x)){
  TAB<-m_crossrefs[m_crossrefs$YEAR %in% c(x[i]:y[i]),] #dim(TAB)	
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

noself2$linetype[noself2$linetype == "AER"]<-"solid"
noself2$linetype[noself2$linetype == "ECTRA"]<-"dashed"
noself2$linetype[noself2$linetype == "EJ"]<-"dotted"
noself2$linetype[noself2$linetype == "JFIN"]<-"dotdash"
noself2$linetype[noself2$linetype == "JPE"]<-"longdash"
noself2$linetype[noself2$linetype == "QJE"]<-"F1"
noself2$linetype[noself2$linetype == "RES"]<-"12345678"


p<-ggplot(noself2, aes(x=PERIOD, y=VALUE, group=CITED))
p + geom_smooth(aes(group=CITED), se=F, span=.5, linetype=noself2$linetype, color="black") + ggtitle("\n") + scale_shape_manual(values=c(1:7)) + theme(axis.text.x = element_text(size = 6)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "\nPeriod", y = "% of citations received\n")+  theme(plot.title=element_text(family="Arial", face="bold", size=14)) + theme(panel.background = element_rect(fill='white', colour='grey')) +geom_point(aes(shape=CITED), alpha=7/10)



##################################################################################################################################################
##########################################################     Affiliation of AER Authors  (Appendix and text) #########################################
rm(list=ls())
affil_AER<-read.csv("AER_AFFIL_3_PERIODS.csv")
dim(affil_AER) #head(affil_AER)
table(affil_AER$PERIOD)

####Prepare data: number of authors
colnames(affil_AER)
affil_AER2<-affil_AER[,c(3,6,9,12,15,18)]
affil_AER2$sum<-rowSums(!is.na(affil_AER2[,c(2:6)])) #Determine number of authors per paper
m_affil_AER<-melt(affil_AER2, c("PERIOD","sum"), na.rm=T)[,c(1,2,4)]
#addmargins(table(m_affil_AER$sum, m_affil_AER$PERIOD)/5)
#Create weighting coefficient for co-authored articles (each institution receives 1/n, n being the number of authors in a paper)
m_affil_AER$sum_inv<-1/m_affil_AER$sum

#Subset for each journal
tail(m_affil_AER)
fifties<-m_affil_AER[m_affil_AER$PERIOD == "1956-1960",] #dim(fifties)
seventies<-m_affil_AER[m_affil_AER$PERIOD == "1976-1980",] #dim(seventies)
naughts<-m_affil_AER[m_affil_AER$PERIOD == "2004-2008",] #dim(naughts)

#Weighted table (with Unknown)
round(sort(wtd.table(fifties$value, weights=fifties$sum_inv))/1.28,2)
round(sort(wtd.table(seventies$value, weights=seventies$sum_inv))/6.6,2)
round(sort(wtd.table(naughts$value, weights=naughts$sum_inv))/4.55,2)

#Weighted table (Unknown removed from the weights)
a<-sort(wtd.table(fifties$value, weights=fifties$sum_inv))
b<-sort(wtd.table(seventies$value, weights=seventies$sum_inv))
c<-sort(wtd.table(naughts$value, weights=naughts$sum_inv))

#Calculate percentage w/o unknown
round(a[rownames(a) != "Unknown"]/sum(a[rownames(a) != "Unknown"])*100,1) #Fifties
round(b[rownames(b) != "Unknown"]/sum(b[rownames(b) != "Unknown"])*100,1) #Seventies
round(c[rownames(c) != "Unknown"]/sum(c[rownames(c) != "Unknown"])*100,1) #Nineties


