#THE SUPERIORITY OF ECONOMISTS#
## Journal of Economic Perspectives 2015 ##
## Fourcade, Ollion, Algan ##


###GENDER AND DISCIPLINES (Figure 1)###
The data comes from the US National Center for Education Statistics, Integrated Post-secondary Education Data System Completion Survey (1966-2011). 
The data is available at: http://www.asanet.org/research/stats/social_science_degrees_by_gender_since_1966.cfm
> See degrees&gender.xlsx


###CROSS CITATIONS BETWEEN DISCIPLINARY JOURNALS (Table 1)###
The data used in figure 1 was calculated through a count of citations in one disciplinary journal (American Economic Review, American Political Science Review and American Sociological Review, respectively AER, APSR and ASR hereafter) to one of the first twenty-five journals in each of the three disciplines. 

Due to copyright laws, we could not make the initial dataset available. It was compiled following the procedure. 
1. we extracted all informations about articles from the ISI’s Web of Social Science (make sure to include "reference list" to the data requested). 
2. We reshaped the information to transform it into a dataframe
3. After a rapid trimming in each reference (to only retain the publication outlet), we looked for patterns to extract references to journals of interest (Regular expression patterns are detailed in the appendix and in the Patterns_econ_journals.csv file). 
4. Finally calculated the presence of each of the 25 journals in the reference list of the 3 flagship journals.

### INTEREST IN INTERDISCIPLINARY EXCHANGE ###
The data were collected by Neil Gross for his research on the American professoriate. The survey was conducted in 2006. Neil Gross kindly shared the relevant data with us, but the dataset cannot be made accessible for matters of individual privacy.

### CURRENT AFFILIATION & INSTITUTION OF PHD (2003-2012 )###
The data on 3 journals edited from a single-university (AJS and JPE at the University of Chicago, QJE at Harvard) was collected from the journal’s websites and articles. When not mentioned on the article, the information was collected via vitas, webpages, and other disciplinary sources (AEA directories, etc.).
> The data is available in the Affil_PhD_Journals_2003-12.csv file.

### DISCIPLINARY ORGANIZATION (FIGURE 2 )###
We collected the list of the non-appointed members of the US main disciplinary organization for 2013, and subsequently coded them according to the ranking of their university (US News & World report ranking, 2013). 
> See Org-Affiliations.csv

### EXTRA-DISCIPLINARY CITATIONS (FIGURE 3) ###
The original data was collected through the electronic ISI's Web of Social Sciences database, which gathers information about articles and their respective reference list (cf. details above). 

Focusing on the outlet of publication (book, journal, proceedings), we used pattern-detection techniques (regular expressions) to aggregate these references according to their publication title. By so doing, we reduced the total number of references (just below 6,000,000 references) to a list of 100,000 unique references (see appendix for details). 
> The coding sheet is available in the JPE-Coding_Patterns.csv spreadsheet.  
> The Base5j_Disj_Redux.csv spreadsheet has detailed information about each article, along with the number of citations to 25 disciplines we coded.

### INTER & INTRA-DISCIPLINARY CITATIONS (FIGURE 4) ###
The crossrefs.csv file features different information regarding citation between and within 7 journals (AER, QJE, EJ, JPE, RES, JF & Econometrica), for each year from 1900 to 2010. 
The data was obtained through the ISI's Web of Science and compiled using Perl Compatible regular expressions.

### AFFILIATION OF AMERICAN ECONOMIC REVIEW AUTHORS (in text and appendix) ###
The data on the American Economic Review was collected from the journal’s websites and from the articles. When not mentioned in the paper, the information was collected via vitas, web pages, and other disciplinary sources (AEA directories, handbook of economists). 

The coding was made according to self-declared affiliation, as indicated in the articles. When authors mentioned several affiliations (a trait that keeps increasing over time), we adopted the following procedure: if there was a clear order, we opted for the first institution. Otherwise, we gave priority to “economics department” when mentioned equally with any another institution.
> see AER_AFFIL_3_PERIODS.csv

### MEDIAN AND TOP 10% WAGES IN SELECTED DISCIPLINES (FIGURE 5) ###
The data is freely downloadable form the BLS website
See > http://www.bls.gov/oes /tables.htm 





