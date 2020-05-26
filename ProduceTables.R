## Packages needed

loadInstall <- function(x) {
  if (require(x, character.only = T)) return(require(x, character.only = T))
  else {install.packages(x, type="source")}
}

x <- c("plyr","plyr","dplyr","stringr","rms","Hmisc","knitr")

for(i in 1:length(x)){
  loadInstall(x[i])
}

#Bring in Data from github
source()


#Bring in myTable1 function from github
source()



#-----------------------------------------
#  How to specify a test in this function:
#  -----------------------------------------
#  aov.t   is for  "ANOVA test"              
# fisher.t  is for "Fisher exact test"       
# chisq.t  is for "Chi-squared test"        
# t.test is for "T-test"                  
# kruskal.t is for "Kruskal-Wallis test"     
# wilcox.t  is for "Wilcoxon ranked sum test"
#----------------------------------------------


#Now create your table1 with myTable1 function

tt=myTable1(dat=dat1, splitvar="sex",splitlabel ="Gender", 
            contvar=c("age","BP.sys","BP.dia","N.smokePday"), # continuous variables
            contTest=c("t.test","wilcox.t","t.test","aov.t"), # Test to be applied respectively to the contvars   
            catvar=c("diabetic",  "Treatment","Race"),        # Categorical variable
            catTest=c("fisher.t","fisher.t","chisq.t"),       # Test to use for categorical variables
            docaption = T,                                    # Should code do caption for you ? 
            my.docaption="xxxxxxxx",                   # If false, then write caption eg. "Summaries by sex"
            prmsd=c("mean","median","mean","mean"),   # Specify statistics for summaries 
            my.loc="./tabhold/mytable1s1.tex",       # location for tex file
            Trace=F,                                 # Used for my editing
            pdec=2,                                  # Decimal place for p-values
            Test=F,                                  # Test statistic column to be included in table
            latexoutput=T,                           # Whether to spit out tex file
            exceloutput=F,exceloutputName ="ExxcelT1" , # Produce an excel file of Table 1
            showtable = F) 




