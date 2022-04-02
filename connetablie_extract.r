setwd(dir="~/Rprojects/R_db_test")
library(stringr)
source("functions_seg.R")
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture des connetablies ##
regex <- "[1-9]+°"
numConnetablie <- grep(regex,text,value=TRUE)
indexConnetablie <- grep(regex,text,value=FALSE)
regex <- "^[AB]$"
indexRdV <- grep(regex,text,value=FALSE)
result <- NULL
for (j in indexConnetablie){
  connetablie <- NULL
  RdVMark <- which(indexRdV >= j)[1]
  
  beg <- j+1 
  end <- indexRdV[RdVMark]-1
  
  if(!is.na(end)) {
    for (i in text[beg:end]) {
      connetablie <- str_c(connetablie,i," ")
    }
    result <- c(result,connetablie)
  }
}
## Données en sous forme de Dataframe ##
if(length(numConnetablie == length(result)+1)){
  numConnetablie= numConnetablie[-length(numConnetablie)]
}
  
df_connetablie =  data.frame(numConnetablie,result)

