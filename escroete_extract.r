setwd(dir="~/Rprojects/R_db_test")
library(stringr)
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture des escroetes ##
regex <- "[IVXLCM]+[1-9]"
numEscroete <- grep(regex,text,value=FALSE)
regex <- "[1-9]+°"
indexConnetablie <- grep(regex,text,value=FALSE)

result <- NULL
for (j in numEscroete){
  
  escroete <- NULL
  connetablieMark <- which(indexConnetablie >= j)[1]
  
  beg <- j+1 
  end <- indexConnetablie[connetablieMark]-1
  
  for (i in text[beg:end]) {
    escroete <- str_c(escroete,i," ")
  }
  result <- c(result,escroete)
}

df = data.frame(numEscroete,result)

print (result)


