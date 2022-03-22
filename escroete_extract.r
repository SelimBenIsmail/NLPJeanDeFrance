setwd(dir="~/Rprojects/R_db_test")
library(stringr)
## Setting ##
dataImported <- scan(file = "sources/extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture des escroetes ##
regex <- "[IVXLCM]+[1-9]"
escroeteNum <- grep(regex,text,value=FALSE)
regex <- "[1-9]+°"
connetablieNum <- grep(regex,text,value=FALSE)

result <- NULL
for (j in escroeteNum){
  
  escroete <- NULL
  connetablieMark <- which(connetablieNum >= j)[1]
  
  beg <- j+1 
  end <- connetablieNum[connetablieMark]-1
  
  for (i in text[beg:end]) {
    escroete <- str_c(escroete,i," ")
  }
  result <- c(result,escroete)
}

print (result)

