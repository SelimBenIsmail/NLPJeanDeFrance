setwd(dir="~/Rprojects/R_db_test")
library(stringr)
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture des connetablies ##
regex <- "[1-9]+°"
connetablieNum <- grep(regex,text,value=FALSE)
regex <- "^[AB]$"
RdVNum <- grep(regex,text,value=FALSE)
result <- NULL
for (j in connetablieNum){
  connetablie <- NULL
  RdVMark <- which(RdVNum >= j)[1]
  
  beg <- j+1 
  end <- RdVNum[RdVMark]-1
  
  if(!is.na(end)) {
    for (i in text[beg:end]) {
      connetablie <- str_c(connetablie,i," ")
    }
    result <- c(result,connetablie)
  }
  
}

print (result)

