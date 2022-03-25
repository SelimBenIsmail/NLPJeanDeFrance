setwd(dir="~/Rprojects/R_db_test")
library(stringr)
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture du rang des voies ##
regex <- "^[AB]$"
indexRdV <- grep(regex,text,value=FALSE)

result <- NULL
for (j in indexRdV){
  section <- NULL
  beg <- j+1
  end <- (indexRdV[(which(indexRdV==j))+1])-1
  if(is.na(end)) {
    end <- length(text)
  }
  for (i in text[beg:end]) {
    section <- str_c(section,i," ")
  }
  result <- c(result,section)
}

print (result)

