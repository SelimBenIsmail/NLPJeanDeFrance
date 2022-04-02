setwd(dir="~/Rprojects/R_db_test")
library(stringr)
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## Supression des tirets ##
regex <- "\\-"
tirets <- grep(regex, text, value=FALSE)
text <-  text[-tirets]

## capture des numeros de rentes consécutives ##
regex <- "[0-9]+\\."
numRente <-  str_subset(text,regex)

## capture des sections de chaque rentes ##
indexRente <- grep(regex,text,value=FALSE)
result <- NULL
for (j in indexRente){
  sentence <- ""
  beg <- j+1
  end <- (indexRente[(which(indexRente==j))+1])-1
  if(is.na(end)) {
    end <- length(text)
  }
  for (i in text[beg:end]) {
    sentence <- str_c(sentence,i," ")
  }
  result <- c(result,sentence)
  print (result)
}
df_rente = data.frame(numRente,result)





