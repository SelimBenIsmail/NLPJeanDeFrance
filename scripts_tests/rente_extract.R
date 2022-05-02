## Setting ##
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
source("functions_Seg.R")
dataImported <- scan(file = "sources/extrait_p37_p57.txt", what = "string")
dataText<- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
dataText <- str_remove(dataText, regex)

#function to extract a list of rent contain in a text
renteExtract <-function(text){
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
  
  return(df_rente = data.frame(numRente,result))
}


## main ##
print("Hello World")
df_seg <- renteExtract(dataText)


