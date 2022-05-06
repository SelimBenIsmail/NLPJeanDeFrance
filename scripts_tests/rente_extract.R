## Setting ##
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
source("functions_Seg.R")
dataImported <- scan(file = "sources/extrait_p37_p57.txt", what = "string")
text<- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

#function to extract a list of rent contained in a text
  ## Supression des tirets ##
  regex <- "\\-"
  tirets <- grep(regex, text, value=FALSE)
  text <-  text[-tirets]
  ## capture des numeros de rentes consécutives ##
  regex <- "[0-9]{2,}\\."
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
  }
  df= data.frame(numRente,result)
  for(i in 1:nrow(df)){
    
    df$numRente[i] <- str_replace(df$numRente[i],as.character(i),str_c(as.character(i),"."))
  }



