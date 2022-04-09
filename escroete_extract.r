setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
source("functions_seg.R")
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture des escroetes ##
regex <- "[IVXLCM]+[1-9]"
indexEscroete <- grep(regex,text,value=FALSE)
numEscroete <- grep(regex,text,value=TRUE)
regex <- "[1-9]+°"
indexConnetablie <- grep(regex,text,value=FALSE)
v_escroete <- NULL
v_section <- NULL
for (j in indexEscroete){
  
  escroete <- NULL
  connetablieMark <- which(indexConnetablie >= j)[1]
  
  beg <- j+1 
  end <- indexConnetablie[connetablieMark]-1
  
  for (i in text[beg:end]) {
    escroete <- str_c(escroete,i," ")
  }
  v_escroete <- c(v_escroete,escroete)
}

for (j in indexEscroete){
  section <- NULL
  beg <- j+1 
  end <- (indexEscroete[which(indexEscroete==j)+1])-1
  if(is.na(end)) {
    end <- length(text)
  }
  for (i in text[beg:end]) {
    section <- str_c(section,i," ")
  }
  v_section <- c(v_section,section)
}
df_escroete = data.frame(numEscroete,v_escroete,v_section)



