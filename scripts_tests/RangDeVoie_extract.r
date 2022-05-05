setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(plyr)
source("functions_seg.R")
## Setting ##
dataImported <- scan(file = "sources/extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture du rang des voies ##
regex <- "^[AB]$"
indexRdV <- grep(regex,text,value=FALSE)

#suppression des faux indexes
v_remove <- NULL
for(i in indexRdV){
  if(text[i] ==  "A" && !str_detect(text[i+1],"[0-9]+\\.")){
    v_remove <- c(v_remove,which(indexRdV==i))
  }
}
if(!is.null(v_remove)){
  indexRdV <- indexRdV[-v_remove]
}

RdV <- text[indexRdV]
result <- NULL
for (j in indexRdV){
  section <- NULL
  beg <- j + 1
  end <- (indexRdV[(which(indexRdV == j )) + 1]) - 1
  if(is.na(end)) {
    end <- length(text)
  }
  for (i in text[beg:end]) {
    section <- str_c(section, i, " ")
  }
  result <- c(result,section)
}

df = data.frame("rdv" = NULL, "numRente" = NULL, "rente" = NULL)
df_rdv = data.frame(RdV, result) #dataframe contenant la section pour chaque Rang de voie
df_rentes = data.frame()

for (i in 1:nrow(df_rdv)) {
  t  <- renteExtract(unlist(str_split(df_rdv$result[i], " ")))
  for (j in 1:nrow(t)) {
    df <- rbind(df, c(df_rdv$RdV[i],t$numRente[j], t$result[j]))
  }
  names(df)[1:3] <- c("rdv", "numRente", "rente")
  df_rentes <- rbind(df_rentes, t)
}


 
