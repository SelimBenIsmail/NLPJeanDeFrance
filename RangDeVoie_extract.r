setwd(dir="~/Rprojects/R_db_test")
library(stringr)
library(plyr)
source("functions_seg.R")
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture du rang des voies ##
regex <- "^[AB]$"
indexRdV <- grep(regex,text,value=FALSE)
RdV <- grep(regex,text,value=TRUE)
df = data.frame("rdv" = NULL, "numRente" = NULL, "rente" = NULL)
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


 
