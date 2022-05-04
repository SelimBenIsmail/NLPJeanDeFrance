setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
source("functions_seg.R")
## Setting ##
dataImported <- scan(file = "sources/extrait_p37_p57.txt", what = "string")
text <- dataImported
df_rentes = data.frame()

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## capture des connetablies ##
regex <- "[0-9]+°"
indexConnetablie <- grep(regex,text,value=FALSE)
#fusion des elements "bis" du vecteur au numero de connetablie
for(i in indexConnetablie){ 
  if(text[i+1]=="bis"){
    text[i] <- str_c(text[i],"bis", sep=" ")
    text <- text[-(i+1)]
  }
}
numConnetablie <- grep(regex,text,value=TRUE)
regex <- "^[AB]$"
indexRdV <- grep(regex,text,value=FALSE)
v_connetablie <- NULL
v_section <- NULL

for (j in indexConnetablie){
  connetablie <- NULL
  RdVMark <- which(indexRdV >= j)[1]
  
  beg <- j+1 
  end <- indexRdV[RdVMark]-1
  
  if(!is.na(end)) {
    for (i in text[beg:end]) {
      connetablie <- str_c(connetablie,i," ")
    }
    v_connetablie <- c(v_connetablie,connetablie)
  }
}

for (j in indexConnetablie){
  section <- NULL
  beg <- j+1 
  end <- (indexConnetablie[which(indexConnetablie==j)+1])-1
  if(is.na(end)) {
    end <- length(text)
  }
  if(!is.na(end)) {
    for (i in text[beg:end]) {
      section <- str_c(section,i," ")
    }
    v_section <- c(v_section,section)
  }
}

## Données en sous forme de Dataframe ##
if(length(numConnetablie == length(v_connetablie)+1)){
  df_connetablie =  data.frame(numConnetablie[1:length(numConnetablie)-1],v_connetablie ,v_section[1:length(numConnetablie)-1])
} else df_connetablie =  data.frame(numConnetablie, v_connetablie, v_section)
names(df_connetablie)[1:3] <- c("numConnetablie", "connetablie", "section")

## extraction des rangs de voie  pour chaque connetablie##
df = data.frame()
for (i in 1:nrow(df_connetablie)) {
  t  <- rdvExtract(unlist(str_split(df_connetablie$section[i], " ")))
  for (j in 1:nrow(t)) {
    df <- rbind(df, c(df_connetablie$numConnetablie[i],df_connetablie$connetablie[i],t[j,1], t[j,2],t[j,3]))
  }
}


