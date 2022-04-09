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
## Données en sous forme de Dataframe ##
df_escroete = data.frame(numEscroete,v_escroete,v_section)
names(df_escroete)[1:3] <- c("numEscroete", "escroete", "section")

## extraction des connetablies pour chaque escroete##
df = data.frame()
for (i in 1:nrow(df_escroete)) {
  t  <- connetablieExtract(unlist(str_split(df_escroete$section[i], " ")))
  for (j in 1:nrow(t)) {
    df <- rbind(df, c(df_escroete$numEscroete[i],df_escroete$escroete[i],t[j,1], t[j,2],t[j,3],t[j,4],t[j,5]))
  }
}

names(df)[1:7] <- c("numEscroete", "escroete", "numConnetablie", "connetablie", "rdv", "numRente","rente")

