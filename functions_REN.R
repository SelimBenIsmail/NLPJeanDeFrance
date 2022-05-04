## Setting ##
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
source("functions_Seg.R")

## Input ##
dataImported <- scan(file = "./sources/extrait_p37_p57.txt", what = "string")
text <- dataImported
df_rentes = data.frame()

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)
renteExtract(text)

## Reconnaissance d'entités nommées ##

regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
#text <- df_rentes$result[1]
r <- NULL


for (i in df_rentes$result)
{
  r <- c(r,str_extract_all(i, regex))
  
}