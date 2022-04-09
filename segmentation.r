## Setting ##
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
source("functions_Seg.R")

## Input ##
dataImported <- scan(file = "./sources/extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

## segmentation du texte  ##
df_rentes = data.frame()
df_main <- fullExtract(text)
names(df_main)[1:7] <- c("numEscroete", "escroete", "numConnetablie", "connetablie", "rdv", "numRente","rente")

## Récupération des tables ##
#write.csv(x=df, file="Export/df_main.csv", row.names = TRUE)


