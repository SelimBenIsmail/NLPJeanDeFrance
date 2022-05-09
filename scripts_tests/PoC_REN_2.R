#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
library(comparator)
library(ggplot2)
source("functions_Seg.R")

## Input ##
dataImported <- scan(file = "./sources/extrait_p37_p57.txt", what = "string")
text <- dataImported
df_rentes = data.frame()
## variables globales ##
count_connetablie <- 1

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

renteExtract(text)

#### Methode dérivée de Damerau-Levenshtein ####
DamerauLevenshtein_mod <- function(str1,str2){
  distance_modicateur <- 0
  if(str_ends(str1,"s") != str_ends(str2,"s")){
    distance_modicateur <- distance_modicateur - .75
  }
  if((str_detect(str1,"ce") && str_detect(str2,"che"))||
     (str_detect(str1,"ci") && str_detect(str2,"chi"))){
    distance_modicateur <- distance_modicateur - .75
    
  } else if((str_detect(str1,"che") && str_detect(str2,"ce"))||
            (str_detect(str1,"chi") && str_detect(str2,"ci"))){
    distance_modicateur <- distance_modicateur - .75
  }
  
  obj <- new("DamerauLevenshtein",deletion = 1, insertion = 1, substitution = 1, transposition = 1)
  return (obj(str1,str2)+distance_modicateur)
}

#### Reconnaissance d'entités nommées ####
r <- NULL
regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
for (i in df_rentes$result)
{
  r <- c(r,str_extract_all(i, regex)[[1]])
}
r <- unique(r)

#### Calcul de la distance Damerau-Levenshtein  ####
d <- NULL
dim <- 20
for(i in r[1:dim]){
  for(j in r[1:dim]){
    f <- DamerauLevenshtein_mod(i,j)
    d <- c(d,f)
    #cat(c("La distance entre ",i, " et ", j ," est  de ", f, "\n"))
  }
}
m_distance = matrix(d,nrow = dim,ncol = dim, byrow = TRUE)
print(m_distance)


