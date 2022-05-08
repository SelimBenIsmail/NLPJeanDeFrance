#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
library(comparator)
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
DamerauLevenshtein_mod <- function (deletion = 1, insertion = 1, substitution = 1, transposition = 1, 
          normalize = FALSE, similarity = FALSE, ignore_case = FALSE, 
          use_bytes = FALSE) 
{
  arguments <- c(as.list(environment()))
  arguments$similarity <- similarity
  arguments$distance <- !similarity
  arguments$symmetric <- deletion == insertion
  arguments$tri_inequal <- deletion == insertion & !similarity
  do.call("new", append("DamerauLevenshtein", arguments))
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
    f <- DamerauLevenshtein_mod()(i,j)
    d <- c(d,f)
    #cat(c("La distance entre ",i, " et ", j ," est  de ", f, " \n"))
  }
}
m_distance = matrix(d,nrow = dim,ncol = dim, byrow = TRUE)
print(m_distance)


