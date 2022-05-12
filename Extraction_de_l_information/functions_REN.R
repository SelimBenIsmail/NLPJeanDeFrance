##### libraries #####
library(stringr)

#### Methode pour capturer des EN ####

ren_extract <- function(text, first = FALSE){
  regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
  if (first){
    str_extract(text,regex)
  } else {
    str_extract_all(text,regex)[[1]]
  }
}

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