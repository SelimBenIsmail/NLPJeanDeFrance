##### libraries #####
library(stringr)

#### regex ####
regex_anthroponyme <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"

#### Methode pour capturer des EN ####

ren_extract <- function(text, first = FALSE){
  regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
  #regex <- "[:upper:][:lower:]+ "
  
  if (first){
    str_extract(text,regex)
  } else {
    str_extract_all(text,regex)[[1]]
  }
}

#### Methode derivee de Damerau-Levenshtein ####
DamerauLevenshtein_mod <- function(str1,str2){
    distance_modicateur <- 0
    if(str_ends(str1,"S") != str_ends(str2,"S")){
      distance_modicateur <- distance_modicateur - .75
   }
  if((str_detect(str1,"CE") && str_detect(str2,"CHE"))||
     (str_detect(str1,"CI") && str_detect(str2,"CHI"))){
    distance_modicateur <- distance_modicateur - .75
    
  } else if((str_detect(str1,"CHE") && str_detect(str2,"CE"))||
            (str_detect(str1,"CHE") && str_detect(str2,"CI"))){
    distance_modicateur <- distance_modicateur - .75
  }
  
  obj <- new("DamerauLevenshtein",deletion = 1, insertion = 1, substitution = 1.25, transposition = 1)
  return (obj(str1,str2)+distance_modicateur)
}