##### libraries #####
library(stringr)

#### regex ####
regex_anthroponyme <- "(Mgr )?[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
regex_anthroponyme_caps <- "(MGR )?[:upper:]{2,} (((L[AEI']S?|D[EUO']L?U?|AU?)?){0,2} ?[:upper:]{2,}(-[:upper:]{2,})?){1,3}"

#### Methode pour capturer des EN ####
ren_extract <- function(text, first = FALSE){
  if (first){
    str_extract(text,regex_anthroponyme)
  } else {
    str_extract_all(text,regex_anthroponyme)[[1]]
  }
}

#### Methode pour capturer des EN en lettres capitales ####
ren_extract_caps <- function(text, first = FALSE){
  if (first){
    str_extract(text,regex_anthroponyme_caps)
  } else {
    str_extract_all(text,regex_anthroponyme_caps)[[1]]
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

#### clustering ####
myClustering <- function(l_anthroponymes,clustering_lim,m_distance){
  l_cluster = list()
  for(i in 1:nrow(m_distance)){
    m_distance[i,1:i] <- NA #  pour eviter les doubles detections
    if(length(v_row <-l_anthroponymes[which(m_distance[i,] <= clustering_lim)])){ #distance egale ou inferieure  a la  limite dans la ligne 
      l_cluster[[i]] <- c(l_anthroponymes[i],v_row)
    } else {
      l_cluster[[i]] <- NA
    }  
  }
  l_cluster <- l_cluster[!is.na(l_cluster)]  
  return(l_cluster)
}


#### Calcul de la distance Damerau-Levenshtein  ####
myDamereauLevenstheinDist <- function(v_string){
  distance <- NULL
  dim <- length(v_string)
  for(i in v_string[1:dim]){
    for(j in v_string[1:dim]){
      print(c(i,"  ", j))
      distance <- c(distance,DamerauLevenshtein_mod(i,j))
    }
  }
  m_distance = matrix(distance,nrow = dim,ncol = dim, byrow = TRUE)
  return(m_distance)
}
