#### Detection des anthroponymes #####
l_anthroponymes <- NULL
caps <- NULL
regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
for (i in df_main$rente)
{
  v_anthroponymes <- str_extract_all(i, regex)[[1]]
  k <- i
  for (j in v_anthroponymes) {
    k <- str_replace_all(k,j,str_to_upper(j))
  }
  caps <- c(caps,k)
  l_anthroponymes <- c(l_anthroponymes,v_anthroponymes)
}
df_main$rente <- caps 

#### Calcul de la distance Damerau-Levenshtein  ####
distance <- NULL
dim <- 100
clustering_lim <- 3

l_anthroponymes <- str_to_upper(unique(l_anthroponymes))
for(i in l_anthroponymes[1:dim]){
  for(j in l_anthroponymes[1:dim]){
    distance <- c(distance,DamerauLevenshtein_mod(i,j))
  }
}
m_distance = matrix(distance,nrow = dim,ncol = dim, byrow = TRUE)

#### clustering ####
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

#### Remplacement des variants ####
temp <- data.frame()
for(i in 1:length(df_main$rente)){
  for (j in l_cluster) {
    for(k in j){
      if(!is.na(df_main$rente[i])){
        if (str_detect(df_main$rente[i],k)) {
          df_main$rente[i] <- str_replace_all(df_main$rente[i],k,j[1])
        }
      }
    }
  }
}

