#### Detection des anthroponymes #####
l_anthroponymes <- NULL
regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
for (i in df_rentes$result)
{
  l_anthroponymes <- c(l_anthroponymes,str_extract_all(i, regex)[[1]])
}
l_anthroponymes <- unique(l_anthroponymes)

#### Calcul de la distance Damerau-Levenshtein  ####
distance <- NULL
dim <- 10
clustring_lim <- 4

for(i in l_anthroponymes[1:dim]){
  for(j in l_anthroponymes[1:dim]){
    distance <- c(distance,DamerauLevenshtein_mod(i,j))
  }
}
m_distance = matrix(distance,nrow = dim,ncol = dim, byrow = TRUE)
#heatmap(m_distance,Rowv = NA, Colv = NA)
m_closeValues <- which(m_distance <= clustring_lim & m_distance !=0  ,arr.ind = TRUE)
m_closeValues <- m_closeValues[1:(nrow(m_closeValues)/2),] #suppression des valeurs doubles (matrice symetrique)
if(!is.null((m_closeValues))){
  if (is.null(nrow(m_closeValues))) {
    cat(c("La distance Damerau-Levenshtein entre ", l_anthroponymes[m_closeValues[1]], "et ", l_anthroponymes[m_closeValues[2]]," est de " ,m_distance[m_closeValues[1],m_closeValues[2]], "\n"))
  } else{
    for(i in 1:nrow(m_closeValues)){
      cat(c("La distance Damerau-Levenshtein entre ", l_anthroponymes[m_closeValues[i,1]], "et ", l_anthroponymes[m_closeValues[i,2]]," est de " ,m_distance[m_closeValues[i,1],m_closeValues[i,2]], "\n"))
    }
  }
}else{
  cat("Aucun resultat \n")
}

#### creation table rente-anthroponymes ####
l_rente_anthroponyme = list()
regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
for (i in 1:nrow(df_rentes))
{
  lst_anthronyme <- str_extract_all(df_rentes$result[i], regex)
  lst_nRente <- list(df_rentes$numRente[i],lst_anthronyme)  
  l_rente_anthroponyme <- append(l_rente_anthroponyme,lst_nRente )
}

