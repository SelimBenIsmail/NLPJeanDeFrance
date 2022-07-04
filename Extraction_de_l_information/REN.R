#### Detection des anthroponymes #####
l_anthroponymes <- NULL
regex <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
regex_anthro2 <- "[:upper:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:]+(-[:upper:]+)?){1,3}"
regex_pn <- "[:upper:]+"
for (i in df_rentes$result)
{
  l_anthroponymes <- c(l_anthroponymes,str_extract_all(i, regex)[[1]])
}
l_anthroponymes <- str_to_upper(unique(l_anthroponymes))

#division de anthropoymes en prenom et patronymes en vu du clustering 
l_prenoms <- NULL
l_patronymes <- NULL

for (i in l_anthroponymes)
{
  l_prenoms <- c(l_prenoms,str_extract(i, regex_pn)[[1]])
  l_patronymes <- c(l_patronymes,
                    str_remove(i, regex_pn)[[1]])
}
l_prenoms <- str_to_upper(unique(l_prenoms))
l_patronymes <- str_to_upper(unique(l_patronymes))


#### Calcul de la distance Damerau-Levenshtein  ####
distance <- NULL
dim <- 100
clustering_lim <- 3

for(i in l_anthroponymes[1:dim]){
  for(j in l_anthroponymes[1:dim]){
    distance <- c(distance,DamerauLevenshtein_mod(i,j))
  }
}
m_distance <- matrix(distance,nrow = dim,ncol = dim, byrow = TRUE)
df_closeDistances = data.frame()
m_closeValues <- which(m_distance <= clustering_lim & m_distance !=0  ,arr.ind = TRUE)
if(!is.null((m_closeValues))){
  if (is.null(nrow(m_closeValues))) {
    v_closeDistances <- c(l_anthroponymes[m_closeValues[i,1]],l_anthroponymes[m_closeValues[i,2]],m_distance[m_closeValues[i,1],m_closeValues[i,2]])
    df_closeDistances <- rbind(df_closeDistances,v_closeDistances)  } else{
    for(i in 1:nrow(m_closeValues)){
      v_closeDistances <- c(l_anthroponymes[m_closeValues[i,1]],l_anthroponymes[m_closeValues[i,2]],m_distance[m_closeValues[i,1],m_closeValues[i,2]])
      df_closeDistances <- rbind(df_closeDistances,v_closeDistances)
    }
    names(df_closeDistances)[1:3] <- c("Antrhoponyme_1","Antrhoponyme_2","Distance")
  }
}else{
  cat("Aucun resultat \n")
}







#### Récupération des tables ####
write.csv(x=l_anthroponymes, file="./export/l_anthroponymes.csv", row.names = FALSE)
write.csv(x=l_prenoms, file="./export/l_prenoms.csv", row.names = FALSE)
write.csv(x=df_closeDistances, file="./export/df_closeDistances.csv", row.names = TRUE)