#### Detection des anthroponymes #####
l_anthroponymes <- NULL
caps <- NULL
for (i in df_main$rente)
{
  v_anthroponymes <-ren_extract(i)
  k <- i
  for (j in v_anthroponymes) {
    k <- str_replace_all(k,j,str_to_upper(j))
  }
  caps <- c(caps,k)
  l_anthroponymes <- c(l_anthroponymes,v_anthroponymes)
}
l_anthroponymes <- unique(l_anthroponymes)
l_anthroponymes <- l_anthroponymes[!is.na(l_anthroponymes)]
df_main$rente <- caps 
l_anthroponymes <- str_to_upper(l_anthroponymes)

#### calcul de distance ####
#m_distance <- myDamereauLevenstheinDist(l_anthroponymes)

#### clustering ####
#l_cluster <- myClustering(l_anthroponymes,clustering_lim=3,m_distance)
load("./export/l_cluster_corr.RData")


#### Remplacement des variants ####
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

