#### settings ####
load("./export/dataPostRen.RData")
source("./scripts/functions_REN.R")
#### Extraction des relations ####
df_links = data.frame()
df_nodes =data.frame(anthroponyme = NULL, numEscroete = NULL, nulConnetablie = NULL, rdv = NULL, numRente = NULL)
unknow_node <- 0 
rentes <- df_main$rente[1:length(df_main$rente)]# !!! nombre de rentes traitees !!!

for(i in 1:length(rentes)){ 
  if(!is.na(rentes[i])){
    text_rente <- rentes[i]
    regex_remove <-"ki fu(rent)? ((femm?e )|((le )?maistre )|((le )?vallés ))?"
    regex <- str_c(regex_remove,regex_anthroponyme_caps)
    text_rente <- str_remove(text_rente,regex)
  
    #### division en souschaines ####
    regex_sep <- "(S|s|K|k)i s?ien?t" #"Si sient","Si siet","ki sient", "ki iet", "ki siet"
    if(str_detect(text_rente,regex_sep)){
      loc <- str_locate(text_rente,regex_sep)
      substring1 <- str_sub(text_rente,0,loc[1,1]-1)
      substring2 <-  str_sub(text_rente,loc[1,2]+1,str_length(text_rente))
      A <- ren_extract_caps(substring1, first = FALSE)
      #cas ou aucune EN est  detectee dans la premiere substring
      if(length(A)==0){
        A[1] <- str_c("ukn",unknow_node)
        unknow_node <-  unknow_node +1
      }
      B <- ren_extract_caps(substring2)
      #### dataframe des aretes ####
      for(k in 1:length(B)){
        df_links <- rbind(df_links, c(A[1],B[k]))
      }
      #### dataframe des noeuds ####
      # if (!is_empty(df_nodes)) {
      #   if(sum(df_nodes[,1] == A[1]) != 0) { #evite les doublons dans le df_nodes
      #     A[1] <- str_c(A[1],df_main$numRente[i])
      #   }
      # }
      v_nodes <- c(A[1],df_main$numEscroete[i],df_main$numConnetablie[i],df_main$rdv[i],df_main$numRente[i])
      df_nodes <- rbind(df_nodes,v_nodes)
    }
  }
}
colnames(df_links) <- c("From", "To")
colnames(df_nodes) <- c("Nom", "Num escroete", "Num connetablie", "Rang", "Num rente")

#### suppression des rentes qui point vers elle mêmes ####
ret <- NULL
for(i in 1:nrow(df_links)){
  if(!is.na(df_links$From[i]) && !is.na(df_links$To[i])){
    if(df_links$From[i] == df_links$To[i]){
      ret <-  c(ret,i)
    }
  }
}
df_links <- df_links[-ret,]
#### suppression des doublons 'simple' ####
df_links <-  distinct(df_links)

#### suppression des liens reciproques ####
ret <- NULL
for(i in 1:nrow(df_links)){
  df_links[i,] <- c(df_links$To[i],df_links$From[i])
  if(filter(df_links, From == df_links$From[i] & To == df_links$To[i]) %>% nrow() >1){
    ret <-  c(ret, i)
  }
}
df_links <- df_links[-ret,]
df_links <- df_links[!is.na(df_links$From),]

#source("./Modelisation_des_graphes/graphes_gen.R")
