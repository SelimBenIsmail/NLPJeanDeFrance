#### Extraction des relations ####
#df_en = data.frame()
df_links = data.frame()
unknow_node <- 0 
rentes <- df_rentes$result[1:nrow(df_rentes)]# !!! nombre de rentes traitées !!!
#for(i in df_rentes$result[1:nrow(df_rentes)]){
for(i in rentes){ 
  text <- i
  #### suppression des ki furent ####
  remove <- c("ki furent", "ki fu")
  for(i in remove){
    regex <- str_c(i," ",regex_anthroponyme)
    text <- str_remove(text,regex)
  }
  
  #### division en souschaines ####
  aim <- c("Si sient","Si siet","ki sient", "ki iet")
  for (i in aim) {
    if(str_detect(text,i)){
      loc <- str_locate(text,i)
      substring1 <- str_sub(text,0,loc[1,1]-1)
      substring2 <-  str_sub(text,loc[1,2]+1,str_length(text))
      A <- ren_extract(substring1)
      #cas ou aucune EN est  detectée dans la premiere substring
      if(length(A)==0){
        A[1] <- str_c("ukn",unknow_node)
        unknow_node <-  unknow_node +1
      }
      B <- ren_extract(substring2)
      #### dataframe des relations ####
      for(i in 1:length(B)){
        df_links <- rbind(df_links, c(A,B[i]))
      }
    }
  }
}
colnames(df_links) <- c("From", "To")
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

