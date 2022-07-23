#### settings ####
load("./export/dataPostRen.RData")
source("./scripts/functions_REN.R")
#### Extraction des relations ####
df_links = data.frame()
unknow_node <- 0 
rentes <- df_main

for(i in 1:nrow(rentes)){ 
  A <- NULL
  B <- NULL
  if(!is.na(rentes[i,7])){
    text_rente <- rentes[i,7] # consolidation pour ne pas altérer les donnés du df_main
    
    #suppression des expression "ki fu + anthroponyme"
    regex_remove <- "ki fu(rent)? ((femm?e )|((le )?maistre )|((le )?vallés ))?"
    regex <- str_c(regex_remove,regex_anthroponyme_caps)
    text_rente <- str_remove(text_rente,regex)
    
    #suppression des expression "Si fu + valeur + anthroponyme"
    regex_sifu <- "Si fu.*$"
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
      if (length(B)!=0) {
        for(k in 1:length(B)){
          df_links <- rbind(df_links, c(A[1],B[k],rentes$numEscroete[i]))
        }
      } 
    }
  }
}
colnames(df_links) <- c("From", "To","NumEscroete")

#### Correction des unk_nodes ####
df_uknCorr <- data.frame(
  ukn_node=c("Ukn0","Ukn1","Ukn2","Ukn3","Ukn4","Ukn5","Ukn6","Ukn7","Ukn8","Ukn9","Ukn10","Ukn11","Ukn12","Ukn13","Ukn14"),
  correction=c("LOTIN", "ROBIERT",NA	,"LES 2 SEREURS DES LICES"	,"LE PRESTRE DES CHARTERIERS"	,"GODIN"	,NA	,NA	,"DES CARTERIERS DES MALADES"	,
               "HAMIEL"	,"DANIEL"	,"MARIIEN DE L'EVE","MARIEN","L'OSPITAL DES WES",NA )
)

for (i in 1:nrow(df_uknCorr)) {
  if (!is.na(df_uknCorr$correction[i])) {
    df_links$To[df_links$To == str_c('ukn',i-1)] <- df_uknCorr$correction[i] 
  }
}

#### Corrections supplementaires ####

df_add_rel <- data_frame(From= c("ROBIERT","TENEMENT DES MALADES"	,"DANIEL"	,"MARIIEN DE L'EVE","MARIIEN DE L'EVE","MARIEN"),
                         To= c("ROBIERT DE FIERIN",	"BAUDE L'ARTISIEN",	"JEHAN LE GIERMAIN",	"PIERON DE HASNON",	"MARIEN",	"MARGOT DE MAGNI"),
                         NumEscroete = c("I1","I1","II1","II1","II1","II1")
                         )



df_links <- rbind(df_links,df_add_rel)


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
  df_links[i,] <- c(df_links$To[i],df_links$From[i],df_links$NumEscroete[i])
  if(filter(df_links, From == df_links$From[i] & To == df_links$To[i]) %>% nrow() >1){
    ret <-  c(ret, i)
  }
}
df_links <- df_links[-ret,]
df_links <- df_links[!is.na(df_links$From),]


##### Run graphe ####
#source("./scripts/graphes_gen.R")



