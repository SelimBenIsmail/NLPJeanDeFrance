#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
library(comparator)
library(ggplot2)
source("./Extraction_de_l_information/functions_Seg.R")
source("./Extraction_de_l_information/functions_REN.R")

dataImported <- scan(file = "./sources/extrait_p37_p57.txt", what = "string")
source("./Extraction_de_l_information/segmentation.R")

regex_anthroponyme <- "[:upper:][:lower:]+ (((l[aei']s?|d[euo']l?u?|au?)?){0,2} ?[:upper:][:lower:]+(-[:upper:][:lower:]+)?){1,3}"
df_en = data.frame()
df_links = data.frame()

for(i in df_rentes$result[19:23]){
  
  text <- i
  
  #### suppression des ki furent ####
  remove <- c("ki furent", "ki fu")
  for(i in remove){
    regex <- str_c(i," ",regex_anthroponyme,",")
    text <- str_remove(text,regex)
    
  }
  
  
  #### substring ####
  regex <- c("Si sient","Si siet","ki sient", "ki iet")
  for (i in regex) {
    if(str_detect(text,i)){
      loc <- str_locate(text,i)
      s1 <- str_sub(text,0,loc[1,1]-1)
      s2 <-  str_sub(text,loc[1,2]+1,str_length(text))
    }
  }
  
  
  A <- ren_extract(s1)
  B <- ren_extract(s2)
  
  for(i in 1:length(B)){
    df_links <- rbind(df_links, c(A,B[i]))
  }
  colnames(df_links) <- c("From", "To")
  
}







