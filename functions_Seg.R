####function to extract a list of rent contain in a text ####
renteExtract <- function(text){
  regex <- "\\-"
  tirets <- grep(regex, text, value=FALSE)
  text <-  text[-tirets]
  regex <- "[0-9]{2,}\\."
  numRente <-  str_subset(text,regex)
  indexRente <- grep(regex,text,value=FALSE)
  result <- NULL
  for (j in indexRente){
    sentence <- ""
    beg <- j+1
    end <- (indexRente[(which(indexRente==j))+1])-1
    if(is.na(end)) {
      end <- length(text)
    }
    for (i in text[beg:end]) {
      sentence <- str_c(sentence,i," ")
    }
    result <- c(result,sentence)
  }
  df = data.frame(numRente,result)
  
#Separation entre les num de rentes succecives et les num de rentes succecives sur une meme connetablie
  if(nrow(df)>0){
    for(i in 1:nrow(df)){
      df$numRente[i] <- str_replace(df$numRente[i],str_c(as.character(count_connetablie),"\\."),str_c("\\.",as.character(count_connetablie)))
      count_connetablie <<- count_connetablie + 1
    }
  }
  df_rentes <<- rbind(df_rentes, df)#df cumulant toutes les rentes. A affecter à un dataframe global
  return(df)
}

#### capture du rang des voies ####
rdvExtract <- function (text,rang){
  regex <- "^[AB]$"
  indexRdV <- grep(regex,text,value=FALSE)

  #suppression des faux indexes
  v_remove_A <- NULL
  for(i in indexRdV){
    if(text[i] ==  "A" && !str_detect(text[i+1],"[0-9]{2,}\\.")){
      v_remove_A <- c(v_remove_A,which(indexRdV==i))
    }
  }
  if(!is.null(v_remove_A)){
    indexRdV <- indexRdV[-v_remove_A]
  }
  
  RdV <- text[indexRdV]
  result <- NULL
  for (j in indexRdV){
    section <- NULL
    beg <- j+1
    end <- (indexRdV[(which(indexRdV==j))+1])-1
    if(is.na(end)) {
      end <- length(text)
    }
    for (i in text[beg:end]) {
      section <- str_c(section,i," ")
    }
    result <- c(result,section)
  }
  
  df = data.frame()
  df_rdv = data.frame(RdV, result) #dataframe contenant la section pour chaque Rang de voie
  for (i in 1:nrow(df_rdv)) {
    t  <- renteExtract(unlist(str_split(df_rdv$result[i], " ")))
    for (j in 1:nrow(t)) {
      df <- rbind(df, c(df_rdv$RdV[i],t$numRente[j], t$result[j]))
    }
  }
  return(df)
}



#### capture des connetablies ####
connetablieExtract <- function(text){
  regex <- "[0-9]+°"
  indexConnetablie <- grep(regex,text,value=FALSE)
  
  #fusion des elements "bis" du vecteur au numero de connetablie
  v_remove <- NULL
  for(i in indexConnetablie){ 
    if(text[i+1]=="bis"){
      text[i] <- str_c(text[i],"bis", sep=" ")
      v_remove <- c(v_remove,i+1)
    }
  }
  if(!is.null(v_remove)){
    text <- text[-v_remove]
  }
  indexConnetablie <- grep(regex,text,value=FALSE)
  
  numConnetablie <- grep(regex,text,value=TRUE)
  regex <- "^[AB]$"
  indexRdV <- grep(regex,text,value=FALSE)
  #suppression des faux indexes
  v_remove <- NULL
  for(i in indexRdV){
    if(text[i] ==  "A" && !str_detect(text[i+1],"[0-9]{2,}\\.")){
      v_remove <- c(v_remove,which(indexRdV==i))
    }
  }
  if(!is.null(v_remove)){
    indexRdV <- indexRdV[-v_remove]
  }
  v_connetablie <- NULL
  v_section <- NULL
  
  #caputure de la définition de chaque connétablie
  for (j in indexConnetablie){
    connetablie <- NULL
    RdVMark <- which(indexRdV >= j)[1]
    beg <- j+1 
    end <- indexRdV[RdVMark]
    
    if(is.na(end)) {
      end <- length(text)
    }
    if(!is.na(end)) {
      i <- text[beg]
      while (beg != end && !str_detect(i, "[0-9]{2,}\\." )) {
        connetablie <- str_c(connetablie,i," ")
        beg <- beg +1
        i <- text[beg]
      }
      v_connetablie <- c(v_connetablie,connetablie)
    }
  }
  
  #Capture de la section de chaque connetablie
  for (j in indexConnetablie){
    section <- NULL
    beg <- j+1 
    end <- (indexConnetablie[which(indexConnetablie==j)+1])-1
    
    if(is.na(end)) {
      end <- length(text)
    }
    if(!is.na(end)) {
      for (i in text[beg:end]) {
        section <- str_c(section,i," ")
      }
      v_section <- c(v_section,section)
    }
  }
  
  ## Donnees en sous forme de Dataframe ##
  if(length(numConnetablie) == length(v_connetablie)+1){
    df_connetablie =  data.frame(numConnetablie[1:length(numConnetablie)-1],v_connetablie[1:length(numConnetablie)-1] ,v_section[1:length(numConnetablie)-1])
  } else df_connetablie =  data.frame(numConnetablie, v_connetablie, v_section)
  names(df_connetablie)[1:3] <- c("numConnetablie", "connetablie", "section")
  df_connetablies <<- rbind(df_connetablies,df_connetablie[1:2])#df cumulant toutes les connetablie. dataframe global
  
  ## extraction des rangs de voie  pour chaque connetablie ##
  df = data.frame(numConnetablie <- NULL, connetablie <- NULL, rdv <- NULL, numRente <- NULL,rente <- NULL)
  for (i in 1:nrow(df_connetablie)) {
    count_connetablie <<- 1
    #cas le numero de connetablie comporte une particule bis
    if(str_detect(df_connetablie$numConnetablie[i],"bis")){
      num <- str_extract(df_connetablie$numConnetablie[i],"\\d+°")
      count_connetablie <<- length(str_subset(df[,1],num))+1
    }
    #cas ou  il n y a pas de rang de voie A
    if(!str_detect(df_connetablie$section[i],"A [0-9]{2,}\\.")){
      t  <- renteExtract(unlist(str_split(df_connetablie$section[i], " ")))
      rdvNA <- NA
      rdvNA[1:nrow(t)] <- NA 
      t <- cbind(rdvNA,t)
    }else {
      t  <- rdvExtract(unlist(str_split(df_connetablie$section[i], " ")))
    } 
    for (j in 1:nrow(t)) {
      df <- rbind(df, c(df_connetablie$numConnetablie[i],df_connetablie$connetablie[i],t[j,1], t[j,2],t[j,3]))
    }
  }
  return(df)
}


#### capture des escroetes ####
escroeteExtract <- function(text){
  regex <- "[IVXLCM]+[1-9]"
  indexEscroete <- grep(regex,text,value=FALSE)
  numEscroete <- grep(regex,text,value=TRUE)
  regex <- "[1-9]+°"
  indexConnetablie <- grep(regex,text,value=FALSE)
  v_escroete <- NULL
  v_section <- NULL
  for (j in indexEscroete){
    
    escroete <- NULL
    connetablieMark <- which(indexConnetablie >= j)[1]
    
    beg <- j+1 
    end <- indexConnetablie[connetablieMark]-1
    
    for (i in text[beg:end]) {
      escroete <- str_c(escroete,i," ")
    }
    v_escroete <- c(v_escroete,escroete)
  }
  
  for (j in indexEscroete){
    section <- NULL
    beg <- j+1 
    end <- (indexEscroete[which(indexEscroete==j)+1])-1
    if(is.na(end)) {
      end <- length(text)
    }
    for (i in text[beg:end]) {
      section <- str_c(section,i," ")
    }
    v_section <- c(v_section,section)
  }
  ## Donnees en sous forme de Dataframe ##
  df_escroete = data.frame(numEscroete,v_escroete,v_section)
  names(df_escroete)[1:3] <- c("numEscroete", "escroete", "section")
  df_escroetes <<- rbind(df_escroetes,df_escroete[1:2])#df cumulant toutes les connetablie. dataframe global
  
  ## extraction des connetablies pour chaque escroete##
  df = data.frame()
  for (i in 1:nrow(df_escroete)) {
    t  <- connetablieExtract(unlist(str_split(df_escroete$section[i], " ")))
    for (j in 1:nrow(t)) {
      df <- rbind(df, c(df_escroete$numEscroete[i],df_escroete$escroete[i],t[j,1], t[j,2],t[j,3],t[j,4],t[j,5]))
    }
  }
  return(df)
}

#### extraction complete ####
fullExtract <- function(text){
  return (escroeteExtract(text))
}
