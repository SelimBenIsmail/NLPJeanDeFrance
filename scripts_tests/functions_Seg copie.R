##function to extract a list of rent contain in a text
renteExtract <- function(text){
  regex <- "\\-"
  tirets <- grep(regex, text, value=FALSE)
  text <-  text[-tirets]
  regex <- "[0-9]+\\."
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
  return(df= data.frame(numRente,result))
}

## capture du rang des voies ##
rdvExtract <- function (text){
  regex <- "^[AB]$"
  indexRdV <- grep(regex,text,value=FALSE)
  RdV <- grep(regex,text,value=TRUE)
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
  return(df = data.frame(RdV, result))
}


## capture des connetablies ##
connetablieExtract <- function(text){
  regex <- "[1-9]+°"
  numConnetablie <- grep(regex,text,value=TRUE)
  indexConnetablie <- grep(regex,text,value=FALSE)
  regex <- "^[AB]$"
  indexRdV <- grep(regex,text,value=FALSE)
  result <- NULL
  for (j in indexConnetablie){
    connetablie <- NULL
    RdVMark <- which(indexRdV >= j)[1]
    beg <- j+1 
    end <- indexRdV[RdVMark]-1
    if(!is.na(end)) {
      for (i in text[beg:end]) {
        connetablie <- str_c(connetablie,i," ")
      }
      result <- c(result,connetablie)
    }
  }
  if(length(numConnetablie == length(result)+1)){
    numConnetablie= numConnetablie[-length(numConnetablie)]
  }
  return(df = data.frame(numConnetablie,result))
}


## capture des escroetes ##
escroeteExtract <- function(text){
  regex <- "[IVXLCM]+[1-9]"
  numEscroete <- grep(regex,text,value=FALSE)
  regex <- "[1-9]+°"
  indexConnetablie <- grep(regex,text,value=FALSE)
  result <- NULL
  for (j in numEscroete){
    escroete <- NULL
    connetablieMark <- which(indexConnetablie >= j)[1]
    beg <- j+1 
    end <- indexConnetablie[connetablieMark]-1
    for (i in text[beg:end]) {
      escroete <- str_c(escroete,i," ")
    }
    result <- c(result,escroete)
  }
  return(df = data.frame(numEscroete,result))
}


## Segmentation complete ##

## capture du rang des voies ##
extract <- function (text){
  regex <- "^[AB]$"
  indexRdV <- grep(regex,text,value=FALSE)
  RdV <- grep(regex,text,value=TRUE)
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
  df_rentes = data.frame()
  
  for (i in 1:nrow(df_rdv)) {
    t  <- renteExtract(unlist(str_split(df_rdv$result[i], " ")))
    for (j in 1:nrow(t)) {
      df <- rbind(df, c(df_rdv$RdV[i],t$numRente[j], t$result[j]))
    }
    names(df)[1:3] <- c("rdv", "numRente", "rente")
    df_rentes <- rbind(df_rentes, t)
  }
  
  return(df)
}

