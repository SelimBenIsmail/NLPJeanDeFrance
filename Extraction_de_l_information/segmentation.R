#### Input ####
text <- dataImported

#### suppression des numéros de pages ####
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

#### segmentation du texte  ####
df_rentes = data.frame()
df_connetablies = data.frame()
df_escroetes = data.frame()

#### variables globales ####
count_connetablie <- NULL


#### main ####
df_main <- fullExtract(text)
names(df_main)[1:7] <- c("numEscroete", "escroete", "numConnetablie", "connetablie", "rdv", "numRente","rente")





