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

#### Récupération des tables ####
write.csv(x=df_main, file="./export/df_main.csv", row.names = TRUE)
write.csv(x=df_connetablies, file="./export/df_connetablies.csv", row.names = TRUE)
write.csv(x=df_rentes, file="./export/df_rentes.csv", row.names = TRUE)



