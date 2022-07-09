#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
library(comparator)
library(ggplot2)
library(igraph)
library(dplyr)
source("./Extraction_de_l_information/functions_Seg.R")
source("./Extraction_de_l_information/functions_REN.R")

#### Execution ####
#dataImported <- scan(file = "./sources/extrait_p37_p57.txt", what = "string")
dataImported <- scan(file = "./sources/DossierFranceTXT.txt", what = "string")

source("./Extraction_de_l_information/segmentation.R")
#source("./Extraction_de_l_information/REN.R")
# source("./Modelisation_des_graphes/relations.R")
# source("./Modelisation_des_graphes/graphes_gen.R")


#### Récupération des tables ####
write.csv(x=df_main, file="./export/df_main.csv", row.names = TRUE)
write.csv(x=l_anthroponymes, file="./export/l_anthroponymes.csv", row.names = FALSE)
