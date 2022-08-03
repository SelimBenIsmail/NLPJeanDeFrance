#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
library(comparator)
library(ggplot2)
library(igraph)
library(dplyr)
require(ggmap)
library(ggraph)
library(tidygeocoder)
library(readxl)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(ggforce)
library(concaveman)
library(stringi)

library("RColorBrewer")

source("./scripts/functions_Seg.R")
source("./scripts/functions_REN.R")

#### Execution ####
dataImported <- scan(file = "./sources/DossierFranceTXT.txt", what = "string")

#### setting palette de couleurs ####
myColors = c("#a93226",
             "#2e4053",
             "#d35400",
             "#148f77",
             "#1f618d",
             "#6c3483",
             "#a569bd",
             "#808b96",
             "#e74c3c",
             "#3498db",
             "#2ecc71",
             "#F2F3F4"
)


source("./scripts/segmentation.R")
source("./scripts/REN.R")
source("./scripts/relations.R")
source("./scripts/graphes_gen.R")


#### Récupération des tables ####
#write.csv(x=df_main, file="./export/df_main.csv", row.names = TRUE)
#write.csv(x=l_anthroponymes, file="./export/l_anthroponymes.csv", row.names = FALSE)
