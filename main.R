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
library(ggrepel)
library(scales)
library(ggforce)
library(concaveman)
source("./scripts/functions_Seg.R")
source("./scripts/functions_REN.R")

#### Execution ####
dataImported <- scan(file = "./sources/DossierFranceTXT.txt", what = "string")

source("./scripts/segmentation.R")
source("./scripts/REN.R")
source("./scripts/relations.R")
source("./scripts/graphes_gen.R")
source("./scripts/carto.R")
source("./scripts/map.R")

