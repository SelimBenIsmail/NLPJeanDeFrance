#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
library(comparator)
library(ggplot2)
source("./Extraction_de_l_information/functions_Seg.R")
source("./Extraction_de_l_information/functions_REN.R")

#### Execution ####
dataImported <- scan(file = "./sources/extrait_p37_p57.txt", what = "string")
source("./Extraction_de_l_information/segmentation.R")
source("./Extraction_de_l_information/REN.R")