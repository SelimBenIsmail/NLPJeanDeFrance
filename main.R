#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
source("./segmentation/functions_Seg.R")

#### Execution ####
dataImported <- scan(file = "./sources/extrait_p37_p57.txt", what = "string")
source("./segmentation/segmentation.r")
