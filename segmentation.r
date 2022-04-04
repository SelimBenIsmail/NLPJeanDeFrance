setwd(dir="~/Rprojects/R_db_test")
library(stringr)
library(tidyverse)
## Setting ##
dataImported <- scan(file = "extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)

df <- data.frame(numEscroete = NULL,numConnetablie = NULL, numRente = NULL)




