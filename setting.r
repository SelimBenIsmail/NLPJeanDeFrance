setwd(dir="~/Rprojects/R_db_test")
library(stringr)
## Setting ##
dataImported <- scan(file = "sources/extrait_p37_p57.txt", what = "string")
text <- dataImported

## suppression des numéros de pages ##
regex  <- "\\{[0-9]+\\}"
text <- str_remove(text, regex)