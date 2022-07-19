#### Setting ####
setwd(dir="~/Rprojects/NLPJeanDeFrance")
library(stringr)
library(tidyverse)
library(ggplot2)
library(igraph)
library(dplyr)
library(RColorBrewer)
csv <-  read.csv(file = "./sources/df_closeDistances_csv.csv",header = TRUE, sep=";")
csv[csv$Test_group_name=="Anthro",1] <- "Anthroponyme"
csv[csv$Test_group_name=="Anthro_md",1] <- "Anthroponyme pm"
csv[csv$Test_group_name=="Prenom",1] <- "Nom de bapteme"
csv[csv$Test_group_name=="Prenom_md",1] <- "Nom de bapteme pm"
csv[csv$Test_group_name=="Patro",1] <- "Patronyme"
csv[csv$Test_group_name=="Patro_md",1] <- "Patronyme pm"
csv[csv$Distance=="1,25",4] <- "1.25"
csv[csv$Distance=="2,25",4] <- "2.25"
csv[csv$Distance=="2,5",4] <- "2.5"
csv$Distance <- as.numeric(csv$Distance)

#### préparation des données ####

d <- subset(csv,Correct =="VRAI" & Test_group_name != "Anthroponyme pm" )
e <- subset(csv,Correct =="FAUX" & Test_group_name != "Anthroponyme pm")
f <- subset(csv,Correct =="VRAI" & Test_group_name == "Anthroponyme pm")
h <- subset(csv,Correct =="FAUX" & Test_group_name == "Anthroponyme pm")

df2 <- subset(csv,Correct =="FAUX" & Test_group_name == "Patronyme" )
df1 <- csv
df2 <- df2 %>%
  group_by(Test_group_name) %>%
  arrange(Test_group_name, Distance) %>%
  mutate(sumrow = sum(Correct=="FAUX")/nrow(df2)) 

for (i  in vector) {
  
}

df2 <-  df2 %>%
filter(Distance == 2 ) %>%
select(sumrow) %>%
mutate(sumrow = sumrow +(sum(df2$Distance<=2)/sum(df2$Distance==2)))

#### compilation du graphique #####
q <- ggplot(df2) +
  geom_col(aes(x = Distance, y = sumrow, fill = Test_group_name) )+
  labs( x= "Distance Damerau-Levensthein",y ="Cumul des groupes éronnés")+
  theme_classic()

#### compilation du graphique #####
g <- ggplot(d)+ 
  geom_dotplot(data =d , aes(y=Distance, x=Test_group_name, fill=(Test_group_name)),
               binaxis='y', stackdir='center', binwidth = 0.07, stackratio=0.6, 
               position=ggstance::position_dodgev(height=0.5), dotsize = 1.2,
               width = 1, show.legend = FALSE )+
  geom_dotplot(data = e,aes(y=(Distance), x=Test_group_name, col= Test_group_name), fill="black",
               inherit.aes= FALSE,binaxis='y', stackdir='center', binwidth = 0.07, stackratio=0.6, 
               position=ggstance::position_dodgev(height=0.5),dotsize = 1.2,
               width = 1, show.legend = FALSE)+
  geom_dotplot(data =f , aes(y=Distance, x=Test_group_name),fill="red",
               binaxis='y', stackdir='center', binwidth = 0.07, stackratio=0.6, 
               position=ggstance::position_dodgev(height=0.5),dotsize = 1.2,
               width = 1 )+
  geom_dotplot(data = h,aes(y=(Distance), x=Test_group_name), fill="black",col= "red",
               inherit.aes= FALSE,binaxis='y', stackdir='center', binwidth = 0.07, stackratio=0.6, 
               position=ggstance::position_dodgev(height=0.5),dotsize = 1.2,
               width = 1)+
  labs( x= NULL,y ="Distance Damerau-Levensthein",col = NULL)+
  theme_classic()+
  coord_flip()


plot(q)
