load("./export/dataPostIgraph.RData")
#### loading libs ####
require(ggmap)
library(ggraph)
library(igraph)
library(dplyr)
library(tidygeocoder)
library(readxl)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(ggforce)
library(concaveman)

#### Import ####
nets <- read.csv("./sources/portes_nets.csv",header = TRUE, sep = ";")
nodes <- read.csv("./sources/portes_nodes.csv",header = TRUE, sep = ";")
corresp_conn <- read_excel("./sources/correspondance_connetablie.xlsx")
df_repere <- read_excel("./sources/coord_histo.xlsx")
df_repere$lat <- as.numeric(df_repere$lat)
df_repere$lng <- as.numeric(df_repere$lng)

bbox = c(left = 3.0620, bottom = 50.3625, right = 3.0950, top = 50.3820)
p = get_stamenmap(bbox, zoom=16, source = "stamen", maptype ="watercolor")

#### Obtention des coordonnees ####
geo_conn <- corresp_conn %>%
  geocode(correspondance, method = 'osm', lat = lat , long = lng)
#correction
corr <- c(50.36646162324021, 3.0847360694545984,50.36792251302541, 3.080996196322546,50.36868284787462, 3.0804070338975666,50.372161921584464, 3.081554536863286)
j <- 1
for (i in c("En le rue Pepin","Ou Pont","En le rue Saint Piere", "En le rue des Bouloires")) {
  geo_conn$lat[geo_conn$connetablie == i] <- corr[j]
  geo_conn$lng[geo_conn$connetablie == i] <- corr[j+1]
    j <- j+2
}

load("./export/dataPostGeoCode.Rdata")

#### df_conn_node from df_main ####
df_conn_nodes <-  unique(df_main[c(4,3,1)])

row.names(df_conn_nodes) <-  NULL 
nbRente <-  NULL

#colonne nbRente
for (i in df_conn_nodes$numConnetablie) {
  count <- df_main$numConnetablie[df_main$numConnetablie == i] %>%  length() 
  nbRente <- c(nbRente, count)
}
df_conn_nodes <- cbind(df_conn_nodes, nbRente)
#correction de l'espace blanc#
for(i in 1:length(df_conn_nodes$connetablie)){
  df_conn_nodes$connetablie[i] <- str_remove(df_conn_nodes$connetablie[i]," $")
}
#jointure
df_conn_nodes <- merge(geo_conn,df_conn_nodes)

#### Graphe des portes ####
names(nets) <- c("porte","porte2")
murs <- inner_join(nets,nodes)
names(murs) <- c("From","porte","From_lat","From_lng")
murs <- inner_join(murs,nodes)
names(murs) <- c("From","porte","From_lat","From_lng","To_lat","To_lng")

portes <- graph_from_data_frame(d=nets,vertices=nodes, directed=FALSE)
V(portes)$lat <- nodes$lat
V(portes)$lng <- nodes$lng
E(portes)$From_lat <- murs$From_lat
E(portes)$From_lng <- murs$From_lng
E(portes)$To_lat <- murs$To_lat
E(portes)$To_lng <- murs$To_lng


#### data ####
for (i in 1:nrow(df_conn_nodes)) {
  df_conn_nodes$numEscroete[i] <- df_conn_nodes$numEscroete[i] %>% 
    str_extract("[IV]+")
  df_conn_nodes$numConnetablie[i] <- df_conn_nodes$numConnetablie[i] %>% 
    str_remove("1$")
}
noms_escroetes <- data.frame(
  numEscroete = c("I","II","III","IV","V","VI","VII"), 
  escroete =c("Markiet","Canteleu","Més","Wés","Nuevile","Deuwioel","Escroete VII"))
df_conn_nodes <- merge(df_conn_nodes,noms_escroetes)
df_conn_nodes <- na.omit(df_conn_nodes)


#### map ploting  connetablie simple ####
ggmap(p, base_layer = ggraph(portes)) +
  geom_point(aes(lng,lat), color = 'red', shape = 18, size= 5)+ #portes
  geom_edge_link(aes(x = From_lng, y = From_lat, xend = To_lng, yend = To_lat ), color = 'red', width = 1) + #murs
  geom_node_point(data=df_conn_nodes, aes(lng, lat, fill = numConnetablie), colour= 'black', shape = 25, size = 2.5) + #connetablies
  geom_node_point(data = df_repere,aes(lng,lat), fill = '#7FB3D5', shape = 21, size= 3, color='black')+ #reperes
  geom_text_repel(data=df_conn_nodes, aes(x=lng, y=lat, label = numConnetablie)) + #label connetablies
  theme(legend.position = "none")

#### map nb rente ####
ggmap(p, base_layer = ggraph(portes)) +
  geom_point(aes(lng,lat), color = 'red', shape = 18, size= 5)+
  geom_edge_link(aes(x = From_lng, y = From_lat, xend = To_lng, yend = To_lat ), color = 'red', width = 1) +
  geom_node_point(data=df_conn_nodes, aes(lng, lat, colour = numConnetablie, size = nbRente ),  shape = 16,alpha = 0.3) +
  geom_node_point(data=df_conn_nodes, aes(lng, lat, fill = numConnetablie), colour= 'black', shape = 25, size = 2.5) +
  scale_size_area(max_size = 25) +
  theme(legend.position = "none") +
  geom_text_repel(data=df_conn_nodes, aes(x=lng, y=lat, label = nbRente))

#### map ploting  Name ####
ggmap(p, base_layer = ggraph(portes)) +
  geom_point(aes(lng,lat), color = 'red', shape = 18, size= 5)+ #portes
  geom_edge_link(aes(x = From_lng, y = From_lat, xend = To_lng, yend = To_lat ), color = 'red', width = 1) + #murs
  geom_node_point(data = df_repere,aes(lng,lat), fill = '#7FB3D5', shape = 21, size= 3, color='black')+ #reperes
  geom_label_repel(aes(x=lng, y=lat, label = name), color='#922B21', size = 2.5, alpha = 0.9) +  #label portes
  geom_label_repel(data =df_repere,aes(x=lng, y=lat, label = nom), color='#1F618D', size = 2.5, alpha = 0.9) + #label reperes
  theme(legend.position = "none")

#### map ploting  Escroete ####
ggmap(p, base_layer = ggraph(portes)) +
  geom_point(aes(lng,lat), color = 'red', shape = 18, size= 5)+ #portes
  geom_edge_link(aes(x = From_lng, y = From_lat, xend = To_lng, yend = To_lat ), color = 'red', width = 1) + #murs
  geom_mark_hull(data = df_conn_nodes, aes(x = lng, y = lat, fill=numEscroete, color=numEscroete, label= escroete),label.width = NULL)+ #contour des escroetes
  geom_node_point(data=df_conn_nodes, aes(lng, lat, fill = numEscroete), colour= 'black', shape = 25, size = 2.5) + #connetablies
  geom_node_point(data = df_repere,aes(lng,lat), fill = '#7FB3D5', shape = 21, size= 3, color='black') + #reperes
  geom_text_repel(data=df_conn_nodes, aes(x=lng, y=lat, label = numConnetablie),size = 2.5) +  #label connetablies
  theme(legend.position = "none")
