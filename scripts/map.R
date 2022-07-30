require(ggmap)
library(ggraph)
library(igraph)
library(dplyr)
library(tidygeocoder)
library(readxl)
library(RColorBrewer)

#### Import ####
load("./export/dataPostIgraph.RData")
nets <- read.csv("./sources/porte1_nets.csv",header = TRUE, sep = ";")
nodes <- read.csv("./sources/porte1_nodes.csv",header = TRUE, sep = ";")
corresp_conn <- read_excel("./sources/correspondance_connetablie.xlsx")

bbox = c(left = 3.0620, bottom = 50.3625, right = 3.0950, top = 50.3820)
p = get_stamenmap(bbox, zoom=16, source = "stamen", maptype ="watercolor")

#### Obtention des coordonnees ####
geo_conn <- corresp_conn %>%
  geocode(correspondance, method = 'osm', lat = lat , long = lng)

load("./export/dataPostGeocode.RData")

df_conn_nodes <-  unique(df_main[c(4,3,2,1)])
row.names(df_conn_nodes) <-  NULL 
#correction de l'espace blanc#
for(i in 1:length(df_conn_nodes$connetablie)){
  df_conn_nodes$connetablie[i] <- str_remove(df_conn_nodes$connetablie[i]," $")
}
df_conn_nodes <- inner_join(geo_conn,df_conn_nodes)

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



####  palette de couleurs ####
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
    "#f1c40f"
    )

#### map ploting ####
ggmap(p, base_layer = ggraph(portes)) +
  geom_point(aes(lng,lat), color = 'red', size= 3)+
  geom_edge_link(aes(x = From_lng, y = From_lat, xend = To_lng, yend = To_lat ), color = 'red', width = 1) +
  geom_node_point(data=df_conn_nodes, aes(lng, lat, colour = numEscroete ), size = 2)+
  scale_color_manual(values = myColors)



