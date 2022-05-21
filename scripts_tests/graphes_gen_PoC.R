library(igraph)
net <- graph_from_data_frame(d=df_links, directed=FALSE) 
net
plot(net)
