load("./export/dataPostIgraph.Rdata")
library(ggraph)


 ggraph(g) +
  geom_edge_link((aes(colour = "NumEscroete"))) + 
  geom_node_point()
   
# options(stringsAsFactors = TRUE)
# options(stringsAsFactors = FALSE)