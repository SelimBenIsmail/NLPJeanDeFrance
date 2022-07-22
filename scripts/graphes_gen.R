#### objet igraph #####
g <- graph_from_data_frame(d=df_links, directed=FALSE) 
g <- simplify(g,remove.loops = TRUE)
lfr = layout.fruchterman.reingold(g)

#### test couleur ####
#clr <-  brewer.pal(n = 3, name = 'Set1')
df_numEscroete_colors = data.frame(
  numEscroete = unique(df_links$NumEscroete)
  
)




E(g)$color <- clr 
#### plotting #####
plot(g, layout=lfr, 
     vertex.size=3 , 
     vertex.label= ifelse(V(g)$name == "JEHAN DE FRANCHE",V(g)$name, NA),
     vertex.label.cex=.6, 
     vertex.label.color ='blue')



