#### objet igraph #####
g <- graph_from_data_frame(d=df_links, directed=FALSE) 
g <- simplify(g,remove.loops = TRUE)
lfr = layout.fruchterman.reingold(g)
colrs = brewer.pal(n = 3, name = 'Set1')
#### test couleur ####
#E(g)$color <- colrs
#deg <- degree(df_links,mode = all)

#### plotting #####
plot(g, layout=lfr, 
     vertex.size=3 , 
     vertex.label= ifelse(degree(g)>5,V(g)$name, NA),
     vertex.label.cex=.5, 
     vertex.label.color ='red')



