#g <- graph_from_data_frame(d=df_links, vertices = df_nodes, directed=FALSE) 
g <- graph_from_data_frame(d=df_links, directed=FALSE) 
g <- simplify(g,remove.loops = TRUE)
lfr = layout.fruchterman.reingold(g)
colrs = brewer.pal(n = 8, name = 'RdBu')
####

#deg <- degree(df_links,mode = all)

###@
plot(g, layout=lfr, vertex.size=3 , vertex.label=NA)
#plot(g, vertex.size=5, vertex.label.cex=.8)




