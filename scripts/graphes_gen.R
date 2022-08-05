####  palette de couleurs ####
df_numEscroete_colors = data.frame(
  numEscroete = unique(df_links$NumEscroete),
  colors = c(brewer.pal(n = 8, name = 'Set1'),brewer.pal(n = 3, name = 'Set2'))
)

#### inclusion des couleurs dans df_links ####
if (length(df_links)<= 6) {
  df_links <- cbind(df_links,df_links$NumEscroete)
  names(df_links)[7] <- "Ecol"
  for (i in 1:length(df_numEscroete_colors$numEscroete)) {
    df_links[df_links[,7] == df_numEscroete_colors$numEscroete[i],7] <- df_numEscroete_colors$colors[i]
  }
}

#### objet igraph #####
#g <- graph_from_data_frame(d=df_links, vertices = df_nodes, directed=FALSE) 
g <- graph_from_data_frame(d=df_links, directed=FALSE) 
g <- simplify(g,remove.loops = TRUE)
E(g)$NumEscroete <- df_links$NumEscroete
E(g)$NumConnetablie <- df_links$NumConnetablie
E(g)$RdV <- df_links$RdV
E(g)$NumRente <- df_links$NumRente
E(g)$color <- df_links$Ecol

#### plotting #####
plot(g, 
     layout=layout_nicely, 
     vertex.size=3 , 
     vertex.label= ifelse(V(g)$name == "JEHAN DE FRANCHE" || V(g)$name == "JEHAN DE FRANCHE",V(g)$name, NA),
     vertex.label.cex=.6)
title(main = "Graphes des relations mitoyennes extraient du rentier de Jean de France")
legend(x=1.2, y=1.,
       df_numEscroete_colors$numEscroete,
       pch=22, col="#777777", pt.bg=df_numEscroete_colors$colors,
       pt.cex=1, cex=.5, bty="n", ncol=1)
