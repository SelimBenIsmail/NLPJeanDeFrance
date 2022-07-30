load("./export/dataPostIgraph.RData")

#### dataframe des attributs des sommets ####
df_nodes = data.frame(Anthroponyme = V(g)$name)
df_nodes <-  cbind(df_nodes,
                   NumEscroete = rep(NA,nrow(df_nodes)),
                   NumConnetablie = rep(NA,nrow(df_nodes)),
                   RdV = rep(NA,nrow(df_nodes)),
                   NumRente = rep(NA,nrow(df_nodes)))

for (i in 1:length(df_nodes$Anthroponyme)) {
  nrente <- df_debiteur_rente[df_debiteur_rente[,1]==df_nodes$Anthroponyme[i],2]
  if (length(nrente) != 0) {
    nrente <- nrente[1]
    from_main  <-  df_main[df_main$numRente == nrente,c(1,3,5,6)] 
    from_main <- from_main[!is.na(from_main$numRente),]
    df_nodes[i,] <- c(df_nodes$Anthroponyme[i],from_main)
  } 
}
## Completion de df_nodes sur base des sommets voisins ##
for (i in 1:length(df_nodes$Anthroponyme)) {
  if(is.na(df_nodes$NumRente[i])){
    d <- distances(g,v= V(g)[V(g)$name == df_nodes$Anthroponyme[i]], to = V(g)) == 1 
    adj_node <- V(g)[d]$name
    v_escroetes_adj <- NULL
    v_connetablies_adj <- NULL
    v_RdV_adj <- NULL
    
    for (j in adj_node) {
      escroete_adj <-  df_nodes$NumEscroete[df_nodes$Anthroponyme == j]
      v_escroetes_adj <- c(v_escroetes_adj,escroete_adj)
      connetablie_adj <-  df_nodes$NumConnetablie[df_nodes$Anthroponyme == j]
      v_connetablies_adj <- c(v_connetablies_adj,connetablie_adj)
      Rdv_adj <-  df_nodes$RdV[df_nodes$Anthroponyme == j]
      v_RdV_adj <- c(v_RdV_adj,Rdv_adj)
    }
    if (length(unique(v_connetablies_adj)) == 1) { #toutes les valeurs sont identiques
      df_nodes$NumConnetablie[i] <- v_connetablies_adj[1]
    } else if (length(unique(v_connetablies_adj)) >1 ) { #egalite -> NA
      df_nodes$NumConnetablie[i] <- NA
    }
    if (length(unique(v_escroetes_adj)) == 1) { #toutes les valeurs sont identiques
      df_nodes$NumEscroete[i] <- v_escroetes_adj[1]
    } else if (length(unique(v_escroetes_adj)) >1 ) { #egalite -> NA
      df_nodes$NumEscroete[i] <- NA
    }
    if (length(unique(v_RdV_adj)) == 1) { #toutes les valeurs sont identiques
      df_nodes$RdV[i] <- v_RdV_adj[1]
    } else if (length(unique(v_RdV_adj)) >1 ) { #egalite -> NA
      df_nodes$RdV[i] <- NA
    }
  }
}
vertex_attr(g) <- df_nodes


#dev.off()