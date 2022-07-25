require(ggmap)
#load("./export/dataPostIgraph.RData")


beffroi = data.frame(lat= 50.36846828695569, lgn = 3.0806453369071782)
# map_beffroi <- get_map(location = beffroi)
# ggmap(map_beffroi)

bbox = c(left = 3.0620, bottom = 50.3625, right = 3.0950, top = 50.3820)

p = get_stamenmap(bbox, zoom=16, source = "stamen", maptype ="watercolor")

ggmap(p) +
  geom_point(data = beffroi, aes(lgn,lat, color = 'red'), size= 0.5) 

# N,E
# 50.381205937504824, 3.0763483291955755 ; 50.37202420150193, 3.0635018972602674
# 50.36361954544403, 3.0899918649267257 

