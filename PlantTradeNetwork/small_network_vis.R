#Plotting code for small network visualisation

source("Network_Functions.R")

node.list <- list(ncom = 8, ncons = 8, nnur = 8, nret = 8, Ret = 32*10)

oak.distribution.list <- list(out.deg.mean = 380*(32/630), out.deg.dispersal = (32/630)*(380)^2/(114587*(32/630) - 380),
                              commercial.mean = 79*(32/630) , commercial.sd = 297*(32/630),
                              consumer.mean = 185*(32/630), consumer.sd = 822*(32/630),
                              nursery.mean = 1844*(32/630) , nursery.sd = 3567*(32/630),
                              retailer.mean = 3497*(32/630), retailer.sd = 9588*(32/630) )


set.seed(28082023)
oak.net<- netgen(node.list, oak.distribution.list)
#save(oak.net, file = "oak_net_32_nurseries.RData" ) # 
oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])

vcount(oak.net)
ecount(oak.net)

deg <- degree(oak.net, mode = 'all')
edge.start <- ends(oak.net, es = E(oak.net), names = FALSE)[, 1]
edge.col <- V(oak.net)$Col[edge.start]


deg.N.R.subgraph <- degree(oak.net,
                           v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))], 
                           mode = "all")
edge.start.N.R.subgraph <- ends(oak.net.N.R.subgraph, es = E(oak.net.N.R.subgraph), names = FALSE)[, 1]
edge.col.N.R.subgraph <- V(oak.net.N.R.subgraph)$Col[edge.start.N.R.subgraph]


  set.seed(5)
  Layout <- layout_with_lgl(oak.net)
  #Layout.N.R.subgraph <- Layout[(dim(Layout)[1]- 351):(dim(Layout)[1]),]
  Layout.N.R.subgraph <- layout_with_lgl(oak.net.N.R.subgraph)
  Layout.N.R.subgraph <- Layout[which(V(oak.net)$name %in% V(oak.net.N.R.subgraph)$name),]
  
  par(mfrow = c(1,2))
  #tiff("32_network_full.tiff", units="in", width=7, height=7, res=300)
  
  plot(oak.net, vertex.label = NA, edge.color = edge.col, vertex.color = V(oak.net)$Col, vertex.size = 1.1*(log10(deg)+1),
       edge.arrow.size = .3, edge.width = .5, layout = Layout) 
  legend(x=-1, y=-0.35, legend = c("Commercial", "Consumer", expression('N'['COM']),expression('N'['CON']), expression('N'['NUR']), expression('N'['Ret']), "Retailer"), pch=21,
         col= 'black',
         pt.bg = c("#F0F757", "#89043D", "#228B22", "#DB7093", "#00008B", "#0A0908", "#98D2EB") ,
         pt.cex=1.8, cex=1, bty="n", ncol=1)


   # dev.off()
  
 # tiff("32_network_N_R_subset.tiff", units="in", width=7, height=7, res=300)
  
  plot(oak.net.N.R.subgraph, vertex.label = NA, edge.color = edge.col.N.R.subgraph,
       vertex.color = V(oak.net.N.R.subgraph)$Col, vertex.size = 2.25*(log10(deg.N.R.subgraph)+1),
       edge.arrow.size = .2, edge.width = 1.5, layout = Layout.N.R.subgraph)
  legend(x=-1, y=-0.35, legend = c(expression('N'['COM']),expression('N'['CON']), expression('N'['NUR']), expression('N'['Ret']), "Retailer"), pch=21,
         col= 'black',
         pt.bg = c("#228B22", "#DB7093", "#00008B", "#0A0908", "#98D2EB") ,
         pt.cex=1.8, cex=1, bty="n", ncol=1)
  #dev.off()
  
  
  
  
