# plan is to run different distributions of nurseries, focus on oak, run a bunch of measures


#Code runs the social network analysis for centrality measures and network vs network subset analysis.



# measures to look at
# betweenness - weights interpreted as distances
# node strength distribution
# hubs, authorities - weights mean strengths
#

source("Network_Functions.R")


#160 nurseries
# different plants will have different nursery distributions
# more commercial plant ( i.e. Oak) ergo more Ncom, less Ncons, Nret
# keep Nnur the same
# consumer plant (more Ncons, Nret, less Ncom)

#first node list: uniformly distributed
#baseline
node.list.1 <- list(ncom = 40,ncons = 40, nnur = 40, nret = 40, Ret = 160*10)

#horticulture
node.list.2 <- list(ncom = 80,ncons = 20, nnur = 40, nret = 20, Ret = 160*10)

#ornamental
node.list.3 <- list(ncom = 20,ncons = 50, nnur = 40, nret = 50, Ret = 160*10)


# try other distributions, 1 nursery type dominating

node.list.4 <- list(ncom = 130,ncons = 10, nnur = 10, nret = 10, Ret = 160*10)

node.list.5 <- list(ncom = 10,ncons = 130, nnur = 10, nret = 10, Ret = 160*10)

node.list.6 <- list(ncom = 10,ncons = 10, nnur = 130, nret = 10, Ret = 160*10)

node.list.7 <- list(ncom = 10,ncons = 10, nnur = 10, nret = 130, Ret = 160*10)


oak.distribution.list <- list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
                              commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                              consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
                              nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
                              retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630) )

#loop for node.list i
for (k in 1:7 ) {
  
betweenness.df <- matrix(NA, nrow = 100, ncol = 5)
betweenness.subgraph.df <- matrix(NA, nrow = 100, ncol = 5)


betweenness.unweighted.df <- matrix(NA, nrow = 100, ncol = 5)
betweenness.unweighted.subgraph.df <- matrix(NA, nrow = 100, ncol = 5)

  str.in.df <- matrix(NA, nrow = 100, ncol = 5 )
   str.in.subgraph.df <- matrix(NA, nrow = 100, ncol = 5 )

   str.out.df <- matrix(NA, nrow = 100, ncol = 5 )
  str.out.subgraph.df <- matrix(NA, nrow = 100, ncol = 5 )
 in.deg.df <- matrix(NA, nrow = 100, ncol = 5 )
 out.deg.df <- matrix(NA, nrow = 100, ncol = 5 )
 in.deg.subgraph.df <- matrix(NA, nrow = 100, ncol = 5 )
 out.deg.subgraph.df <- matrix(NA, nrow = 100, ncol = 5 )
 
as.df <- matrix(NA, nrow = 100, ncol = 5)
hs.df <- matrix(NA, nrow = 100, ncol = 5)
as.subgraph.df <- matrix(NA, nrow = 100, ncol = 5)
hs.subgraph.df <- matrix(NA, nrow = 100, ncol = 5)
as.unweighted.df <- matrix(NA, nrow = 100, ncol = 5)
hs.unweighted.df <- matrix(NA, nrow = 100, ncol = 5)
as.subgraph.unweighted.df <- matrix(NA, nrow = 100, ncol = 5)
hs.subgraph.unweighted.df <- matrix(NA, nrow = 100, ncol = 5)



for (i in 1:100) {
  
  
  
  #change node.list for different scenario
  oak.net<- netgen(get(paste0("node.list.", k)), oak.distribution.list)
    
  type.totals <- as_tibble(data.frame("Type" = V(oak.net)$Type))   %>%  count(Type, .drop = FALSE) 
  #store vectors for node type positions
  com.pos <- 1:type.totals$n[1]
  cons.pos <- seq((type.totals$n[1]+1), length  = (type.totals$n[2]))
  N.com.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3]))
  N.cons.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + 1), length = (type.totals$n[4]))
  N.nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + 1), length = (type.totals$n[5]))
  N.ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + 1), length = (type.totals$n[6]))
  Nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6]))
  ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6] + 1), length = (type.totals$n[7]))
  N.to.R <- N.com.pos[1]:tail(ret.pos,1)
  
  #Code below subsets the graph to include only nurseries and retailers
  oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])
  
  

  betwns.1 <- betweenness(oak.net, weights = 1/E(oak.net)$weight) #betweenness(oak.net, weights = NA)
   betwns.unweighted.1 <- betweenness(oak.net, weights = NA)

  betwns.subgraph <- betweenness(oak.net.N.R.subgraph, weights = 1/E(oak.net.N.R.subgraph)$weight) #betweenness(oak.net.N.R.subgraph, weights = NA)
   betwns.unweighted.subgraph <- betweenness(oak.net.N.R.subgraph, weights = NA)
  
   str.in <- as.numeric(strength(oak.net, mode = "in"))
   in.deg <- as.numeric(degree(oak.net, mode = "in"))
   str.in[is.na(str.in)] = 0
  str.out <- as.numeric(strength(oak.net, mode = "out"))
   out.deg <- as.numeric(degree(oak.net, mode = "out"))
   str.out[is.na(str.out)] = 0
  str.in.subgraph <- as.numeric(strength(oak.net.N.R.subgraph, mode = "in"))
  str.in.subgraph[is.na(str.in.subgraph)] = 0
   in.deg.subgraph <- as.numeric(degree(oak.net.N.R.subgraph, mode = "in"))
   in.deg.subgraph[is.na(in.deg.subgraph)] = 0
   
   str.out.subgraph <- as.numeric(strength(oak.net.N.R.subgraph, mode = "out"))
   str.out.subgraph[is.na(str.out.subgraph)] = 0
   
   
   out.deg.subgraph <- as.numeric(degree(oak.net.N.R.subgraph, mode = "out"))
   out.deg.subgraph[is.na(out.deg.subgraph)] = 0
   
    as.1 <- as.numeric(authority_score(oak.net)$vector)
    hs.1 <- as.numeric(hub_score(oak.net)$vector)
    as.subgraph <- as.numeric(authority_score(oak.net.N.R.subgraph)$vector)
    hs.subgraph <- as.numeric(hub_score(oak.net.N.R.subgraph)$vector)
  
  as.unweighted.1 <- as.numeric(authority_score(oak.net, weights = NA)$vector)
  hs.unweighted.1 <- as.numeric(hub_score(oak.net, weights = NA)$vector)
  as.subgraph.unweighted.1 <- as.numeric(authority_score(oak.net.N.R.subgraph, weights = NA)$vector)
  hs.subgraph.unweighted.1 <- as.numeric(hub_score(oak.net.N.R.subgraph, weights = NA)$vector)
  
  
  # # 
  as.df[i,] = c(mean(as.1[N.com.pos]), mean(as.1[N.cons.pos]), mean(as.1[N.nur.pos]), mean(as.1[N.ret.pos]), mean(as.1[ret.pos]))
 hs.df[i,] = c(mean(hs.1[N.com.pos]), mean(hs.1[N.cons.pos]), mean(hs.1[N.nur.pos]), mean(hs.1[N.ret.pos]), mean(hs.1[ret.pos]))
  as.unweighted.df[i,] = c(mean(as.unweighted.1[N.com.pos]), mean(as.unweighted.1[N.cons.pos]), mean(as.unweighted.1[N.nur.pos]), mean(as.unweighted.1[N.ret.pos]), mean(as.unweighted.1[ret.pos]))
  hs.unweighted.df[i,] = c(mean(hs.unweighted.1[N.com.pos]), mean(hs.unweighted.1[N.cons.pos]), mean(hs.unweighted.1[N.nur.pos]), mean(hs.unweighted.1[N.ret.pos]), mean(hs.unweighted.1[ret.pos]))
  as.subgraph.df[i,] = c(mean(as.subgraph[seq(1, length = length(N.com.pos))]), mean(as.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                            mean(as.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                         mean(as.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                            mean(as.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
   
   hs.subgraph.df[i,] = c(mean(hs.subgraph[seq(1, length = length(N.com.pos))]), mean(hs.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                           mean(hs.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                            mean(hs.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                           mean(hs.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))

  as.subgraph.unweighted.df[i,] = c(mean(as.subgraph.unweighted.1[seq(1, length = length(N.com.pos))]), mean(as.subgraph.unweighted.1[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                           mean(as.subgraph.unweighted.1[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                           mean(as.subgraph.unweighted.1[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                           mean(as.subgraph.unweighted.1[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
  
  hs.subgraph.unweighted.df[i,] = c(mean(hs.subgraph.unweighted.1[seq(1, length = length(N.com.pos))]), mean(hs.subgraph.unweighted.1[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                           mean(hs.subgraph.unweighted.1[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                           mean(hs.subgraph.unweighted.1[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                           mean(hs.subgraph.unweighted.1[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
  
  
  

   betweenness.df[i,] = c(mean(betwns.1[N.com.pos]), mean(betwns.1[N.cons.pos]), mean(betwns.1[N.nur.pos]), mean(betwns.1[N.ret.pos]), mean(betwns.1[ret.pos]))
    betweenness.unweighted.df[i,] = c(mean(betwns.unweighted.1[N.com.pos]), mean(betwns.unweighted.1[N.cons.pos]), mean(betwns.unweighted.1[N.nur.pos]), mean(betwns.unweighted.1[N.ret.pos]), mean(betwns.unweighted.1[ret.pos]))
  

   betweenness.subgraph.df[i,] = c(mean(betwns.subgraph[seq(1, length = length(N.com.pos))]), mean(betwns.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]), 
                                    mean(betwns.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]), 
                                    mean(betwns.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                                    mean(betwns.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
  
       betweenness.unweighted.subgraph.df[i,] = c(mean(betwns.unweighted.subgraph[seq(1, length = length(N.com.pos))]), mean(betwns.unweighted.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]), 
                                      mean(betwns.unweighted.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]), 
                                       mean(betwns.unweighted.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                                       mean(betwns.unweighted.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
       
       
    
   #  
    str.in.df[i,] = c(mean(str.in[N.com.pos]), mean(str.in[N.cons.pos]), mean(str.in[N.nur.pos]), mean(str.in[N.ret.pos]), mean(str.in[ret.pos]))
    
    in.deg.df[i,] = c(mean(in.deg[N.com.pos]), mean(in.deg[N.cons.pos]), mean(in.deg[N.nur.pos]), mean(in.deg[N.ret.pos]), mean(in.deg[ret.pos]))
        
    str.out.df[i,] = c(mean(str.out[N.com.pos]), mean(str.out[N.cons.pos]), mean(str.out[N.nur.pos]), mean(str.out[N.ret.pos]), mean(str.out[ret.pos]))
    
    out.deg.df[i,] = c(mean(out.deg[N.com.pos]), mean(out.deg[N.cons.pos]), mean(out.deg[N.nur.pos]), mean(out.deg[N.ret.pos]), mean(out.deg[ret.pos]))
   # 
    
    
     str.in.subgraph.df[i,] = c(mean(str.in.subgraph[seq(1, length = length(N.com.pos))]), mean(str.in.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                                  mean(str.in.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                                 mean(str.in.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                                  mean(str.in.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
    
    in.deg.subgraph.df[i,] = c(mean(in.deg.subgraph[seq(1, length = length(N.com.pos))]), mean(in.deg.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                                 mean(in.deg.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                                 mean(in.deg.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                                 mean(in.deg.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
    
    
   # 
     str.out.subgraph.df[i,] = c(mean(str.out.subgraph[seq(1, length = length(N.com.pos))]), mean(str.out.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                                  mean(str.out.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                                  mean(str.out.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                                  mean(str.out.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))
    out.deg.subgraph.df[i,] = c(mean(out.deg.subgraph[seq(1, length = length(N.com.pos))]), mean(out.deg.subgraph[seq(1 + length(N.com.pos) , length = length(N.cons.pos))]),
                                  mean(out.deg.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) , length = length(N.nur.pos))]),
                                  mean(out.deg.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos)  , length = length(N.ret.pos))]),
                                  mean(out.deg.subgraph[seq(1 + length(N.com.pos) + length(N.cons.pos) + length(N.nur.pos) + length(N.ret.pos), length = length(ret.pos))]))

  
}

#Below code changes number of file to fit node.list number 
 
 
 assign(paste0("betweenness.df.", k),betweenness.df)
 assign(paste0("betweenness.subgraph.df.", k),betweenness.subgraph.df)
 save( list = (paste0("betweenness_df_", k)), file=paste0(paste0("betweenness_df_", k), ".Rdata") )
 save( list = (paste0("betweenness_subgraph_df_", k)), file=paste0(paste0("betweenness_subgraph_df_", k), ".Rdata") )

 
 assign(paste0("betweenness.unweighted.df.", k),betweenness.unweighted.df)
 assign(paste0("betweenness.unweighted.subgraph.df.", k),betweenness.unweighted.subgraph.df)
 save( list = (paste0("betweenness_unweighted_df_", k)), file=paste0(paste0("betweenness_unweighted_df_", k), ".Rdata") )
 save( list = (paste0("betweenness_unweighted_subgraph_df_", k)), file=paste0(paste0("betweenness_unweighted_subgraph_df_", k), ".Rdata") )
 

 assign(paste0("str.in.df.", k),str.in.df)
 assign(paste0("str.in.subgraph.df.", k),str.in.subgraph.df)
 save( list = (paste0("str.in.df.", k)), file=paste0(paste0("str_in_df_", k), ".Rdata") )
 save( list = (paste0("str.in.subgraph.df.", k)), file=paste0(paste0("str_in_subgraph_df_", k), ".Rdata") )


 assign(paste0("str.out.df.", k),str.out.df)
 assign(paste0("str.out.subgraph.df.", k),str.out.subgraph.df)
 save( list = (paste0("str.out.df.", k)), file=paste0(paste0("str_out_df_", k), ".Rdata") )
 save( list = (paste0("str.out.subgraph.df.", k)), file=paste0(paste0("str_out_subgraph_df_", k), ".Rdata") )
 
 
 assign(paste0("in.deg.df.", k),in.deg.df)
 assign(paste0("in.deg.subgraph.df.", k),in.deg.subgraph.df)
 save( list = (paste0("in.deg.df.", k)), file=paste0(paste0("in_deg_df_", k), ".Rdata") )
 save( list = (paste0("in.deg.subgraph.df.", k)), file=paste0(paste0("in_deg_subgraph_df_", k), ".Rdata") )
 
 
 assign(paste0("out.deg.df.", k),out.deg.df)
 assign(paste0("out.deg.subgraph.df.", k),out.deg.subgraph.df)
 save( list = (paste0("out.deg.df.", k)), file=paste0(paste0("out_deg_df_", k), ".Rdata") )
 save( list = (paste0("out.deg.subgraph.df.", k)), file=paste0(paste0("out_deg_subgraph_df_", k), ".Rdata") )
  
  
 assign(paste0("as.df.", k),as.df)
 assign(paste0("as.subgraph.df.", k),as.subgraph.df)
 save( list = (paste0("as.df.", k)), file=paste0(paste0("as_df_", k), "_Rdata") )
 save( list = (paste0("as.subgraph.df.", k)), file=paste0(paste0("as_subgraph_df_", k), ".Rdata") )
 
  
 assign(paste0("hs.df.", k),hs.df)
 assign(paste0("hs.subgraph.df.", k),hs.subgraph.df)
 save( list = (paste0("hs.df.", k)), file=paste0(paste0("hs_df_", k), "_Rdata") )
 save( list = (paste0("hs.subgraph.df.", k)), file=paste0(paste0("hs_subgraph_df_", k), ".Rdata") )
 
  
 assign(paste0("as.unweighted.df.", k),as.unweighted.df)
 assign(paste0("as.subgraph.unweighted.df.", k),as.subgraph.unweighted.df)
 save( list = (paste0("as.unweighted.df.", k)), file=paste0(paste0("as_unweighted_df_", k), ".Rdata") )  
 save( list = (paste0("as.subgraph.unweighted.df.", k)), file=paste0(paste0("as_subgraph_unweighted_df_", k), ".Rdata") )
  
  
 assign(paste0("hs.unweighted.df.", k),hs.unweighted.df)
 assign(paste0("hs.subgraph.unweighted.df.", k),hs.subgraph.unweighted.df)
 save( list = (paste0("hs.unweighted.df.", k)), file=paste0(paste0("hs_unweighted_df_", k), ".Rdata") ) 
 save( list = (paste0("hs.subgraph.unweighted.df.", k)), file=paste0(paste0("hs_subgraph_unweighted_df_", k), ".Rdata") )
  
  


  
}





