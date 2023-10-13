# nursery and retailer degree distributions

# out.deg.mean 
# out.deg. dispersal

# measures to look at
# hubs
# authorities
# betweenness

source("../Network_Functions.R")


library(parallel)
library('snow')
#   input: combinations matrix

# function: for each combination, 
#       - creates network
#       - calculates centrality measures: as, hs, betwns and unweighted variants
#       - takes average result of centrality measures per node group for 100 networks

#   output: average centrality score per node group for given combination 


#baseline
node.list <- list(ncom = 40,ncons = 40, nnur = 40, nret = 40, Ret = 160*10)


oak.distribution.list.1 <- list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
                                commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                                consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
                                nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
                                retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630))

out.deg.mean <- seq(100, 1000, by = 100)
out.degree.variance <- seq(2000, 20000, by = 2000)


# create all permutations of the parameters being varied
# combinations <- crossing(variable_x,variable_y)
combinations <-   expand.grid(out.deg.mean, out.degree.variance)

combinations = as.data.frame(combinations)
combinations[,3:32] = NA
colnames(combinations) <- c("Out Degree Mean", "Out Degree Variance"
                            ,"N.Com.as", "N.Cons.as", "N.Nur.as", "N.Ret.as", "Ret.as"
                            ,"N.Com.hs", "N.Cons.hs", "N.Nur.hs", "N.Ret.hs", "Ret.hs"
                            ,"N.Com.betwns", "N.Cons.betwns", "N.Nur.betwns", "N.Ret.betwns", "Ret.betwns"
                            ,"N.Com.as.uw", "N.Cons.as.uw", "N.Nur.as.uw", "N.Ret.as.uw", "Ret.as.uw"
                            ,"N.Com.hs.uw", "N.Cons.hs.uw", "N.Nur.hs.uw", "N.Ret.hs.uw", "Ret.hs.uw"
                            ,"N.Com.betwns.uw", "N.Cons.betwns.uw", "N.Nur.betwns.uw", "N.Ret.betwns.uw", "Ret.betwns.uw"                            
                            ,"N.Com.str.in", "N.Cons.str.in", "N.Nur.str.in", "N.Ret.str.in", "Ret.str.in"
                            ,"N.Com.str.out", "N.Cons.str.out", "N.Nur.str.out", "N.Ret.str.out", "Ret.str.out")


#create function for input j, gives vector output of centrality scores

hundred.networks.function <- function(j){
  
  oak.net<- netgen(node.list, distribution.list = list(out.deg.mean = combinations[j,1],
                                                       out.deg.dispersal = (combinations[j,1])^2/(combinations[j,2] - combinations[j,1]),
                                                       commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                                                       consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
                                                       nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
                                                       retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630)))
  
  type.totals <- as_tibble(data.frame("Type" = V(oak.net)$Type))   %>%  count(Type, .drop = FALSE)
  
  #store vectors for node type positions
  # com.pos <- 1:type.totals$n[1]
  #  cons.pos <- seq((type.totals$n[1]+1), length  = (type.totals$n[2]))
  N.com.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3]))
  N.cons.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + 1), length = (type.totals$n[4]))
  N.nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + 1), length = (type.totals$n[5]))
  N.ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + 1), length = (type.totals$n[6]))
  ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6] + 1), length = (type.totals$n[7]))
  #  
  #oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])
  
  #betweenness(oak.net.N.R.subgraph, weights = 1/E(oak.net.N.R.subgraph)$weight)
  
  
  as.1 <- as.numeric(authority_score(oak.net)$vector)
  hs.1 <- as.numeric(hub_score(oak.net)$vector)

  betwns.1 <- betweenness(oak.net, weights = 1/E(oak.net)$weight)
  str.in.1 <- strength(oak.net, mode = "in")
  str.out.1 <- strength(oak.net, mode = "out")
  
  as.uw.1 <- as.numeric(authority_score(oak.net, weights = NA)$vector)
  hs.uw.1 <- as.numeric(hub_score(oak.net, weights = NA)$vector)
  
  betwns.uw.1 <- betweenness(oak.net, weights = NA)
  
  #    
  avg.centralities.for.i <- c(mean(as.1[N.com.pos], na.rm = TRUE), mean(as.1[N.cons.pos], na.rm = TRUE), mean(as.1[N.nur.pos], na.rm = TRUE),
                              mean(as.1[N.ret.pos], na.rm = TRUE), mean(as.1[ret.pos], na.rm = TRUE)
                              
                              ,mean(hs.1[N.com.pos], na.rm = TRUE), mean(hs.1[N.cons.pos], na.rm = TRUE), mean(hs.1[N.nur.pos], na.rm = TRUE),
                              mean(hs.1[N.ret.pos], na.rm = TRUE), mean(hs.1[ret.pos], na.rm = TRUE),
                              
                              mean(betwns.1[N.com.pos], na.rm = TRUE), mean(betwns.1[N.cons.pos], na.rm = TRUE), mean(betwns.1[N.nur.pos], na.rm = TRUE),
                              mean(betwns.1[N.ret.pos], na.rm = TRUE), mean(betwns.1[ret.pos], na.rm = TRUE) 
                          
                          
                          ,mean(as.uw.1[N.com.pos], na.rm = TRUE), mean(as.uw.1[N.cons.pos], na.rm = TRUE), mean(as.uw.1[N.nur.pos], na.rm = TRUE),
                            mean(as.uw.1[N.ret.pos], na.rm = TRUE), mean(as.uw.1[ret.pos], na.rm = TRUE)
                            
                            ,mean(hs.uw.1[N.com.pos], na.rm = TRUE), mean(hs.uw.1[N.cons.pos], na.rm = TRUE), mean(hs.uw.1[N.nur.pos], na.rm = TRUE),
                            mean(hs.uw.1[N.ret.pos], na.rm = TRUE), mean(hs.uw.1[ret.pos], na.rm = TRUE),
                            
                            mean(betwns.uw.1[N.com.pos], na.rm = TRUE), mean(betwns.uw.1[N.cons.pos], na.rm = TRUE), mean(betwns.uw.1[N.nur.pos], na.rm = TRUE),
                            mean(betwns.uw.1[N.ret.pos], na.rm = TRUE), mean(betwns.uw.1[ret.pos], na.rm = TRUE),
                          
                              
                              mean(str.in.1[N.com.pos], na.rm = TRUE), mean(str.in.1[N.cons.pos], na.rm = TRUE), mean(str.in.1[N.nur.pos], na.rm = TRUE),
                              mean(str.in.1[N.ret.pos], na.rm = TRUE), mean(str.in.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.out.1[N.com.pos], na.rm = TRUE), mean(str.out.1[N.cons.pos], na.rm = TRUE), mean(str.out.1[N.nur.pos], na.rm = TRUE),
                              mean(str.out.1[N.ret.pos], na.rm = TRUE), mean(str.out.1[ret.pos], na.rm = TRUE))
  
                                     
  
  return(avg.centralities.for.i)
  
}

#create function for input j, gives vector output of centrality scores

test.function <- function(combinations, j){
  
  
  j.row.for.combinations <- rowMeans(sapply(X = rep(j,100),  function(X) hundred.networks.function(X) ))
  
  
  return(j.row.for.combinations)
  
  
}


#test.function(combinations, 2)


#each column needs to be added to the jth row of 




ncores <- 10

cl <- snow::makeCluster(ncores, type = "SOCK")
clusterCall(cl, function() library(tidyverse))
clusterCall(cl, function() library(truncnorm))
clusterCall(cl, function() library(igraph))


list.of.things.needed <- list("test.function", "netgen", "node.list", "combinations", "rnegbinomt", "hundred.networks.function")
clusterExport(cl,list.of.things.needed , envir=environment())
#set.seed(105)

result <- clusterApply(cl, x = 1:nrow(combinations), fun  = function(x)  test.function(combinations, x)) 
stopCluster(cl)

#transform list into matrix, then take transform to get it in same format as combinations data frame
result <- t(do.call(cbind, result))


combinations[1:nrow(result), seq(3, length.out = ncol(result), by = 1) ] = result

save(combinations, file = "combinations.Rdata")


