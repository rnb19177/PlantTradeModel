# customer consignment distributions
#install.packages('snow')
setwd("/home/rnb19177/Documents/RStudioStuff/Rstudio/final_data_set_work/Network_work/SocialNetworkAnalysis/Customer_Consignment_Analysis")

oak.distribution.list <- list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
                              commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                              consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
                              nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
                              retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630) )

#install.packages('snow')
#install.packages('tidyverse')
#install.packages('truncnorm')
#install.packages('igraph')

library('snow')



ncores <- 10

source("Network_Functions_04_10_21.R")
node.list <- list(ncom = 40,ncons = 40, nnur = 40, nret = 40, Ret = 160*10)
a<- getwd()


com.mean <- c(10, seq(100, 1000, by = 100))
com.sd <- c(100, seq(1000, 5000, by = 500))
cons.mean <- c(10, seq(100, 1000, by = 100))
cons.sd <- c(100, seq(1000, 5000, by = 500))
nur.mean <- c(10, seq(100, 1000, by = 100))
nur.sd <- c(100, seq(1000, 5000, by = 500))
ret.mean <-c(10, seq(100, 1000, by = 100))
ret.sd <- c(100, seq(1000, 5000, by = 500))

# commercial parameters   

combinations <-   expand.grid(com.mean, com.sd)


combinations = as.data.frame(combinations)
combinations[,3:27] = NA
colnames(combinations) <- c("Commercial Mean", "Commercial s.d"
                            ,"N.Com.as", "N.Cons.as", "N.Nur.as", "N.Ret.as", "Ret.as"
                            ,"N.Com.hs", "N.Cons.hs", "N.Nur.hs", "N.Ret.hs", "Ret.hs"
                            ,"N.Com.betwns", "N.Cons.betwns", "N.Nur.betwns", "N.Ret.betwns", "Ret.betwns"
                            ,"N.Com.str.in", "N.Cons.str.in", "N.Nur.str.in", "N.Ret.str.in", "Ret.str.in"
                            ,"N.Com.str.out", "N.Cons.str.out", "N.Nur.str.out", "N.Ret.str.out", "Ret.str.out")


#redefine this function for every parameter set
hundred.networks.function <- function(j){
  
  oak.net<- netgen(node.list, distribution.list = list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
        commercial.mean = combinations[j,1] , commercial.sd = combinations[j,2],
        consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
        nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
        retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630) ))
  type.totals <- as_tibble(data.frame("Type" = V(oak.net)$Type))   %>%  count(Type, .drop = FALSE)
  
  #store vectors for node type positions
  # com.pos <- 1:type.totals$n[1]
  #  cons.pos <- seq((type.totals$n[1]+1), length  = (type.totals$n[2]))
  N.com.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3]))
  N.cons.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + 1), length = (type.totals$n[4]))
  N.nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + 1), length = (type.totals$n[5]))
  N.ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + 1), length = (type.totals$n[6]))
  Nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6]))
  ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6] + 1), length = (type.totals$n[7]))
  # N.to.R <- N.com.pos[1]:tail(ret.pos,1)
  #  
  #oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])
  
  #betweenness(oak.net.N.R.subgraph, weights = 1/E(oak.net.N.R.subgraph)$weight)
  
  as.1 <- as.numeric(authority_score(oak.net)$vector)
  hs.1 <- as.numeric(hub_score(oak.net)$vector)
  betwns.1 <- betweenness(oak.net, weights = 1/E(oak.net)$weight)
  str.in.1 <- strength(oak.net, mode = "in")
  str.out.1 <- strength(oak.net, mode = "out")
  #    
  avg.centralities.for.i <- c(mean(as.1[N.com.pos], na.rm = TRUE), mean(as.1[N.cons.pos], na.rm = TRUE), mean(as.1[N.nur.pos], na.rm = TRUE),
                              mean(as.1[N.ret.pos], na.rm = TRUE), mean(as.1[ret.pos], na.rm = TRUE)
                              
                              ,mean(hs.1[N.com.pos], na.rm = TRUE), mean(hs.1[N.cons.pos], na.rm = TRUE), mean(hs.1[N.nur.pos], na.rm = TRUE),
                              mean(hs.1[N.ret.pos], na.rm = TRUE), mean(hs.1[ret.pos], na.rm = TRUE),
                              
                              mean(betwns.1[N.com.pos], na.rm = TRUE), mean(betwns.1[N.cons.pos], na.rm = TRUE), mean(betwns.1[N.nur.pos], na.rm = TRUE),
                              mean(betwns.1[N.ret.pos], na.rm = TRUE), mean(betwns.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.in.1[N.com.pos], na.rm = TRUE), mean(str.in.1[N.cons.pos], na.rm = TRUE), mean(str.in.1[N.nur.pos], na.rm = TRUE),
                              mean(str.in.1[N.ret.pos], na.rm = TRUE), mean(str.in.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.out.1[N.com.pos], na.rm = TRUE), mean(str.out.1[N.cons.pos], na.rm = TRUE), mean(str.out.1[N.nur.pos], na.rm = TRUE),
                              mean(str.out.1[N.ret.pos], na.rm = TRUE), mean(str.out.1[ret.pos], na.rm = TRUE))
  
  return(avg.centralities.for.i)
  
}



test.function <- function(combinations, j){
  
  
  j.row.for.combinations <- rowMeans(sapply(X = rep(j,100),  function(X) hundred.networks.function(X) ))
  
  
  return(j.row.for.combinations)
  
  
}


#each column needs to be added to the jth row of combinations 


cl <- snow::makeCluster(ncores, type = "SOCK")
clusterCall(cl, function() library(tidyverse))
clusterCall(cl, function() library(truncnorm))
clusterCall(cl, function() library(igraph))


list.of.things.needed <- list("test.function", "netgen", "node.list", "combinations", "rnegbinomt", "hundred.networks.function")
clusterExport(cl,list.of.things.needed , envir=environment())

#system.time(
  result <- clusterApply(cl, x = 1:nrow(combinations), fun  = function(x)  test.function(combinations, x)) 
 # )
stopCluster(cl)

#transform list into matrix, then take transform to get it in same format as combinations data frame
result <- t(do.call(cbind, result))

combinations[1:nrow(result), seq(3, length.out = ncol(result), by = 1) ] = result

setwd(paste(a, "/Commercial_Parameters", sep = "")) 

save(combinations, file = "combinations.RData")

setwd(a) 

# consumer parameters   

combinations <- expand.grid(cons.mean, cons.sd)

combinations = as.data.frame(combinations)
combinations[,3:27] = NA
colnames(combinations) <- c("Consumer Mean", "Consumer s.d"
                            ,"N.Com.as", "N.Cons.as", "N.Nur.as", "N.Ret.as", "Ret.as"
                            ,"N.Com.hs", "N.Cons.hs", "N.Nur.hs", "N.Ret.hs", "Ret.hs"
                            ,"N.Com.betwns", "N.Cons.betwns", "N.Nur.betwns", "N.Ret.betwns", "Ret.betwns"
                            ,"N.Com.str.in", "N.Cons.str.in", "N.Nur.str.in", "N.Ret.str.in", "Ret.str.in"
                            ,"N.Com.str.out", "N.Cons.str.out", "N.Nur.str.out", "N.Ret.str.out", "Ret.str.out")

hundred.networks.function <- function(j){
  
  oak.net<- netgen(node.list, distribution.list = list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
                                                       commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                                                       consumer.mean = combinations[j,1], consumer.sd = combinations[j,2],
                                                       nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
                                                       retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630) ))
  
  type.totals <- as_tibble(data.frame("Type" = V(oak.net)$Type))   %>%  count(Type, .drop = FALSE)
  
  #store vectors for node type positions
  # com.pos <- 1:type.totals$n[1]
  #  cons.pos <- seq((type.totals$n[1]+1), length  = (type.totals$n[2]))
  N.com.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3]))
  N.cons.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + 1), length = (type.totals$n[4]))
  N.nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + 1), length = (type.totals$n[5]))
  N.ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + 1), length = (type.totals$n[6]))
  Nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6]))
  ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6] + 1), length = (type.totals$n[7]))
  #  
  #oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])
  
  #betweenness(oak.net.N.R.subgraph, weights = 1/E(oak.net.N.R.subgraph)$weight)
  
  
  as.1 <- as.numeric(authority_score(oak.net)$vector)
  hs.1 <- as.numeric(hub_score(oak.net)$vector)
  betwns.1 <- betweenness(oak.net, weights = 1/E(oak.net)$weight)
  str.in.1 <- strength(oak.net, mode = "in")
  str.out.1 <- strength(oak.net, mode = "out")
  #    
  avg.centralities.for.i <- c(mean(as.1[N.com.pos], na.rm = TRUE), mean(as.1[N.cons.pos], na.rm = TRUE), mean(as.1[N.nur.pos], na.rm = TRUE),
                              mean(as.1[N.ret.pos], na.rm = TRUE), mean(as.1[ret.pos], na.rm = TRUE)
                              
                              ,mean(hs.1[N.com.pos], na.rm = TRUE), mean(hs.1[N.cons.pos], na.rm = TRUE), mean(hs.1[N.nur.pos], na.rm = TRUE),
                              mean(hs.1[N.ret.pos], na.rm = TRUE), mean(hs.1[ret.pos], na.rm = TRUE),
                              
                              mean(betwns.1[N.com.pos], na.rm = TRUE), mean(betwns.1[N.cons.pos], na.rm = TRUE), mean(betwns.1[N.nur.pos], na.rm = TRUE),
                              mean(betwns.1[N.ret.pos], na.rm = TRUE), mean(betwns.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.in.1[N.com.pos], na.rm = TRUE), mean(str.in.1[N.cons.pos], na.rm = TRUE), mean(str.in.1[N.nur.pos], na.rm = TRUE),
                              mean(str.in.1[N.ret.pos], na.rm = TRUE), mean(str.in.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.out.1[N.com.pos], na.rm = TRUE), mean(str.out.1[N.cons.pos], na.rm = TRUE), mean(str.out.1[N.nur.pos], na.rm = TRUE),
                              mean(str.out.1[N.ret.pos], na.rm = TRUE), mean(str.out.1[ret.pos], na.rm = TRUE))
  
  
  
  return(avg.centralities.for.i)
  
}







cl <- snow::makeCluster(ncores, type = "SOCK")
clusterCall(cl, function() library(tidyverse))
clusterCall(cl, function() library(truncnorm))
clusterCall(cl, function() library(igraph))


list.of.things.needed <- list("test.function", "netgen", "node.list", "combinations", "rnegbinomt", "hundred.networks.function")
clusterExport(cl,list.of.things.needed , envir=environment())

#system.time(
  result <- clusterApply(cl, x = 1:nrow(combinations), fun  = function(x)  test.function(combinations, x)) 
  #)
stopCluster(cl)

#transform list into matrix, then take transform to get it in same format as combinations data frame
result <- t(do.call(cbind, result))

combinations[1:nrow(result), seq(3, length.out = ncol(result), by = 1) ] = result

setwd(paste(a, "/Consumer_Parameters", sep = ""))

save(combinations, file = "combinations.RData")



setwd(a) 

# Nursery parameters   

combinations <-   expand.grid(nur.mean, nur.sd)

combinations = as.data.frame(combinations)
combinations[,3:27] = NA
colnames(combinations) <- c("Nursery Mean", "Nursery s.d"
                            ,"N.Com.as", "N.Cons.as", "N.Nur.as", "N.Ret.as", "Ret.as"
                            ,"N.Com.hs", "N.Cons.hs", "N.Nur.hs", "N.Ret.hs", "Ret.hs"
                            ,"N.Com.betwns", "N.Cons.betwns", "N.Nur.betwns", "N.Ret.betwns", "Ret.betwns"
                            ,"N.Com.str.in", "N.Cons.str.in", "N.Nur.str.in", "N.Ret.str.in", "Ret.str.in"
                            ,"N.Com.str.out", "N.Cons.str.out", "N.Nur.str.out", "N.Ret.str.out", "Ret.str.out")

hundred.networks.function <- function(j){
  
  oak.net<- netgen(node.list, distribution.list = list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
                                                       commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                                                       consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
                                                       nursery.mean = combinations[j,1] , nursery.sd = combinations[j,2],
                                                       retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630) ))
  
  
  type.totals <- as_tibble(data.frame("Type" = V(oak.net)$Type))   %>%  count(Type, .drop = FALSE)
  
  #store vectors for node type positions
  # com.pos <- 1:type.totals$n[1]
  #  cons.pos <- seq((type.totals$n[1]+1), length  = (type.totals$n[2]))
  N.com.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3]))
  N.cons.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + 1), length = (type.totals$n[4]))
  N.nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + 1), length = (type.totals$n[5]))
  N.ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + 1), length = (type.totals$n[6]))
  Nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6]))
  ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6] + 1), length = (type.totals$n[7]))
  # N.to.R <- N.com.pos[1]:tail(ret.pos,1)
  #  
  #oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])
  
  #betweenness(oak.net.N.R.subgraph, weights = 1/E(oak.net.N.R.subgraph)$weight)
  
  
  as.1 <- as.numeric(authority_score(oak.net)$vector)
  hs.1 <- as.numeric(hub_score(oak.net)$vector)
 # pr.1 <- as.numeric(page_rank(oak.net)$vector)
  betwns.1 <- betweenness(oak.net, weights = 1/E(oak.net)$weight)
  str.in.1 <- strength(oak.net, mode = "in")
  str.out.1 <- strength(oak.net, mode = "out")
  #    
  avg.centralities.for.i <- c(mean(as.1[N.com.pos], na.rm = TRUE), mean(as.1[N.cons.pos], na.rm = TRUE), mean(as.1[N.nur.pos], na.rm = TRUE),
                              mean(as.1[N.ret.pos], na.rm = TRUE), mean(as.1[ret.pos], na.rm = TRUE)
                              
                              ,mean(hs.1[N.com.pos], na.rm = TRUE), mean(hs.1[N.cons.pos], na.rm = TRUE), mean(hs.1[N.nur.pos], na.rm = TRUE),
                              mean(hs.1[N.ret.pos], na.rm = TRUE), mean(hs.1[ret.pos], na.rm = TRUE),
                              
                          #    mean(pr.1[N.com.pos], na.rm = TRUE), mean(pr.1[N.cons.pos], na.rm = TRUE), mean(pr.1[N.nur.pos], na.rm = TRUE),
                           #   mean(pr.1[N.ret.pos], na.rm = TRUE), mean(pr.1[ret.pos], na.rm = TRUE),
                              
                              mean(betwns.1[N.com.pos], na.rm = TRUE), mean(betwns.1[N.cons.pos], na.rm = TRUE), mean(betwns.1[N.nur.pos], na.rm = TRUE),
                              mean(betwns.1[N.ret.pos], na.rm = TRUE), mean(betwns.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.in.1[N.com.pos], na.rm = TRUE), mean(str.in.1[N.cons.pos], na.rm = TRUE), mean(str.in.1[N.nur.pos], na.rm = TRUE),
                              mean(str.in.1[N.ret.pos], na.rm = TRUE), mean(str.in.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.out.1[N.com.pos], na.rm = TRUE), mean(str.out.1[N.cons.pos], na.rm = TRUE), mean(str.out.1[N.nur.pos], na.rm = TRUE),
                              mean(str.out.1[N.ret.pos], na.rm = TRUE), mean(str.out.1[ret.pos], na.rm = TRUE))
  
  
  
  return(avg.centralities.for.i)
  
}


cl <- snow::makeCluster(ncores, type = "SOCK")
clusterCall(cl, function() library(tidyverse))
clusterCall(cl, function() library(truncnorm))
clusterCall(cl, function() library(igraph))


list.of.things.needed <- list("test.function", "netgen", "node.list", "combinations", "rnegbinomt", "hundred.networks.function")
clusterExport(cl,list.of.things.needed , envir=environment())

#system.time(
  result <- clusterApply(cl, x = 1:nrow(combinations), fun  = function(x)  test.function(combinations, x)) 
  #)
stopCluster(cl)

#transform list into matrix, then take transform to get it in same format as combinations data frame
result <- t(do.call(cbind, result))




combinations[1:nrow(result), seq(3, length.out = ncol(result), by = 1) ] = result

setwd(paste(a, "/Nursery_Parameters", sep = ""))

save(combinations, file = "combinations.RData")

setwd(a)


#Retailer parameters   

combinations <-   expand.grid(ret.mean, ret.sd)

combinations = as.data.frame(combinations)
combinations[,3:27] = NA
colnames(combinations) <- c("Retailer Mean", "Retailer s.d"
                            ,"N.Com.as", "N.Cons.as", "N.Nur.as", "N.Ret.as", "Ret.as"
                            ,"N.Com.hs", "N.Cons.hs", "N.Nur.hs", "N.Ret.hs", "Ret.hs"
                            ,"N.Com.betwns", "N.Cons.betwns", "N.Nur.betwns", "N.Ret.betwns", "Ret.betwns"
                            ,"N.Com.str.in", "N.Cons.str.in", "N.Nur.str.in", "N.Ret.str.in", "Ret.str.in"
                            ,"N.Com.str.out", "N.Cons.str.out", "N.Nur.str.out", "N.Ret.str.out", "Ret.str.out")


hundred.networks.function <- function(j){
  
  oak.net<- netgen(node.list, distribution.list = list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
                                                       commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                                                       consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
                                                       nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
                                                       retailer.mean = combinations[j,1], retailer.sd = combinations[j,2] ))
  
  
  type.totals <- as_tibble(data.frame("Type" = V(oak.net)$Type))   %>%  count(Type, .drop = FALSE)
  
  #store vectors for node type positions
  # com.pos <- 1:type.totals$n[1]
  #  cons.pos <- seq((type.totals$n[1]+1), length  = (type.totals$n[2]))
  N.com.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3]))
  N.cons.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + 1), length = (type.totals$n[4]))
  N.nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + 1), length = (type.totals$n[5]))
  N.ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + 1), length = (type.totals$n[6]))
  Nur.pos <- seq((type.totals$n[1]+ type.totals$n[2] + 1), length = (type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6]))
  ret.pos <- seq((type.totals$n[1]+ type.totals$n[2] + type.totals$n[3] + type.totals$n[4] + type.totals$n[5] + type.totals$n[6] + 1), length = (type.totals$n[7]))
  # N.to.R <- N.com.pos[1]:tail(ret.pos,1)
  #  
  #oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])
  
  #betweenness(oak.net.N.R.subgraph, weights = 1/E(oak.net.N.R.subgraph)$weight)
  
  
  as.1 <- as.numeric(authority_score(oak.net)$vector)
  hs.1 <- as.numeric(hub_score(oak.net)$vector)
  betwns.1 <- betweenness(oak.net, weights = 1/E(oak.net)$weight)
  str.in.1 <- strength(oak.net, mode = "in")
  str.out.1 <- strength(oak.net, mode = "out")
  #    
  avg.centralities.for.i <- c(mean(as.1[N.com.pos], na.rm = TRUE), mean(as.1[N.cons.pos], na.rm = TRUE), mean(as.1[N.nur.pos], na.rm = TRUE),
                              mean(as.1[N.ret.pos], na.rm = TRUE), mean(as.1[ret.pos], na.rm = TRUE)
                              
                              ,mean(hs.1[N.com.pos], na.rm = TRUE), mean(hs.1[N.cons.pos], na.rm = TRUE), mean(hs.1[N.nur.pos], na.rm = TRUE),
                              mean(hs.1[N.ret.pos], na.rm = TRUE), mean(hs.1[ret.pos], na.rm = TRUE),
                              
                              mean(betwns.1[N.com.pos], na.rm = TRUE), mean(betwns.1[N.cons.pos], na.rm = TRUE), mean(betwns.1[N.nur.pos], na.rm = TRUE),
                              mean(betwns.1[N.ret.pos], na.rm = TRUE), mean(betwns.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.in.1[N.com.pos], na.rm = TRUE), mean(str.in.1[N.cons.pos], na.rm = TRUE), mean(str.in.1[N.nur.pos], na.rm = TRUE),
                              mean(str.in.1[N.ret.pos], na.rm = TRUE), mean(str.in.1[ret.pos], na.rm = TRUE),
                              
                              mean(str.out.1[N.com.pos], na.rm = TRUE), mean(str.out.1[N.cons.pos], na.rm = TRUE), mean(str.out.1[N.nur.pos], na.rm = TRUE),
                              mean(str.out.1[N.ret.pos], na.rm = TRUE), mean(str.out.1[ret.pos], na.rm = TRUE))
  
  return(avg.centralities.for.i)
  
}




cl <- snow::makeCluster(ncores, type = "SOCK")
clusterCall(cl, function() library(tidyverse))
clusterCall(cl, function() library(truncnorm))
clusterCall(cl, function() library(igraph))


list.of.things.needed <- list("test.function", "netgen", "node.list", "combinations", "rnegbinomt", "hundred.networks.function")
clusterExport(cl,list.of.things.needed , envir=environment())

#system.time(
  result <- clusterApply(cl, x = 1:nrow(combinations), fun  = function(x)  test.function(combinations, x)) 
  #)
stopCluster(cl)

#transform list into matrix, then take transform to get it in same format as combinations data frame
result <- t(do.call(cbind, result))


combinations[1:nrow(result), seq(3, length.out = ncol(result), by = 1) ] = result

setwd(paste(a, "/Retailer_Parameters", sep = ""))

save(combinations, file = "combinations.RData")