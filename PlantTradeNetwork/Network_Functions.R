#Network Functions 07_09_21

#functions to run network
library(tidyr)
library(tidyverse)
library(lubridate)
library(MASS)
library(truncnorm)
library(dplyr)
library(Matrix)
library(igraph)
library(RSpectra)
library(parallel)

rnegbinomt <- function(n, range, size, mu) {
  
  # range is a vector of two values
  
  F.a <- pnbinom(min(range), size = size, mu = mu )
  F.b <- pnbinom(max(range), size = size, mu = mu)
  
  u <- runif(n, min = F.a, max = F.b)
  
  qnbinom(u, size = size, mu = mu)
  
}



netgen <- function(node.list, distribution.list ) {
  
  
  # node.list <- list(ncom = 40, ncons = 40, nnur = 40, nret = 40, Ret = 160*10)
  #distribution.list <- list(out.deg.mean = 1442*(160/630), out.deg.dispersal = (160/630)*1443^2/(569969*(160/630) - 1443), 
  #                          commercial.mean = 18*(160/630) , commercial.sd = 34*(160/630),
  #                         consumer.mean = 20*(160/630), consumer.sd = 38*(160/630),
  #                        nursery.mean = 43*(160/630) , nursery.sd = 62*(160/630),
  #                       retailer.mean = 26*(160/630), retailer.sd = 23*(160/630) )
  
  # # #
  
  # ncom <- 40
  # 
  #  ncons <- 40
  # 
  #  nnur <- 40
  #   nret <- 40
  #   Ret = 160*10
  # 
  #   out.deg.mean = 1442*(160/630)
  #     out.deg.dispersal = (160/630)*1443^2/(569969 - 1443)
  #   commercial.mean = 18*(160/630)
  #  commercial.sd = 34*(160/630)
  #  consumer.mean = 20*(160/630)
  # consumer.sd = 38*(160/630)
  # nursery.mean = 43*(160/630)
  # nursery.sd = 62*(160/630)
  # retailer.mean = 26*(160/630)
  # retailer.sd = 23*(160/630)
  # #    
  
  
  
  
  with(as.list(c(node.list, distribution.list)),{
    
    
    total.nurseries <- ncom  + ncons + nnur + nret
    #  Ret = total.nurseries * 10
    
    # https://hta.org.uk/uploads/assets/uploaded/d9afcd6e-8a2c-48a1-89c1d67d94e19d2d.pdf link to hta website that could lead to a way of approximating retailers
    
    
    n.com.nodes <- as.character(paste0("N.Com.", formatC(1:ncom, width=(floor(log10(nnur))+1), flag="0"), sep = ""))
    n.cons.nodes <- as.character(paste0("N.Cons.", formatC(1:ncons, width=(floor(log10(nnur))+1), flag="0"), sep = ""))
    n.nur.nodes <- as.character(paste0("N.Nur.", formatC(1:nnur, width=(floor(log10(nnur))+1), flag="0"), sep = ""))
    n.ret.nodes <- as.character(paste0("N.Ret.", formatC(1:nret, width=(floor(log10(nnur))+1), flag="0"), sep = ""))
    
    #now have list of nurseries. need to generate customer demographics  
    
    nursery.node.df <- data.frame('nodes' = c(n.com.nodes, n.cons.nodes, n.nur.nodes, n.ret.nodes),
                                  'percent.commercial' = NA,
                                  'percent.consumer' = NA,
                                  'percent.nursery' = NA,
                                  'percent.retailer' = NA)
    
    
    #trying to rewrite the percent customers without the for loops 
    # generate main customer, secondary customer, third customer and final customer vectors
    # vector splits: 1:(ncom) , c((ncom + 1):(ncom + ncons)) , c((ncom + ncons + 1):(total.nurseries))
    # rearrange 
    
    
    #
    #     __%com_|_%cons_|_%nur_|_%Ret_|______________  
    #     |       |      |      |      |
    #     |       |      |      |      |
    #ncom | >50%  |      |      |      |
    #ncon |       | >50% |      |      |
    #nnur |       |      |>50%  |      |
    #nret |       |      |      |>50%  |
    # assign each nursery its main customer
    #
    #
    #
    #
    #
    #generate main customer demographic
    customer.1 <- round(runif(total.nurseries, min = 0.5, max = 1), digits = 2)   #sapply(round(runif(total.nurseries, min = 0.5, max = 1), digits = 2), function(x) max(0,x) )
    
    #generate 2nd demographic from what is left
    customer.2 <- round(runif(total.nurseries, min = 0, max = sapply( (1 - customer.1) , function(x) max(0,x))), digits = 2) 
    
    #generate 3rd from what is left
    customer.3 <- round(runif(total.nurseries, min = 0, max = sapply((1 - customer.1 - customer.2), function(x) max(0,x) )) , digits = 2) 
    
    #Last one is agiven
    customer.4 <-  sapply( round((1 - customer.1 - customer.2 - customer.3) , digits = 2) , function(x) max(0,x) ) 
    #customer 1 <- main customer
    
    test<- matrix(c(customer.2, customer.3, customer.4), nrow = total.nurseries, ncol = 3, byrow = FALSE) # matrix rows represent nursery's cust. dem of non-dominant customer
    #randomly assign customer demographics
    test <- matrix(apply( test, 1, function(x) sample(x)), nrow = total.nurseries, ncol = 3, byrow = TRUE) # randomises column positions for each row.
    
    # apply customer demographics to data.frame
    nursery.node.df$percent.commercial[1:(ncom)] = customer.1[1:(ncom)] 
    nursery.node.df[1:(ncom), c(3,4,5) ] = test[1:(ncom),]
    
    nursery.node.df$percent.consumer[c((ncom + 1):(ncom + ncons))] = customer.1[c((ncom + 1):(ncom + ncons))]
    nursery.node.df[c((ncom + 1):(ncom+ncons)), c(2, 4, 5)] = test[c((ncom + 1):(ncom+ncons)),]
    
    
    nursery.node.df$percent.nursery[c((ncom + ncons + 1):(ncom + ncons + nnur))] = customer.1[c((ncom + ncons + 1):(ncom + ncons + nnur))]
    nursery.node.df[c((ncom + ncons + 1):(ncom + ncons + nnur)), c(2, 3, 5)] = test[c((ncom + ncons + 1):(ncom + ncons + nnur)),]
    
    nursery.node.df$percent.retailer[c((ncom+ncons+nnur+1):total.nurseries)] = customer.1[c((ncom+ncons+nnur+1):total.nurseries)]
    nursery.node.df[c((ncom+ncons+nnur+1):total.nurseries), c(2, 3, 4)] = test[c((ncom+ncons+nnur+1):total.nurseries),]  
    
    
    #round percentages to 2 significant figures
    nursery.node.df$percent.commercial <- round(nursery.node.df$percent.commercial, digits = 2)
    nursery.node.df$percent.consumer <- round(nursery.node.df$percent.consumer, digits = 2)
    nursery.node.df$percent.nursery <- round(nursery.node.df$percent.nursery, digits = 2)
    nursery.node.df$percent.retailer <- round(nursery.node.df$percent.retailer, digits = 2)
    
    
    
    #now generate out-degree for nurseries and then generate number of commercial and consumer  nodes
    #simulate out-degrees (number of unique customers in a year) from data mean and variance
    
    #sample mean 103.75
    #sample sd 102.656
    #for each nursery- the out degree will be dictated by truncated normal distribution with upper bound of  total nurseries/%nursery
    
    #uppertrunc gives upper bound based on nurseries
    uppertrunc <- total.nurseries/nursery.node.df$percent.nursery
    #uppertrunc[which(uppertrunc == -Inf)] = Inf
    #uppertrunc2 gives upper bound based on Retailers
    uppertrunc2 <-Ret/nursery.node.df$percent.retailer
    #uppertrunc2[which(uppertrunc2 == - Inf)] = Inf
    # we use this upper bound as a nursery can only sell to every other nursery in the network at most. 
    #we minus 1 here as a nursery cannot sell to itself.
    uppertrunc3 <- floor(mapply(function(x,y) min(x,y), x = (uppertrunc-1), y = uppertrunc2))
    dummy4 <- round(sapply((uppertrunc3), function(x) rnegbinomt(1, c(1, min(x, 3000) ), size = out.deg.dispersal, mu = out.deg.mean ) ))
    
    # dummy <- round(sapply((uppertrunc - 1), function(x) rnegbinomt(1, c(0, min(x, 3000) ), size = out.deg.dispersal, mu = out.deg.mean ) ))
    
    
    # dummy gives the out.degree of each nursery bounded by the number of nurseries in the network
    
    #this gives number of nurseries each nursery trades with
    #for each nursery, its out degree is simulated from truncated normal dist. min = 0.5, max = total.nurseries/%nursery - 1 so as to not assign more links
    # to nurseries than there are.
    
    #find minimum between these two potential out.degrees so we do not have too many links going to not enough nurseries OR retailers
    #mapply(function(x,y) min(x, y), x = dummy, dummy2 )
    
    nursery.node.df$number.commercial <- floor(dummy4 * nursery.node.df$percent.commercial)
    nursery.node.df$number.consumer <-  floor(dummy4 * nursery.node.df$percent.consumer)
    nursery.node.df$number.nursery <- floor(dummy4 * nursery.node.df$percent.nursery)
    nursery.node.df$number.retailer <- floor(dummy4 * nursery.node.df$percent.retailer)
    nursery.node.df$out.degree <- nursery.node.df$number.commercial + nursery.node.df$number.consumer +
      nursery.node.df$number.nursery + nursery.node.df$number.retailer
    
    #Ret <- sum(nursery.node.df$number.retailer)
    #if(sum(nursery.node.df$number.retailer) == 0){
    
    #generate data frame for number of retailer nodes and their out.degree
    retailer.nodes <- data.frame("Nodes" = as.character(paste0("Retailer.", formatC(1:Ret, width=(floor(log10(Ret)+1)), flag="0"), sep = "")),
                                 "number.consumer" =  rnegbinomt(Ret, c(1, 3000 ), size = (out.deg.dispersal), mu = out.deg.mean ))
    #sum(retailer.nodes$number.consumer)
    #imposing maximum out.deg for retailers and nurseries of 3000
    
    
    #number of commercial + consumer nodes are now given.
    #format code just makes sure the numbering works like 01, 02 to help with ordering in data.frames
    commercial.nodes <-  as.character(paste0("Commercial.", formatC(1:sum(nursery.node.df$number.commercial), width=(floor(log10(sum(nursery.node.df$number.commercial)))+1), flag="0"), sep = ""))
    consumer.nodes <- as.character(paste0("Consumer.", formatC(1:(sum(nursery.node.df$number.consumer)+sum(retailer.nodes$number.consumer)), width=(floor(log10((sum(nursery.node.df$number.consumer)+sum(retailer.nodes$number.consumer))))+1), flag="0"), sep = ""))
    
    
    
    #now have list of nodes in a list, I need to assign connections
    #assigning links to nurseries
    # pick nursery: look at links designated to nurseries: randomly assign without replacement that number of nurseries then store those chosen ones as assigned links
    # : then pick next nursery
    #listing nurseries per number of times they have a nursery connection
    nursery.connections.from <- rep(as.character(nursery.node.df$nodes), nursery.node.df$number.nursery)
    nursery.connections.to <- c()
    nursery.connections.to <-  unlist(sapply(unique(nursery.connections.from), function(x) sample( as.character(nursery.node.df$nodes[-which(nursery.node.df$nodes == x)]),
                                                                                                   size = length(nursery.connections.from[which(nursery.connections.from == x)]), replace = FALSE ) )) 
    
    
    #-------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #assigning links to retailers
    
    # only nurseries sell to retailers
    retailer.connections.from <- rep(as.character(nursery.node.df$nodes), nursery.node.df$number.retailer)
    #retailers buy from multiple nurseries 
    retailer.connections.to <- (unlist(sapply( nursery.node.df$number.retailer[nursery.node.df$number.retailer>0] , function(x) sample(retailer.nodes$Nodes, size = x, replace = FALSE ))))
    
    
    #-------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # assigning commercial links
    
    # assuming commercial companies only buy from nurseries
    commercial.connections.from <- rep(as.character(nursery.node.df$nodes), nursery.node.df$number.commercial)
    commercial.connections.to <- sample(commercial.nodes,
                                        size = length(commercial.connections.from), replace = FALSE)  
    
    #-------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #need to fix this one. nursery node and retailer nodes are separated.
    #assigning consumer links
    #nurseries and retailers sell to consumers
    consumer.connections.from <- rep(c(as.character(nursery.node.df$nodes), as.character(retailer.nodes$Nodes)),
                                     c(nursery.node.df$number.consumer, retailer.nodes$number.consumer))
    consumer.connections.to <- sample(as.character(consumer.nodes),
                                      size = length(consumer.connections.from), replace = FALSE) 
   
    
    # colour scheme: COM    , CONS   , N_COM  , N_CONS , N_NUR  , N_RET  ,  Ret
    #                #F0F757, #89043D, #228B22, #DB7093, #00008B, #000000, #98D2EB
    #                Yellow  , Dark red , Green  , Pink   , Blue, Black  , Sky Blue
    

    net.nodes <- data.frame( "ID" = c( as.character(commercial.nodes), as.character(consumer.nodes), as.character(nursery.node.df$nodes), as.character(retailer.nodes$Nodes)),
                             "Col" = c( rep("#F0F757", length(commercial.nodes)),
                                        rep("#89043D", length(consumer.nodes)),
                                        rep("#228B22",(ncom)), rep("#DB7093", (ncons) ), rep("#00008B",nnur), rep("#0A0908" , nret), 
                                        rep("#98D2EB", length(retailer.nodes$Nodes))), 
                             "Type" = c(rep("Commercial", length(commercial.nodes)), rep("Consumer", length(consumer.nodes)), rep("N.Com", (ncom)),
                                        rep("N.Cons", (ncons)), rep("N.Nur", (nnur)), rep("N.Ret", nret),  rep("Retailer", length(retailer.nodes$Nodes))),
                             
                             "Capacity" = NA)
    
    #
    
    #Explanation for below:
    # lognormal Random Variable X with  ln(x) ~ N(mu, s^2)
    
    # We can express mu and s with the mean and sd of X using the formulas below:
    
    com.weight.dist.mean <- log(commercial.mean^2/sqrt(commercial.mean^2 +commercial.sd^2))
    com.weight.dist.sd <- sqrt(log(1+(commercial.sd/commercial.mean)^2 ))
    cons.weight.dist.mean <- log(consumer.mean^2/sqrt(consumer.mean^2 +consumer.sd^2))
    cons.weight.dist.sd <- sqrt(log(1+(consumer.sd/consumer.mean)^2 ))
    nur.weight.dist.mean <- log(nursery.mean^2/sqrt(nursery.mean^2 +nursery.sd^2))
    nur.weight.dist.sd <- sqrt(log(1+(nursery.sd/nursery.mean)^2 ))
    ret.weight.dist.mean <- log(retailer.mean^2/sqrt(retailer.mean^2 +retailer.sd^2))
    ret.weight.dist.sd <- sqrt(log(1+(retailer.sd/retailer.mean)^2 ))
    #just need to assign weights from capacities here
    net.connections <- data.frame("From" = c(as.character(commercial.connections.from), as.character(consumer.connections.from),
                                             as.character(nursery.connections.from), as.character(retailer.connections.from)) , 
                                  "To" = c(as.character(commercial.connections.to), as.character(consumer.connections.to),
                                           as.character(nursery.connections.to), as.character(retailer.connections.to)), 
                                  "weight" = NA)
    
    
    #to assign weights
    # create distributions for weights of commercial, consumer, nursery, retailer

    #customer weight distributions:
    commercial.weight.dist <- round(exp(rtruncnorm(length(as.character(commercial.connections.to)), a=0, b= log(10^4), com.weight.dist.mean, sd = com.weight.dist.sd)))
    consumer.weight.dist <- round(exp(rtruncnorm(length(as.character(consumer.connections.to)),  a=0, b= log(10^4), cons.weight.dist.mean , sd = cons.weight.dist.sd)))
    nursery.weight.dist <- round(exp(rtruncnorm(length(as.character(nursery.connections.to)), a=0, b= log(10^5), nur.weight.dist.mean , sd = nur.weight.dist.sd )))
    retailer.weight.dist <- round(exp(rtruncnorm(length(as.character(retailer.connections.to)),  a=0, b= log(10^5), ret.weight.dist.mean , sd = ret.weight.dist.sd )))
    
    net.connections$weight = c(commercial.weight.dist, consumer.weight.dist, nursery.weight.dist, retailer.weight.dist)
    
    #Create igraph object  
    net <-  graph_from_data_frame( d = as.data.frame(net.connections), vertices = net.nodes, directed = T)
    
    #Delete nodes with zero in-degree
    net <- delete_vertices(net, v = names(which(degree(net, mode = "in") == 0) ))
    net <- delete_vertices(net, v = names(which(degree(net, mode = "in") == 0) ))
    
    return(net)
  })
}








#------------------------------------------
#End of Script
#------------------------------------------