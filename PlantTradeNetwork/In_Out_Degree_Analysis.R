# run 100 networks with baseline, retain the out-degrees for the nurseries and retailers 

source("Network_Functions.R")


#160 nurseries

total.nurseries <- 160


#baseline
node.list <- list(ncom = 40,ncons = 40, nnur = 40, nret = 40, Ret = 160*10)


oak.distribution.list.1 <- list(out.deg.mean = 380*(160/630), out.deg.dispersal = (160/630)*(380)^2/(114587*(160/630) - 380),
                                commercial.mean = 79*(160/630) , commercial.sd = 297*(160/630),
                                consumer.mean = 185*(160/630), consumer.sd = 822*(160/630),
                                nursery.mean = 1844*(160/630) , nursery.sd = 3567*(160/630),
                                retailer.mean = 3497*(160/630), retailer.sd = 9588*(160/630))


 n.com.out.deg <- c()
  n.cons.out.deg <- c()
  n.nur.out.deg <- c()
  n.ret.out.deg <- c()
  retailer.out.deg <- c()
  com.in.deg <- c()
  cons.in.deg <- c()
  n.com.in.deg <- c()
  n.cons.in.deg <- c()
  n.nur.in.deg <- c()
  n.ret.in.deg <- c()
  retailer.in.deg <- c()

 
  for (i in 1:100) {
    
    
    
    
    #change node.list for different scenario
    oak.net<- netgen(node.list, oak.distribution.list.1)
    
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
    
    #oak.net.N.R.subgraph <- induced_subgraph(oak.net, v = V(oak.net)[which(substr(vertex_attr(oak.net, name = "Type"), 1, 1) %in% c("N", "R"))])
    
    
    
    
    out.deg.1 <- as.numeric(degree(oak.net, mode = "out"))
    in.deg.1 <- as.numeric(degree(oak.net, mode = "in"))
  
    
    
    n.com.out.deg <- c(n.com.out.deg, out.deg.1[N.com.pos]   )
    n.cons.out.deg <- c(n.cons.out.deg, out.deg.1[N.cons.pos]   )
    n.nur.out.deg <- c(n.nur.out.deg, out.deg.1[N.nur.pos]   )
    n.ret.out.deg <- c(n.ret.out.deg, out.deg.1[N.ret.pos]   )
    retailer.out.deg <- c(retailer.out.deg, out.deg.1[ret.pos]   )
  #  com.in.deg <- c(com.in.deg, in.deg.1[com.pos])
  #  cons.in.deg <- c(cons.in.deg, in.deg.1[cons.pos])   
    n.com.in.deg <- c(n.com.in.deg, in.deg.1[N.com.pos]   )
    n.cons.in.deg <-  c(n.cons.in.deg, in.deg.1[N.cons.pos]   )
    n.nur.in.deg <- c(n.nur.in.deg, in.deg.1[N.nur.pos]   )
    n.ret.in.deg <- c(n.ret.in.deg, in.deg.1[N.ret.pos]   )
    retailer.in.deg <- c(retailer.in.deg, in.deg.1[ret.pos]   )
    
  
    
  }
 

# nursery.out.degrees.df <- matrix(NA, nrow = 40*100, ncol = 4)
# 
# nursery.out.degrees.df[,1] = n.com.out.deg
# nursery.out.degrees.df[,2] = n.cons.out.deg
# nursery.out.degrees.df[,3] = n.nur.out.deg
# nursery.out.degrees.df[,4] = n.ret.out.deg


nursery.in.degrees.df <- matrix(NA, nrow = 40*100, ncol = 4)

nursery.in.degrees.df[,1] = n.com.in.deg
nursery.in.degrees.df[,2] = n.cons.in.deg
nursery.in.degrees.df[,3] = n.nur.in.deg
nursery.in.degrees.df[,4] = n.ret.in.deg



max(n.ret.out.deg)

# save(nursery.out.degrees.df, file = "nursery_out_degrees_df.RData" )
# save(nursery.in.degrees.df, file = "nursery_in_degrees_df.RData" )
# save(retailer.out.deg, file = "retailer_out_deg.RData" )
# save(retailer.in.deg, file = "retailer_in_deg.RData" )





#plots

#out-degrees
par(mfrow = c(3,2))
plot(density(n.com.out.deg), xlab = "Out-degree", ylab = "Density", xlim = c(0,800), main =  parse(text=sprintf('paste(N[Com],  " nodes")')))
plot(density(n.cons.out.deg), xlab = "Out-degree", ylab = "Density", xlim = c(0,800), main =  parse(text=sprintf('paste(N[Cons],  " nodes")')))
plot(density(n.nur.out.deg), xlab = "Out-degree", ylab = "Density", xlim = c(0,800), main =  parse(text=sprintf('paste(N[Nur],  " nodes")')))
plot(density(n.ret.out.deg), xlab = "Out-degree", ylab = "Density", xlim = c(0,800), main =  parse(text=sprintf('paste(N[Ret],  " nodes")')))
plot(density(retailer.out.deg), xlab = "Out-degree", ylab = "Density", xlim = c(0,800), main =  parse(text=sprintf('paste(Retailer,  " nodes")')))


par(mfrow = c(3,2), cex.main = 2, cex.axis = 2, cex.lab = 2, mai = c(1, 1, 0.5, 0.5))
hist(log10(n.com.out.deg), xlab = parse(text=sprintf('paste(log[10],  " (Out-degree)")')), ylab = "Density", xlim = c(0,4),ylim = c(0,1.5), main =  parse(text=sprintf('paste(N[Com],  " nodes")')), freq = FALSE)
hist(log10(n.cons.out.deg), xlab = parse(text=sprintf('paste(log[10],  " (Out-degree)")')), ylab = "Density", xlim = c(0,4),ylim = c(0,1.5), main =  parse(text=sprintf('paste(N[Cons],  " nodes")')), freq = FALSE)
hist(log10(n.nur.out.deg), xlab = parse(text=sprintf('paste(log[10],  " (Out-degree)")')), ylab = "Density", xlim = c(0,4),ylim = c(0,1.5), main =  parse(text=sprintf('paste(N[Nur],  " nodes")')), freq = FALSE)
hist(log10(n.ret.out.deg), xlab = parse(text=sprintf('paste(log[10],  " (Out-degree)")')), ylab = "Density", xlim = c(0,4),ylim = c(0,1.5), main =  parse(text=sprintf('paste(N[Ret],  " nodes")')), freq = FALSE)
hist(log10(retailer.out.deg), xlab = parse(text=sprintf('paste(log[10],  " (Out-degree)")')), ylab = "Density", xlim = c(0,4),ylim = c(0,1.5), main =  parse(text=sprintf('paste(Retailer,  " nodes")')), freq = FALSE)
dev.off()



max(retailer.in.deg)
#in-degrees


par(mfrow = c(3,2))
plot(density(n.com.in.deg), xlab = "in-degree", ylab = "Density", xlim = c(0,40), main =  parse(text=sprintf('paste(N[Com],  " nodes")')))
plot(density(n.cons.in.deg), xlab = "in-degree", ylab = "Density", xlim = c(0,40), main =  parse(text=sprintf('paste(N[Cons],  " nodes")')))
plot(density(n.nur.in.deg), xlab = "in-degree", ylab = "Density", xlim = c(0,40), main =  parse(text=sprintf('paste(N[Nur],  " nodes")')))
plot(density(n.ret.in.deg), xlab = "in-degree", ylab = "Density", xlim = c(0,40), main =  parse(text=sprintf('paste(N[Ret],  " nodes")')))
plot(density(retailer.in.deg), xlab = "in-degree", ylab = "Density", xlim = c(0,40), main =  parse(text=sprintf('paste(Retailer,  " nodes")')))


par(mfrow = c(3,2), cex.main = 2, cex.axis = 2, cex.lab = 2, mai = c(1, 1, 0.5, 0.5))
hist(log10(n.com.in.deg), xlab = parse(text=sprintf('paste(log[10],  " (In-degree)")')), ylab = "Density", xlim = c(0,2), ylim = c(0,6), main =  parse(text=sprintf('paste(N[Com],  " nodes")')), freq = FALSE)
hist(log10(n.cons.in.deg), xlab = parse(text=sprintf('paste(log[10],  " (In-degree)")')), ylab = "Density", xlim = c(0,2),ylim = c(0,6), main =  parse(text=sprintf('paste(N[Cons],  " nodes")')), freq = FALSE)
hist(log10(n.nur.in.deg), xlab = parse(text=sprintf('paste(log[10],  " (In-degree)")')), ylab = "Density", xlim = c(0,2),ylim = c(0,6), main =  parse(text=sprintf('paste(N[Nur],  " nodes")')), freq = FALSE)
hist(log10(n.ret.in.deg), xlab = parse(text=sprintf('paste(log[10],  " (In-degree)")')), ylab = "Density", xlim = c(0,2),ylim = c(0,6), main =  parse(text=sprintf('paste(N[Ret],  " nodes")')), freq = FALSE)
hist(log10(retailer.in.deg), xlab = parse(text=sprintf('paste(log[10],  " (In-degree)")')), ylab = "Density", xlim = c(0,2),ylim = c(0,6), main =  parse(text=sprintf('paste(Retailer,  " nodes")')), freq = FALSE)


par(mfrow = c(1,2), cex.main = 2, cex.axis = 2, cex.lab = 2, mai = c(1, 1, 0.5, 0.5)  )
hist(log10(c(n.com.out.deg, n.cons.out.deg, n.nur.out.deg, n.ret.out.deg, retailer.out.deg )), xlab = parse(text=sprintf('paste(log[10],  " (Out-degree)")')), ylab = "Density", xlim = c(0,5), ylim = c(0,1), main =  "Out-degrees for nurseries and retailers", freq = FALSE)

hist(log10(c(n.com.in.deg, n.cons.in.deg, n.nur.in.deg, n.ret.in.deg, retailer.in.deg )), xlab = parse(text=sprintf('paste(log[10],  " (In-degree)")')), ylab = "Density", xlim = c(0,2), ylim = c(0,3), main =  "In-degrees for nurseries and retailers", freq = FALSE)

library(ggplot2)


#out-degrees histograms

p.1.out <- ggplot(data = data.frame("N.Com.out.deg" = (n.com.out.deg)), aes(x =  N.Com.out.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.2.out <- ggplot(data = data.frame("N.Cons.out.deg" = (n.cons.out.deg)), aes(x =  N.Cons.out.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.3.out <- ggplot(data = data.frame("N.Nur.out.deg" = (n.nur.out.deg)), aes(x =  N.Nur.out.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.4.out <- ggplot(data = data.frame("N.Ret.out.deg" = (n.ret.out.deg)), aes(x =  N.Ret.out.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.5.out <- ggplot(data = data.frame("Retailer.out.deg" = (retailer.out.deg)), aes(x =  Retailer.out.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()





#in-degrees histograms

p.1.in <- ggplot(data = data.frame("N.Com.in.deg" = (n.com.in.deg)), aes(x =  N.Com.in.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.2.in <- ggplot(data = data.frame("N.Cons.in.deg" = (n.cons.in.deg)), aes(x =  N.Cons.in.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.3.in <- ggplot(data = data.frame("N.Nur.in.deg" = (n.nur.in.deg)), aes(x =  N.Nur.in.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.4.in <- ggplot(data = data.frame("N.Ret.in.deg" = (n.ret.in.deg)), aes(x =  N.Ret.in.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()

p.5.in <- ggplot(data = data.frame("Retailer.in.deg" = (retailer.in.deg)), aes(x =  Retailer.in.deg )) +
  geom_histogram(color = "black", fill = "grey") +theme_classic()






