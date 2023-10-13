# plan is to run different distributions of nurseries, focus on oak, run a bunch of measures

# measures to look at
# betweenness - weights interpreted as distances
# node strength distribution
# hubs, authorities - weights mean strengths
#
library(tidyverse)

a<- getwd()

setwd(paste0(a, "/Data"))
for (k in 1:7) {
#   

load(paste0(paste0("betweenness_df_", k), ".Rdata"))
load(paste0(paste0("betweenness_subgraph_df_", k), ".Rdata"))
load(paste0(paste0("betweenness_unweighted_df_", k), ".Rdata"))
load(paste0(paste0("betweenness_unweighted_subgraph_df_", k), ".Rdata"))
load(paste0(paste0("str_in_df_", k), ".Rdata"))
load(paste0(paste0("str_out_df_", k), ".Rdata"))
load(paste0(paste0("str_in_subgraph_df_", k), ".Rdata"))
load(paste0(paste0("str_out_subgraph_df_", k), ".Rdata"))
load(paste0(paste0("as_df_", k), ".Rdata"))
load(paste0(paste0("hs_df_", k), ".Rdata"))
load(paste0(paste0("as_subgraph_df_", k), ".Rdata"))
load(paste0(paste0("hs_subgraph_df_", k), ".Rdata"))
load(paste0(paste0("as_unweighted_df_", k), ".Rdata"))
load(paste0(paste0("hs_unweighted_df_", k), ".Rdata"))
load(paste0(paste0("as_subgraph_unweighted_df_", k), ".Rdata"))
load(paste0(paste0("hs_subgraph_unweighted_df_", k), ".Rdata"))

}



setwd(paste0(a, "/Figures"))
#plotting full network against network subset

for (k in 1:7) {
  
  
  
#in-strength  
png(filename = paste0(paste0("str_in_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( InStrength ~ type, data = (log10(1 + get(paste0("str.in.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "InStrength")), xlab = "", xaxt = "n", ylab = "In-Strength", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( InStrength ~ type, data = (log10(1 + get(paste0("str.in.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "InStrength")), xlab = "", xaxt = "n", ylab = "In-Strength", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()
# #
# 
# #out-strength
png(filename = paste0(paste0("str_out_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( OutStrength ~ type, data = (log10(1 + get(paste0("str.out.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "OutStrength")), xlab = "", xaxt = "n", ylab = "Out-Strength", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( OutStrength ~ type, data = (log10(1 + get(paste0("str.out.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "OutStrength")), xlab = "", xaxt = "n", ylab = "Out-Strength", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()
#

#in-degree  
png(filename = paste0(paste0("in_deg_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( InDegree ~ type, data = (log10(1 + get(paste0("in.deg.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "InDegree")), xlab = "", xaxt = "n", ylab = "In-degree", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( InDegree ~ type, data = (log10(1 + get(paste0("in.deg.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "InDegree")), xlab = "", xaxt = "n", ylab = "In-degree", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()
# #
# 
# #out-degree
png(filename = paste0(paste0("out_deg_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( OutDegree ~ type, data = (log10(1 + get(paste0("out.deg.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "OutDegree")), xlab = "", xaxt = "n", ylab = "Out-degree", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( OutDegree ~ type, data = (log10(1 + get(paste0("out.deg.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "OutDegree")), xlab = "", xaxt = "n", ylab = "Out-degree", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()



#
#
#betweenness
png(filename = paste0(paste0("betweenness", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( Betweenness ~ type, data = (log10(1 + get(paste0("betweenness.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Betweenness")), xlab = "", xaxt = "n", ylab = "Betweenness", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( Betweenness ~ type, data = (log10(1 + get(paste0("betweenness.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Betweenness")), xlab = "", xaxt = "n", ylab = "Betweenness", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()


#betweenness unweighted
png(filename = paste0(paste0("betweenness_unweighted", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( Betweenness ~ type, data = (log10(1 + get(paste0("betweenness.unweighted.df.",k))[,3:7]) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Betweenness")), xlab = "", xaxt = "n", ylab = "Unweighted betweenness", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( Betweenness ~ type, data = (log10(1 + get(paste0("betweenness.unweighted.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Betweenness")), xlab = "", xaxt = "n", ylab = "Unweighted betweenness", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()

  

# authority score
png(filename = paste0(paste0("as_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( Authorityscore ~ type, data = (log10(1 + get(paste0("as.df.",k))) %>% as_tibble() %>%  setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>%
                                          pivot_longer(N.Com:Ret, names_to = "type", values_to = "Authorityscore")),
         xlab = "", xaxt = "n", ylab = "Authority score", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( Authorityscore ~ type, data = (log10(1 + get(paste0("as.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Authorityscore")), xlab = "", xaxt = "n", ylab = "Authority score", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()

# unweighted authority score
png(filename = paste0(paste0("as_unweighted_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( Authorityscore ~ type, data = (log10(1 + get(paste0("as.unweighted.df.",k))[,1:5]) %>% as_tibble() %>%
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>%
                                         pivot_longer(N.Com:Ret, names_to = "type", values_to = "Authorityscore")),
        xlab = "", xaxt = "n", ylab = "unweighted authority score", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( Authorityscore ~ type, data = (log10(1 + get(paste0("as.subgraph.unweighted.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Authorityscore")), xlab = "", xaxt = "n", ylab = "unweighted authority score", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()

# hub score
png(filename = paste0(paste0("hs_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( Hubscore ~ type, data = (log10(1 + get(paste0("hs.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Hubscore")), xlab = "", xaxt = "n", ylab = "Hub score", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( Hubscore ~ type, data = (log10(1 + get(paste0("hs.subgraph.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Hubscore")), xlab = "", xaxt = "n", ylab = "Hub score", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()


# unweighted hub score
png(filename = paste0(paste0("hs_unweighted_", k), ".png"), width = 2300, height = 1764 )
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)
boxplot( Hubscore ~ type, data = (log10(1 + get(paste0("hs.unweighted.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Hubscore")), xlab = "", xaxt = "n", ylab = "unweighted hub score", main = "Full network")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
boxplot( Hubscore ~ type, data = (log10(1 + get(paste0("hs.subgraph.unweighted.df.",k))) %>% as_tibble() %>% setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "Hubscore")), xlab = "", xaxt = "n", ylab = "unweighted hub score", main = "Network without customers")
axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer") )
dev.off()

  
}



# plotting all scenarios for the network subset

#In-strength for subset
  png(filename = "In_Strength_Scenarios_Subgraph_Boxplot.png", width = 2500, height = 3000 )
  par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )
  
  for (k in 1:7) {
    
    dat <- log10(1 + get(paste0("str.in.subgraph.df.",k)))
    
    boxplot( PageRank ~ type, data = (log10(1 + get(paste0("str.in.subgraph.df.",k))) %>% as_tibble() %>%
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>%
                                        pivot_longer(N.Com:Ret, names_to = "type", values_to = "PageRank")),
             xlab = "", ylab = "In-strength", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(3.2,4.6))
    axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer"), cex.axis = 8 )
    axis(2, at = c(3.2,4.6), labels = c(3.2,4.6)  )
  }
  dev.off()
  
  
  
  #Out-strength for subset  
  png(filename = "Out_Strength_Scenarios_Subgraph_Boxplot.png", width = 2500, height = 3000 )
  par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )
  
  for (k in 1:7) {
    
    dat <- log10(1 + get(paste0("str.out.subgraph.df.",k)))
    
    boxplot( PageRank ~ type, data = (log10(1 + get(paste0("str.out.subgraph.df.",k))[,1:4]) %>% as_tibble() %>%
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                        pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")), xlab = "",
             ylab = "Out-strength", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim =  c(3.5,5.5))
    axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
    axis(2, at = c(3.5,5.5), labels = c(3.5,5.5)  )
  }
  dev.off()
  
  
  #hub_score for subset
  png(filename = "hs_Scenarios_Subgraph_Boxplot.png", width = 2500, height = 3000 )
  par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )
  
  for (k in 1:7) {
    
    dat <- log10(1 + get(paste0("hs.subgraph.df.",k)))
    
    boxplot( PageRank ~ type, data = (log10(1 + get(paste0("hs.subgraph.df.",k))[,1:4]) %>% as_tibble() %>% 
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                        pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
             xlab = "", ylab = "Hub score", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(0,0.1))
    axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
    axis(2, at = c(0,0.1), labels = c(0,0.1)  )
  }
  dev.off()
  
  
  #Authority Score for subset
  png(filename = "as_Scenarios_Subgraph_Boxplot.png", width = 2500, height = 3000 )
  par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )
  
  for (k in 1:7) {
    
    dat <- log10(1 + get(paste0("as.subgraph.df.",k)))
    
    boxplot( PageRank ~ type, data = (log10(1 + get(paste0("as.subgraph.df.",k))) %>% as_tibble() %>% 
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% 
                                        pivot_longer(N.Com:Ret, names_to = "type", values_to = "PageRank")),
             xlab = "", ylab = "Authority score", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k))
    axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer"), cex.axis = 8 )
    axis(2, at = c(0, signif(max(dat), 2)), labels = c(0, signif(max(dat), 2))  )
  }
  dev.off()
  
  
  
  #Betweenness score for subset
  png(filename = "betweenness_Scenarios_Subgraph_Boxplot.png", width = 2500, height = 3000 )
  par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )
  
  for (k in 1:7) {
    
    dat <- log10(1 + get(paste0("betweenness.subgraph.df.",k)))
    
    boxplot( PageRank ~ type, data = (log10(1 + get(paste0("betweenness.subgraph.df.",k))[,1:4]) %>% as_tibble() %>%
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>% 
                                        pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")), xlab = "",
             ylab = "Betweenness score", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(2.5,4.5))
    axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
    axis(2, at = c(2.5,4.5), labels = c(2.5,4.5)  )
  }
  dev.off()
  
  
  #Unweighted Betweenness score for subset
  png(filename = "betweenness_unweighted_Scenarios_Subgraph_Boxplot.png", width = 2500, height = 3000 )
  par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )
  
  for (k in 1:7) {
    
    dat <- log10(1 + get(paste0("betweenness.unweighted.subgraph.df.",k)))
    
    boxplot( PageRank ~ type, data = (log10(1 + get(paste0("betweenness.unweighted.subgraph.df.",k))[,1:4]) %>% as_tibble() %>%
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                        pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
             xlab = "", ylab = "Unweighted betweenness", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(2.5,4.5))
    axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
    axis(2, at = c(2.5, 4.5), labels = c(2.5, 4.5) )
  }
  dev.off()
  
  #Unweighted authortiy score for subset
png(filename = "as_unweighted_subgraph_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6 )

for (k in 1:7) {
  
  
  boxplot( PageRank ~ type, data = ((get(paste0("as.subgraph.unweighted.df.",k))) %>% as_tibble() %>% 
           setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "PageRank")),
           xlab = "", ylab = "Unweighted authority score", ylim = c(0,1),
           xaxt = "n", yaxt = "n", main = paste0("Scenario ", k))
  axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer"), cex.axis = 8 )
  axis(2, at = c(0, 1), labels = c(0, 1)  )
}
dev.off()


#Unweighted hub score for subset
png(filename = "hs_unweighted_subgraph_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6 )

for (k in 1:7) {
  
  
  boxplot( PageRank ~ type, data = ((get(paste0("hs.subgraph.unweighted.df.",k))[,1:4]) %>% as_tibble() %>% 
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                      pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
           xlab = "", ylab = "Unweighted hub score", ylim = c(0,0.6),
           xaxt = "n", yaxt = "n", main = paste0("Scenario ", k))
  axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
  axis(2, at = c(0, 0.6), labels = c(0, 0.6)  )
}
dev.off()



# scenarios for full network -----

#In-strength 
png(filename = "In_Strength_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  boxplot( InStrength ~ type, data = (log10(1 + get(paste0("str.in.df.",k))) %>% as_tibble() %>%
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>%
                                      pivot_longer(N.Com:Ret, names_to = "type", values_to = "InStrength")),
           xlab = "", ylab = "In-strength", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(3,4.5))
  axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer"), cex.axis = 8 )
  axis(2, at = c(3,4.5), labels = c(3,4.5)  )
}
dev.off()


#In-degree
png(filename = "In_Degree_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  boxplot( InDegree ~ type, data = (log10(1 + get(paste0("in.deg.df.",k))) %>% as_tibble() %>%
                                        setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>%
                                        pivot_longer(N.Com:Ret, names_to = "type", values_to = "InDegree")),
           xlab = "", ylab = "In-degree", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(0.3,1.8))
  axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer"), cex.axis = 8 )
  axis(2, at = c(0.3,1.8), labels = c(0.3,1.8)  )
}
dev.off()



#Out-strength
png(filename = "Out_Strength_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  
  
  boxplot( PageRank ~ type, data = (log10(1+ get(paste0("str.out.df.",k))) %>% as_tibble() %>%
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>%
                                      pivot_longer(N.Com:Ret, names_to = "type", values_to = "PageRank")), xlab = "",
           ylab = "Out-strength", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim =  c(3.5,5.5))
  axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer" ), cex.axis = 8 )
  axis(2, at = c(3.5,5.5), labels = c(3.5,5.5)  )
}
dev.off()

#Out-degree
png(filename = "Out_Degree_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  
  
  boxplot( PageRank ~ type, data = (log10(1+ get(paste0("out.deg.df.",k))) %>% as_tibble() %>%
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>%
                                      pivot_longer(N.Com:Ret, names_to = "type", values_to = "PageRank")), xlab = "",
           ylab = "Out-degree", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim =  c(1.5,2.3))
  axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer" ), cex.axis = 8 )
  axis(2, at = c(1.5,2.3), labels = c(1.5,2.3)  )
}
dev.off()



#Hub-score
png(filename = "hs_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  
  boxplot( PageRank ~ type, data = (log10(1+get(paste0("hs.df.",k))[,1:4]) %>% as_tibble() %>% 
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                      pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
           xlab = "", ylab = "Hub score", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(0,0.1))
  axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
  axis(2, at = c(0,0.1), labels = c(0,0.1)  )
}
dev.off()


#Authority-score
png(filename = "as_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  dat <- log10(1 + get(paste0("as.df.",k)))
  boxplot( PageRank ~ type, data = (log10(1 + get(paste0("as.df.",k))) %>% as_tibble() %>% 
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% 
                                      pivot_longer(N.Com:Ret, names_to = "type", values_to = "PageRank")),
           xlab = "", ylab = "Authority score", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(0,max(dat)))
  axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer"), cex.axis = 8 )
  axis(2, at = c(0,signif(max(dat), 2)), labels = c(0,signif(max(dat), 2))  )
}
dev.off()


#Betweenness score
png(filename = "betweenness_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  
  boxplot( PageRank ~ type, data = (log10(1 + get(paste0("betweenness.df.",k))[,1:4]) %>% as_tibble() %>%
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>% 
                                      pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")), xlab = "",
           ylab = "Betweenness score", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(3.5,6.5))
  axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
  axis(2, at = c(3.5,6.5), labels = c(3.5,6.5)  )
}
dev.off()


#Unweighted Betweenness score
png(filename = "betweenness_unweighted_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6, c(5.1, 4.1, 4.1, 2.1) )

for (k in 1:7) {
  
  dat <- log10(get(paste0("betweenness.unweighted.df.",k)))
  
  boxplot( PageRank ~ type, data = (log10(1 + get(paste0("betweenness.unweighted.df.",k))[,3:6]) %>% as_tibble() %>%
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                      pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
           xlab = "", ylab = "Unweighted betweenness", xaxt = "n", yaxt = "n", main = paste0("Scenario ", k), ylim = c(4, 6.5))
  axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
  axis(2, at = c(4, 6.5), labels = c(4, 6.5) )
}
dev.off()




#Unweighted Hub score
png(filename = "hs_unweighted_Scenarios_Boxplot.png", width = 2500, height = 3000 )

par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 6, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6 )
for (k in 1:7) {
  
  #dat <- log10(1 + get(paste0("as.subgraph.unweighted.df.",k)))
  
  boxplot( PageRank ~ type, data = (log10(1 + get(paste0("hs.unweighted.df.",k))[,1:4]) %>% as_tibble() %>% 
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                      pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
           xlab = "", ylab = "Unweighted hub score", ylim = c(0,0.3),
           xaxt = "n", yaxt = "n", main = paste0("Scenario ", k))
  axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 8 )
  axis(2, at = c(0, 0.3), labels = c(0, 0.3)  )
}

dev.off()



#Unweighted Authority score
png(filename = "as_unweighted_Scenarios_Boxplot.png", width = 2500, height = 3000 )
par(mfrow = c(4, 2), cex.axis = 4, cex.lab = 4, mai = c(3, 3, 1, 1), mgp = c(16, 9, 0), cex.main = 6 )

for (k in 1:7) {
  
  #dat <- log10(1 + get(paste0("as.subgraph.unweighted.df.",k)))
  
  boxplot( PageRank ~ type, data = (log10(1 + get(paste0("as.unweighted.df.",k))) %>% as_tibble() %>% 
                                      setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret", "Ret")) %>% pivot_longer(N.Com:Ret, names_to = "type", values_to = "PageRank")),
           xlab = "", ylab = "Unweighted authority score", ylim = c(0,0.3),
           xaxt = "n", yaxt = "n", main = paste0("Scenario ", k))
  axis(1, at = 1:5, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret]), "Retailer"), cex.axis = 8 )
  axis(2, at = c(0, 0.3), labels = c(0, 0.3)  )
}
dev.off()





# ECMTB 2022 boxplots 
{
#png(filename = "ECMTB_Hub_score_boxplots.png", width = 2*3000, height = 3000, res = 320 )
  setEPS()
  postscript(file = "ECMTB_Hub_score_boxplots.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 16, height = 8)  
par(mfrow = c(1, 2), cex.axis = 3, cex.lab = 3, mai = c(2, 1.5, 1, 1), mgp = c(5, 2, 0), cex.main = 3)

boxplot( PageRank ~ type, data = (log10(1+get(paste0("hs.df.",k))[,1:4]) %>% as_tibble() %>% 
                                    setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                    pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
         xlab = "", ylab = "Hub score", xaxt = "n", yaxt = "n", main = "Weighted network", ylim = c(0,0.05))
axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 3 )
axis(2, at = c(0,0.05), labels = c(0,0.05)  )



boxplot( PageRank ~ type, data = (log10(1 + get(paste0("hs.unweighted.df.",k))[,1:4]) %>% as_tibble() %>% 
                                    setNames(c("N.Com", "N.Cons", "N.Nur", "N.Ret")) %>%
                                    pivot_longer(N.Com:N.Ret, names_to = "type", values_to = "PageRank")),
         xlab = "", ylab = "Hub score", ylim = c(0,0.2),
         xaxt = "n", yaxt = "n", main = "Unweighted network")
axis(1, at = 1:4, labels = c(expression(N[Com]), expression(N[Cons]), expression(N[Nur]), expression(N[Ret])), cex.axis = 3 )
axis(2, at = c(0, 0.2), labels = c(0, 0.2)  )

dev.off()
}
