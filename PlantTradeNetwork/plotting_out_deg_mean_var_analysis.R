# out degree mean and dispersal plotting



a <- getwd()

library(metR)
library(ggplot2)
library(viridis)


  
  #parse(text=sprintf('paste(d[mu],\' = %s\')',100)) 
  # data <- str.out.Ret.combinations
  #   title <-  parse(text=sprintf('N[Com] authority score'))
  #   legend_name <- paste("Retailer", " Out-Strength Score")
 heatmap.fun <- function(data, legend_name, title,x_label, y_label, bins, min.lim,  max.lim){

  #x_label <-"Out degree mean"
  #y_label <-"Out degree variance"
  
  # the_plot below is a contour plot with 20 bins
  binz <- bins
  
  
  pal <- viridis(binz, option = "A")
   
  # visualise pallete:
  # show_col(pal)
  
  # scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
  #   binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value
  #                #,n.breaks = 10
  #               # ,breaks = seq(min.lim, max.lim, length.out = 10)
  #               # ,limits = c(min.lim, max.lim)
  #                ,guide = guide, ...)
  # }
  
  
  the_plot <- ggplot(data, aes(data[,1],data[,2], z = data[,3]))  +
    # geom_contour_fill(bins = 20) +
    geom_contour_fill() +
     scale_fill_viridis(limits = c(min.lim, max.lim), option = "magma") +
    
   # scale_fill_fermenter_custom(pal) +
    
    labs(title = title
         ,subtitle = ""
         ,x = as.expression(bquote(.(as.name(x_label))))
         ,y = as.expression(bquote(.(as.name(y_label))))
         ,fill = legend_name
    ) +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    # scale_x_continuous(breaks = seq(1,10, by = 1))+
    # scale_y_continuous(breaks = formatC(seq(0,max(optimal_gamma$y_data), length.out = 6), format = "e", digits = 2))
    theme_bw() +
    theme(panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank()
          ,panel.background = element_rect(colour = "black", size=0.1)
          #,legend.position = ""
          ,axis.title.y = element_text(angle = 90, size = 20, margin = margin(t = 0, r = 10, b = 0, l = 0))
          ,axis.title.x = element_text(size = 20, margin = margin(t = 10, r = 10, b = 0, l = 0))
          , axis.text.x = element_text(size = 20, angle = 45, hjust = 1)
          , plot.title = element_text(hjust = 0.5, size = 20)
          , axis.text=element_text(size=20)
          ,legend.text = element_text(size = 20)
          ,legend.title = element_text(size = 20)
    ) +
    # edit the colourbar...
    guides(fill = guide_colorbar(
      ticks = TRUE, 
      even.steps = FALSE,
      frame.linewidth = 0.55, 
      frame.colour = "black", 
      ticks.colour = "black",
      barheight = 25,
      ticks.linewidth = 0.3)) 
  
  
  return(the_plot)
 }  

 
 
 load('combinations.Rdata')
 
 colnames(combinations) <- c("Out Degree Mean", "Out Degree Variance"
                             ,"N.Com.as", "N.Cons.as", "N.Nur.as", "N.Ret.as", "Ret.as"
                             ,"N.Com.hs", "N.Cons.hs", "N.Nur.hs", "N.Ret.hs", "Ret.hs"
                             ,"N.Com.betwns", "N.Cons.betwns", "N.Nur.betwns", "N.Ret.betwns", "Ret.betwns"
                             ,"N.Com.as.uw", "N.Cons.as.uw", "N.Nur.as.uw", "N.Ret.as.uw", "Ret.as.uw"
                             ,"N.Com.hs.uw", "N.Cons.hs.uw", "N.Nur.hs.uw", "N.Ret.hs.uw", "Ret.hs.uw"
                             ,"N.Com.betwns.uw", "N.Cons.betwns.uw", "N.Nur.betwns.uw", "N.Ret.betwns.uw", "Ret.betwns.uw"                            
                             ,"N.Com.str.in", "N.Cons.str.in", "N.Nur.str.in", "N.Ret.str.in", "Ret.str.in"
                             ,"N.Com.str.out", "N.Cons.str.out", "N.Nur.str.out", "N.Ret.str.out", "Ret.str.out")
 
 
 
 #authority scores
 data <- combinations[,c(1,2,3:7)]
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 x_label <- "Out-degree mean" 
 y_label <- "Out-degree variance"
 legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 0
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 0.1
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout)  +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 40,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "AuthorityScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 #log10 scale
 data <- combinations[,c(1,2,3:7)]
 data[,3:7] = log10(data[,3:7])
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 
 # legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
 legend_name <- expression(log[10]("Authority score"))
 
 min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
 min.lim <- -3
 max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
 max.lim <- -1
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) 
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 50,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "AuthorityScoreHeatMaps_Out_Deg_Mean_sd_log10.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 

 
  #Hub scores
 data <- combinations[,c(1,2, 8:11)]
 
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6]))
 min.lim <- 0
 max(c(data[,3], data[,4], data[,5], data[,6]))
 max.lim <- 0.3
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) 
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 # title <-  parse(text=sprintf('paste(Retailer,  " hub score")'))
 # p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 
 library(patchwork)
 
 layout <- '
ABC
D##

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                    #  E = p.5,
                      design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) + 
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "HubScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
 #log10 scale
 data <- combinations[,c(1,2, 8:11)]
 data[,3:6] = log10(data[,3:6])
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 
 # legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
 legend_name <- expression(log[10]("Hub score"))
 
 min((c(data[,3], data[,4], data[,5], data[,6])))
 min.lim <- -4
 max((c(data[,3], data[,4], data[,5], data[,6])))
 max.lim <- 0
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) 
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 #title <-  parse(text=sprintf('paste(Retailer,  " hub score")'))
 #p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 
 library(patchwork)
 
 layout <- '
ABC
D##

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      #E = p.5,
                      design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +  
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "HubScoreHeatMaps_Out_Deg_Mean_sd_log10.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
 
 #betweenness
 
 data <- combinations[,c(1,2, 13:17)]
 
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 legend_name <- parse(text=sprintf('paste("Betweenness",  "")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 10000
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 6500000
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )  + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +   
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "BetweennessScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
 #betweenness log scale
 
 data <- combinations[,c(1,2, 13:17)]
 data[, 3:7] = log10(data[, 3:7])
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 legend_name <- expression(log[10]("Betweenness"))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 4
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 7
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +  
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "BetweennessScoreHeatMaps_Out_Deg_Mean_sd_log10.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 ) 
 
 

 #str.in
 
 data <- combinations[,c(1,2, 18:22)]
 
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 0
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 45000
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout)   +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "In_strength_ScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
 #log10 scale
 data <- combinations[,c(1,2, 18:22)]
 data[,3:7] = log10(data[,3:7])
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 # legend_name <- parse(text=sprintf('paste("In-strength",  " score")'))
 legend_name <- expression(log[10]("In-strength"))
 
 min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
 min.lim <- 3
 max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
 max.lim <- 5
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "In_strengthScoreHeatMaps_Out_Deg_Mean_sd_log10.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 #str.out
 
 data <- combinations[,c(1,2, 23:27)]
 
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 5000
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 700000
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "Out_strengthScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
 #log10 scale
 data <- combinations[,c(1,2, 23:27)]
 data[,3:7] = log10(data[,3:7])
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 
 # legend_name <- parse(text=sprintf('paste("Out-strength",  " score")'))
 legend_name <- expression(log[10]("Out-strength"))
 
 min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
 min.lim <- 3
 max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
 max.lim <- 6
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "Out_strengthScoreHeatMaps_Out_Deg_Mean_sd_log10.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
  # unweighted plots  --------------------------------------------------------------------------------------
 
 #authority scores
 data <- combinations[,c(1,2,18:22)]
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 x_label <- "Out-degree mean" 
 y_label <- "Out-degree variance"
 legend_name <- parse(text=sprintf('paste("Unweighted authority",  " score")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 0
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 1
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "Unweighted_AuthorityScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
 
 #Hub scores
 data <- combinations[,c(1,2, 23:27)]
 
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
# data[, 3:6] = log10(data[, 3:6])
 #legend_name <- expression(log[10]("Unweighted hub score"))
 
 legend_name <- parse(text=sprintf('paste("Unweighted hub",  " score")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6]))
 min.lim <- 0
 max(c(data[,3], data[,4], data[,5], data[,6]))
 max.lim <- 1
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
#  title <-  parse(text=sprintf('paste(Retailer,  " hub score")'))
#  p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 
 library(patchwork)
 
 layout <- '
ABC
D##

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      #  E = p.5,
                      design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "Unweighted_HubScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 

 
 
 #betweenness
 
 data <- combinations[,c(1,2, 28:32)]
 
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 legend_name <- parse(text=sprintf('paste("Unweighted betweenness",  "")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 10000
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 4500000
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "Unweighted_BetweennessScoreHeatMaps_Out_Deg_Mean_sd.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 )
 
 
 
 #betweenness log scale
 {
 data <- combinations[,c(1,2, 28:32)]
 data[, 3:7] = log10(data[, 3:7])
 title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
 legend_name <- #expression(log[10]("Unweighted betweenness"))
 
 parse(text=sprintf('paste(log[10],  "(Unweighted betweenness)")'))
 
 min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 min.lim <- 4
 max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
 max.lim <- 7
 p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Cons],  "")'))
 p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(N[Nur],  "")'))
 p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) 
 title <-  parse(text=sprintf('paste(N[Ret],  "")'))
 p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 title <-  parse(text=sprintf('paste(Retailer,  "")'))
 p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 
 library(patchwork)
 
 layout <- '
ABC
DE#

'
 
 library(gridExtra)
 library(grid)
 
 p.10 <-   wrap_plots(A = p.1, B = p.2, C= p.3, D= p.4,
                      E = p.5, design = layout) +   guides(fill = guide_colorbar(
                        ticks = TRUE,
                        even.steps = FALSE,
                        frame.linewidth = 0.55,
                        frame.colour = "black",
                        ticks.colour = "black",
                        barheight = 30,
                        ticks.linewidth = 0.3)) +
   plot_annotation(title = expression("" ), theme = theme(plot.title = element_text(hjust = 0.5)))
 p.10
 ggsave(
   filename = "Unweighted_BetweennessScoreHeatMaps_Out_Deg_Mean_sd_log10.png",
   device="png",
   width=2*3600,
   height=3600,
   units="px",
   plot=p.10, 
   limitsize = TRUE
 ) 
 
 }
 