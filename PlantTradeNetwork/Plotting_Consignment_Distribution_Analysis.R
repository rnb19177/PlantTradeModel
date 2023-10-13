# plotting for Consignment distribution analysis
library(ggplot2)
library(metR)
library(viridis)
a <- getwd()

setwd(paste(a, "/Commercial_Parameters", sep = ""))
load("combinations.RData")
commercial.combinations <- combinations


setwd(paste(a, "/Consumer_Parameters", sep = ""))
load("combinations.RData")
consumer.combinations <- combinations


setwd(paste(a, "/Nursery_Parameters", sep = ""))
load("combinations.RData")
nursery.combinations <- combinations


setwd(paste(a, "/Retailer_Parameters", sep = ""))
load("combinations.RData")
retailer.combinations <- combinations



# combinations matrix: rows are for each combination of parameters in 1st 2 columns

# next columns are centrality measures per nursery & retailer group:
# cols 3:7   authority scores
# cols 8:12  hub scores
# cols 13:17 betweenness
# cols 18:22 in. strength
# cols 23:27 out.strength
#names(commercial.combinations)



# Function to plot heatmap data -----------------

#make_a_plot_from_data <- function(data, legend_name){


 # data <- as.N.com.combinations
#  legend_name <- expression(paste(N[Com], " Authority Score"))

#  data <- as.N.cons.combinations
#  legend_name <-expression(paste(N[Cons], " Authority Score"))

 # data <- as.N.nur.combinations
 # legend_name <- expression(paste(N[Nur], " Authority Score"))

# data <- as.N.ret.combinations
#  legend_name <- expression(paste(N[Ret], " Authority Score"))
  
  data <- commercial.combinations
  legend_name <- paste("Retailer", "Authority Score")


  heatmap.fun <- function(data, legend_name, title, x_label, y_label, bins, min.lim,  max.lim){
    
    
    x_label <-x_label
    y_label <-y_label
    
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
            , axis.text.x = element_text(angle = 45, hjust = 1)
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

  
  
  # commercial parameters ---------------------------------------------------------------------------------------------
  
 
  #authority scores
  data <- commercial.combinations[,c(1,2,3:7)]
  title <-  parse(text=sprintf('paste(N[Com],  " authority score")'))
  x_label <- "Commercial mean" 
  y_label <- "Commercial s.d"
  legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 0.005
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
    filename = "AuthorityScoreHeatMaps_Commercial_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- commercial.combinations[,c(1,2,3:7)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
 
 # legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  legend_name <- expression(log[10]("Authority score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- -3
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- -2
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
    filename = "AS_HeatMaps_Commercial_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
    #Hub scores
  data <- commercial.combinations[,c(1,2, 8:12)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
  legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 0.03
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
    filename = "HubScoreHeatMaps_Commercial_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- commercial.combinations[,c(1,2, 8:12)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
 
  # legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  legend_name <- expression(log[10]("Hub score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- -18
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- -1
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  #title <-  parse(text=sprintf('paste(Retailer,  "")'))
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
    filename = "HubScoreHeatMaps_Commercial_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  #betweenness
  
  data <- commercial.combinations[,c(1,2, 18:22)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  legend_name <- parse(text=sprintf('paste("Betweenness",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 10000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 800000
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
    filename = "BetwnsHeatMaps_Commercial_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  #str.in
  
  data <- commercial.combinations[,c(1,2, 23:27)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 2000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 10000
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
    filename = "IS_HeatMaps_Commercial_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- commercial.combinations[,c(1,2, 23:27)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))

  # legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  legend_name <- expression(log[10]("In-strength"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- 3
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- 4
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
    filename = "IS_HeatMaps_Commercial_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  #str.out
  
  data <- commercial.combinations[,c(1,2, 28:32)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 5000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 90000
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
    filename = "OS_HeatMaps_Commercial_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- commercial.combinations[,c(1,2, 28:32)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  
  # legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  legend_name <- expression(log[10]("Out-strength"))
  
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
    filename = "OS_HeatMaps_Commercial_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  


#Consumer parameters ---------------------------------------------------------------------------------------------
  
  #authority scores
  data <- consumer.combinations[,c(1,2,3:7)]
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  x_label <- "Consumer mean" 
  y_label <- "Consumer s.d"
  legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 0.005
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
    filename = "AuthorityScoreHeatMaps_Consumer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- consumer.combinations[,c(1,2,3:7)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  legend_name <- expression(log[10]("Authority score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- -3
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- -2
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
    filename = "AS_HeatMaps_Consumer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  #Hub scores
  data <- consumer.combinations[,c(1,2, 8:12)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
  legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 0.03
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
    filename = "HS_HeatMaps_Consumer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- consumer.combinations[,c(1,2, 8:12)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  legend_name <- expression(log[10]("Hub score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- -18
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- -1
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
    filename = "HS_HeatMaps_Consumer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  #betweenness
  
  data <- consumer.combinations[,c(1,2, 18:22)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))

  legend_name <- parse(text=sprintf('paste("Betweenness",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 10000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 800000
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
    filename = "BetwnsScoreHeatMaps_Consumer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #str.in
  
  data <- consumer.combinations[,c(1,2, 23:27)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
  legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 2000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 10000
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
    filename = "IS_HeatMaps_Consumer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- consumer.combinations[,c(1,2, 23:27)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
 
  # legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  legend_name <- expression(log[10]("In-strength"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- 3
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- 4
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
    filename = "In_strengthScoreHeatMaps_Commercial_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  #str.out
  
  data <- consumer.combinations[,c(1,2, 28:32)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 2000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 100000
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
    filename = "OS_HeatMaps_Consumer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- consumer.combinations[,c(1,2, 28:32)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  legend_name <- expression(log[10]("Out-strength"))
  
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
    filename = "OS_HeatMaps_Consumer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  
  #Nursery parameters ---------------------------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------------
  
  
  #authority scores
  data <- nursery.combinations[,c(1,2,3:7)]
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  x_label <- "Nursery mean" 
  y_label <- "Nursery s.d"
  legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 0.05
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
    filename = "AuthorityScoreHeatMaps_Nursery_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- nursery.combinations[,c(1,2,3:7)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  legend_name <- expression(log[10]("Authority score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- -5
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- -1
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
    filename = "AuthorityScoreHeatMaps_Nursery_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  #Hub scores
  data <- nursery.combinations[,c(1,2, 8:11)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
  legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6]))
  max.lim <- 0.05
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  #title <-  parse(text=sprintf('paste(Retailer,  "")'))
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
    filename = "HubScoreHeatMaps_Nursery_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- nursery.combinations[,c(1,2, 8:11)]
  data[,3:6] = log10(data[,3:6])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  legend_name <- expression(log[10]("Hub score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6])))
  min.lim <- -4
  max((c(data[,3], data[,4], data[,5], data[,6])))
  max.lim <- -1
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  #title <-  parse(text=sprintf('paste(Retailer,  "")'))
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
    filename = "HubScoreHeatMaps_Nursery_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
 
  
  #betweenness
  
  data <- nursery.combinations[,c(1,2, 18:22)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
  legend_name <- parse(text=sprintf('paste("Betweenness",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 10000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 1.1*10^6
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
    filename = "BetweennessScoreHeatMaps_Nursery_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  #betweenness log10 scale
  
  data <- nursery.combinations[,c(1,2, 18:21)]
  data[, 3:7] = log10(data[, 3:6])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  #legend_name <- parse(text=sprintf('paste("Betweenness",  "")'))
  legend_name <- expression(log[10]("Betweenness"))
  min(c(data[,3], data[,4], data[,5], data[,6]))
  min.lim <- 5
  max(c(data[,3], data[,4], data[,5], data[,6]))
  max.lim <- 6.1
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  #title <-  parse(text=sprintf('paste(Retailer,  "")'))
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
    filename = "BetweennessHeatMaps_Nursery_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  
  #str.in
  
  data <- nursery.combinations[,c(1,2, 23:27)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))

  legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 500
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 20000
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
    filename = "In_strength_ScoreHeatMaps_Nursery_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- nursery.combinations[,c(1,2, 23:27)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  
  # legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  legend_name <- expression(log[10]("In-strength"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- 2
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
    filename = "In_strengthHeatMaps_Nursery_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  
  
  #str.out
  
  data <- nursery.combinations[,c(1,2, 28:32)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))

  legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 80000
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
    filename = "Out_strengthScoreHeatMaps_Nursery_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- nursery.combinations[,c(1,2, 28:32)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  legend_name <- expression(log[10]("Out-strength"))
  
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
    filename = "Out_strengthScoreHeatMaps_Nursery_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  #---------------------------------------------------------------------------------
  
  
  
  
  #Retailer parameters ---------------------------------------------------------------------------------------------
  
  
  
  
  #authority scores
  data <- retailer.combinations[,c(1,2,3:7)]
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  x_label <- "Retailer mean" 
  y_label <- "Retailer s.d"
  legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 0.2
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
    filename = "AuthorityScoreHeatMaps_Retailer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- retailer.combinations[,c(1,2,3:7)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Authority",  " score")'))
  legend_name <- expression(log[10]("Authority score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- -4
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- 0
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
    filename = "AuthorityHeatMaps_Retailer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #Hub scores
  data <- retailer.combinations[,c(1,2, 8:12)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))

  legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 0.2
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
    filename = "HubScoreHeatMaps_Retailer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- retailer.combinations[,c(1,2, 8:11)]
  data[,3:7] = log10(data[,3:6])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("Hub",  " score")'))
  legend_name <- expression(log[10]("Hub score"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- -4
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- 0
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
 # title <-  parse(text=sprintf('paste(Retailer,  "")'))
#  p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
  
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
    filename = "HubScoreHeatMaps_Retailer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  
  
  #betweenness
  
  data <- retailer.combinations[,c(1,2, 18:22)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
 
  legend_name <- parse(text=sprintf('paste("Betweenness",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 10000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 850000
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
                       E = p.5,
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
    filename = "BetweennessScoreHeatMaps_Retailer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  #log10 scale 
  str(data)
  
  data <- retailer.combinations[,c(1,2, 18:22)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  #legend_name <- parse(text=sprintf('paste("Betweenness",  "")'))
  legend_name <- expression(log[10]("Betweenness"))
  
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7] ))
  min.lim <- 4
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 6
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title, x_label, y_label, bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
#  title <-  parse(text=sprintf('paste(Retailer,  "")'))
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
                       design = layout) + guides(fill = guide_colorbar(
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
    filename = "BetweennessHeatMaps_Retailer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  
  
  
  
  #str.in
  
  data <- retailer.combinations[,c(1,2, 23:27)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))

  legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 10000
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
    filename = "In_strength_ScoreHeatMaps_Retailer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- retailer.combinations[,c(1,2, 23:27)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  

  # legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  legend_name <- expression(log[10]("In-strength"))
  
  min((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  min.lim <- 1
  max((c(data[,3], data[,4], data[,5], data[,6], data[,7])))
  max.lim <- 4
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
    filename = "In_strengthScoreHeatMaps_Retailer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  #str.out
  
  data <- retailer.combinations[,c(1,2, 28:32)]
  
  title <-  parse(text=sprintf('paste(N[Com],  "")'))

  legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 5000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 80000
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
    filename = "Out_strengthScoreHeatMaps_Retailer_Mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  #log10 scale
  data <- retailer.combinations[,c(1,2, 28:32)]
  data[,3:7] = log10(data[,3:7])
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  
  # legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  legend_name <- expression(log[10]("Out-strength"))
  
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
    filename = "Out_strengthScoreHeatMaps_Retailer_Mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
#---------------------------------------------------------------------------------  
  
  
  # out-strength scores for commercial, consumer, nurseries, retailers ------------------------------------------
  
  data <- combinations[,1:2]
  
  data[,3] <- commercial.combinations$N.Com.str.out
  data[,4] <-consumer.combinations$N.Cons.str.out
  data[,5] <-consumer.combinations$Ret.str.out
  data[,6] <-nursery.combinations$N.Nur.str.out
  data[,7] <-retailer.combinations$N.Ret.str.out
  
  
  legend_name <- parse(text=sprintf('paste("Out-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 2000
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 100000
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label = "Commercial mean", y_label = "Commercial s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title,x_label = "Consumer mean", y_label = "Consumer s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(Retailer,  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title,x_label = "Consumer mean", y_label = "Consumer s.d" ,
                     bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title,x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title,x_label = "Retailer mean", y_label = "Retailer s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  
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
    filename = "Out_strengthScoreHeatMaps_mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  # in-strength scores for commercial, consumer, nurseries, retailers ------------------------------------------
  
  data <- combinations[,1:2]
  
  data[,3] <- nursery.combinations$N.Com.str.in
  data[,4] <-nursery.combinations$N.Cons.str.in
  data[,5] <-nursery.combinations$N.Nur.str.in
  data[,6] <-nursery.combinations$N.Nur.str.in
  data[,7] <-retailer.combinations$Ret.str.in
  
  
  legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 20000
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title,x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title,x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title,x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(Retailer,  "")'))
  p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title,x_label = "Retailer mean", y_label = "Retailer s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  
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
    filename = "In_strengthScoreHeatMaps_mean_sd.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )
  
  
  
  # in-strength scores for commercial, consumer, nurseries, retailers: log10 scale ------------------------------------------
  
  data <- combinations[,1:2]
  
  data[,3] <- log10(nursery.combinations$N.Com.str.in)
  data[,4] <-log10(nursery.combinations$N.Cons.str.in)
  data[,5] <-log10(nursery.combinations$N.Nur.str.in)
  data[,6] <-log10(nursery.combinations$N.Nur.str.in)
  data[,7] <- log10(retailer.combinations$Ret.str.in)
  
  
  #legend_name <- parse(text=sprintf('paste("In-strength",  "")'))
  legend_name <- expression(log[10]("In-strength"))
  min(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  min.lim <- 0
  max(c(data[,3], data[,4], data[,5], data[,6], data[,7]))
  max.lim <- 5
  title <-  parse(text=sprintf('paste(N[Com],  "")'))
  p.1 <- heatmap.fun(data[,c(1,2,3)], legend_name, title, x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Cons],  "")'))
  p.2 <- heatmap.fun(data[,c(1,2,4)], legend_name, title,x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(N[Nur],  "")'))
  p.3 <- heatmap.fun(data[,c(1,2,5)], legend_name, title,x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim )
  title <-  parse(text=sprintf('paste(N[Ret],  "")'))
  p.4 <- heatmap.fun(data[,c(1,2,6)], legend_name, title,x_label = "Nursery mean", y_label = "Nursery s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  title <-  parse(text=sprintf('paste(Retailer,  "")'))
  p.5 <- heatmap.fun(data[,c(1,2,7)], legend_name, title,x_label = "Retailer mean", y_label = "Retailer s.d" ,
                     bins = 10, min.lim, max.lim ) + theme(legend.position = "none")
  
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
    filename = "In_strengthScoreHeatMaps_mean_sd_log10.png",
    device="png",
    width=2*3600,
    height=3600,
    units="px",
    plot=p.10, 
    limitsize = TRUE
  )