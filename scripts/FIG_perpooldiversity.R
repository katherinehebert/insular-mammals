# Script to make violin plot to describe the variation and mean of island-specific
# pool diversity per island system 
# Makes figure: perpooldiversity.png (Supplementary Material)
# Author: Katherine Hébert

# load libraries
library(ggplot2)
library(patchwork)

# load pool diversity output
perisland_pool <- readRDS("~/Documents/GitHub/IslandMammals/data/pool_perisland_diversity.RDS")

# function to make a violin plot to describe the variation and mean of island-specific
# pool diversity per island system
plot_div <- function(df, pooldiv){

  # order groups to match other plots
  df$group <- factor(df$group,
                     levels = c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", 
                                "Phl", "Snd", "Mlk", "Mln"))
  # boxplot
  p <- ggplot(df, aes(x = group, y = get(pooldiv))) + 
    geom_violin(aes(x = group, y = get(pooldiv)), 
                fill = "dodgerblue3",
                alpha = .4, lwd = 0) +
    stat_summary(fun.data = "mean_sdl", 
                 geom = "pointrange", 
                 fun.args = list(mult = 1),
                 shape = 16)  +
    labs(x = "") +
    ggpubr::theme_pubr() 
  return(p)
}


# make the plots
a <- plot_div(perisland_pool, "sp_rich") +  
  labs(y = "Species richness")
b <- plot_div(perisland_pool, "sesmpd") +  
  labs(y = "sesMPD") +
  geom_hline(yintercept = 0, lty = 2, col = "grey70")

# put them together using patchwork
a / b + plot_annotation(tag_levels = "a")

# save!
ggsave("figs/perpooldiversity.png")