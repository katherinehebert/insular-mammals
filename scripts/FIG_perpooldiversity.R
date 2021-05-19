# Script to plot pool diversity

# load libraries
library(ggplot2)
library(patchwork)

# load beta-diversity result
beta <- readRDS("~/Documents/GitHub/IslandMammals/data/beta_phylo.RDS")
# prep data
source('~/Documents/GitHub/IslandMammals/scripts/lmm_fullworkflow_prepdata.R')


# update (re-do!) SES plot
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

a<-plot_div(perisland_pool, "sp_rich") + 
  labs(y = "Species richness")
b<-plot_div(perisland_pool, "sesmpd") + 
  labs(y = "sesMPD") +
  geom_hline(yintercept = 0, lty = 2, col = "grey70")

a / b + plot_annotation(tag_levels = "a")
ggsave("figs/perpooldiversity.png")
