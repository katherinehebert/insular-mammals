# Script to combine the lmm coefficient plots for Fig. 4 of the manuscript

library(patchwork)
library(ggplot2)
theme_set(ggpubr::theme_pubr())

tax <- readRDS("output/lmm_taxonomic_pca_coefplot.rds") + coord_flip(ylim = c(-0.5, 0.5))
phy <- readRDS("output/lmm_phylogenetic_pca_coefplot.rds") + coord_flip(ylim = c(-0.5, 0.5))
(tax | phy) & 
  theme(plot.tag = element_text(face = "bold"),
        plot.title = element_text(face = "bold")) 
ggsave("figs/fig4_lmm.png", width = 8.85, height = 3.94)
