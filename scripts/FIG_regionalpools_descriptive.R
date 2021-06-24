## Scripts to make descriptive plots of the regional source pools
## Author: Katherine Hébert

# load packages
library(ggplot2)
library(dplyr)
library(patchwork)

# load a ggplot format object
format <- readRDS("output/format_figures.RDS")

# load results of pool diversity
pooldiversity <- readRDS("output/regionalpool_diversity.RDS")

pooldiv_plot <- function(df, diversity, ylabel) {
  # order groups to match other plots
  df$group <- factor(df$group, 
                     levels = c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", 
                                "Phl", "Snd", "Mlk", "Mln"))
  # plot
  p <- ggplot(df) +
    geom_bar(aes(y = get(diversity), x = group), stat = "identity") +
    labs(x = "",
         y = ylabel) +
    ggpubr::theme_pubr()
  return(p)
}

# make plots
p1 <- pooldiv_plot(pooldiversity, 
                   "tr_25p", 
                   "Taxonomic richness (TR)")
p2 <- pooldiv_plot(pooldiversity, 
                   "sesmpd_25p", 
                   "sesMPD") + geom_hline(yintercept = 0)
# patchwork together
p1 / p2 + plot_annotation(tag_levels = "a")
ggsave("figs/figs6_pooldiversity.png", width = 7.73, height = 6.63)


# write pools to shapefiles
write_shp <- function(filename){
  pool <- readRDS(paste0("output/regionalpool", filename, ".rds"))
  pool <- pool$grid
  filename <- gsub(" ", "", filename)
  maptools::writeSpatialShape(pool, paste0("output/", filename))
}

names_list <- c("Adr_25p", "Alx_25p", "Clf_25p", "Snd_25p", "MdtLB_25p", "MdtOC_25p", 
                "Mlk_25p", "Mln_25p", "Phl_25p")
lapply(names_list, write_shp)
