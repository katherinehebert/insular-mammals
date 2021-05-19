## Scripts to make descriptive plots of the regional pools

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

# fig 3 ----

# import vp results
vp <- read.csv("data/VP_bsim.csv")
df <- left_join(vp, pooldiversity)

cor_plot <- function(df){
  
  # order island systems
  df$group <- factor(df$group, levels = c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", 
                                          "Phl", "Snd", "Mlk", "Mln"))
  
  # calculate correlation
  res = list()
  res$TD <- cor.test(df$Radj[df$diversity == "T"], df$tr_25p[df$diversity == "T"], 
                     method = "pearson", alternative = "greater") %>%
    broom::tidy()
  res$PD <- cor.test(df$Radj[df$diversity == "P"], df$sesmpd_25p[df$diversity == "P"], 
                     method = "pearson", alternative = "greater") %>%
    broom::tidy()
  res <- bind_rows(res, .id = "diversity")
  # taxonomic
  pt <- filter(df, diversity == "T") %>%
    ggplot() +
    geom_point(aes(x = tr_25p, y = Radj, col = group, shape = type),
               size = 3.5) +
    labs(x = "Pool TR") + #, 
         #title = "a) Taxonomic")  + 
    # annotate with correlation results
    annotate(geom = "text", x = 400, y = .47, size = 5,
             label = paste0("R = ", round(res$estimate[1], 3),
                            "\n p = ", round(res$p.value[1], 2))) +
    format +
    theme(legend.position = "none") 
  # phylogenetic
  pp <- filter(df, diversity == "P") %>%
    ggplot() +
    geom_point(aes(x = sesmpd_25p, y = Radj, col = group, shape = type),
               size = 3.5) +
    # annotate with correlation results
    annotate(geom = "text", x = 0, y = .47, size = 5,
             label = paste0("R = ", round(res$estimate[2], 2),
                            "\n p = ", round(res$p.value[2], 3))) +
    format +
    theme(legend.position = "right") +
    labs(x = "Pool sesMPD", 
         #title = "b) Phylogenetic",
         color = "Archipelagos")  
  # patchwork together
  return(pt + pp)
}

# make plot
s4A <- cor_plot(df)
# save
#ggsave("figs/fig3_pool_selection_corr.png", width = 8.64, height = 4)


# do the same with the ses results for fig. s4----------------------------------

# import vp results
vp <- read.csv("data/VP_sesbsim.csv")
df2 <- left_join(vp, pooldiversity)
# make plot
s4B <- cor_plot(df2) + theme(legend.position = "none")
# save
#ggsave("figs/figs4_pool_selection_corr.png", width = 8.64, height = 4)

s4A / s4B + plot_annotation(tag_levels = "a")
ggsave("figs/figs4_pool_selection_corr.png", width = 9.3, height = 7)

# write pools to shapefiles
write_shp <- function(filename){
  pool <- readRDS(paste0("output/regionalpool", filename, ".rds"))
  pool <- pool$grid
  filename <- gsub(" ", "", filename)
  maptools::writeSpatialShape(pool, paste0("output/", filename))
}

names_list <- c(" Adr _50p", " Alx _50p", " Clf _50p", " Snd _50p", " MdtLB _50p", 
                " MdtOC _50p", " Mlk _50p", " Mln _50p", " Phl _50p")
lapply(names_list, write_shp)

names_list <- c("Adr_25p", "Alx_25p", "Clf_25p", "Snd_25p", "MdtLB_25p", "MdtOC_25p", 
                "Mlk_25p", "Mln_25p", "Phl_25p")
lapply(names_list, write_shp)
