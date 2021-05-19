# clear workspace
rm(list=ls())

# load packages
require(tidyverse)
require(vegan)
require(ggbiplot)

# import environmental datasets
env = readRDS("data/env_metacomm.RDS")
names(env)[c(5,6)] = c("MdtLB", "MdtOC")

# combine into one dataset
for(i in 1:length(env)){
  env[[i]]$group = names(env)[i]
  env[[i]]$island = rownames(env[[i]])}
env = do.call(rbind, env)

# log transform area
env$Area = log10(env$Area)

# subset to variables used in study
ids = subset(env, select = c(group, island))
env = subset(env, select = -c(X, ID, group, island, Lat, Long, Dist, SLMP, GMMC, CCVT))

# center and standardize variables
envstd = decostand(apply(env, 2, as.numeric), "standardize")
rownames(envstd) = ids$island

# compute PCA
pca = prcomp(envstd)

# palette from metacommunity/sp pool map
palette <- c("skyblue1", "#FD8E38", "#E942A6", "#FEDA28", "#21A723",
             "#BC43F1", "#28BD92", "#1C38F7", "#D73535")
names(palette) <-  c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", "Phl", "Snd", "Mlk", "Mln")

# plot
source('scripts/FUN_ggbiplot_custom.R') 
ggbiplot_custom(pca, groups = gsub("Ind", "Snd", ids$group)) + 
  scale_color_manual(values = palette) + labs(col = NULL) +
  theme_classic() +
  theme(legend.text = element_text(size = 10),
        axis.title = element_text(size = 11, face = "bold"))
ggsave("figs/env_PCA.png", width = 6, height = 6)