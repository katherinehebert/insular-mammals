# Script to prepare data for lmm workflow
# run this before the lmm workflow scripts!


# load libraries
require(tidyverse)
require(otuSummary)
require(vegan)
require(patchwork)
require(broom)

# load environmental data 
env_full <- readRDS("~/Documents/GitHub/IslandMammals/data/env_metacomm.RDS")
names(env_full)[c(5,6)] = c("MdtLB", "MdtOC")

# load pool diversity
pooldiversity <- readRDS("~/Documents/GitHub/IslandMammals/output/pooldiversity.RDS")[[3]]

# set ggplot theme
theme_set(theme_linedraw() + 
            theme(panel.grid = element_blank(),
                  legend.title = element_text(colour = "white")))


#### long_list function ####
# to convert list to long format data frame with identifier column
long_list <- function(x, valuename){
  lapply(x, as.dist) %>%
    lapply(., matrixConvert, colname = c("island_a", "island_b", valuename)) %>%
    bind_rows(.id = "group") 
  # ^ this binds rows to convert list into data frame with a column of list element names
}

# convert each list to long format df
beta = long_list(beta, "beta")

# take mean of beta-diversity values per island
mean_beta = beta %>% group_by(group, island_a) %>% 
  summarize(mean_beta = mean(beta, na.rm = TRUE),
            sd_beta = sd(beta, na.rm = TRUE))
colnames(mean_beta)[2] = "island"

# collapse list of environment dataframes
for(i in 1:length(env_full)){
  env_full[[i]]$island = rownames(env_full[[i]])
}
env_full =  bind_rows(env_full, .id = "group") 

# keep islands which are in both
env_sub = env_full[which(env_full$island %in% mean_beta$island),]

# join the df together
perisland = merge(mean_beta, env_sub, by = "island")
perisland$GMMC = as.factor(perisland$GMMC)
perisland$group.x = gsub("Ind", "Snd", perisland$group.x)
perisland$group.x = as.factor(perisland$group.x)

# transform and scale variables for modelling
perisland$Area = log10(perisland$Area+1)
perisland$Elev = log10(perisland$Elev+1)
perisland[,colnames(perisland) %in% c("Lat", "Long", "Dist", "SLMP","Area","Elev","Temp","varT","Prec","varP")] <-
  apply(perisland[,colnames(perisland) %in% c("Lat", "Long", "Dist", "SLMP","Area","Elev","Temp","varT","Prec","varP")], 2, scale)
perisland$GMMC = as.factor(perisland$GMMC)

# add pool diversity column
perisland$pool <- NA
rownames(pooldiversity) = gsub("Snd", "Ind", rownames(pooldiversity))
for(i in 1:nrow(pooldiversity)){
  perisland[which(perisland$group.y == rownames(pooldiversity)[i]),"pool"] <- pooldiversity$SESMPD_Env25p[i]
}

# add per island species pool diversity
perisland$island <- as.character(perisland$island)
perisland_pool <- readRDS("output/pool_perisland_diversity.RDS")
# join
perisland <- left_join(perisland, perisland_pool, by = "island")
perisland$sp_rich <- scale(perisland$sp_rich)
perisland$sesmpd <- scale(perisland$sesmpd)


# full PCA ----
# if (!requireNamespace('BiocManager', quietly = TRUE))
#   install.packages('BiocManager')
# BiocManager::install('PCAtools')
library(PCAtools)

# make matrix of island characteristic variables 
mat <- subset(perisland, 
              select = c(Area, Elev, Temp, varT, Prec, varP, SLMP, Dist)) %>%
  as.matrix() %>% 
  t()
# do principal coordinates analysis
all <- pca(mat)

# select axes to keep
elbow <- findElbowPoint(all$variance)
elbow
horn <- parallelPCA(mat)
horn$n
which(cumsum(all$variance) > 80)[1]
screeplot(all, axisLabSize = 18, titleLabSize = 22, vline = c(elbow), hline = 80, title = "")
ggsave("figs/pca_screeplot.png", width = 6.03, height = 5.74)

# check biplot
biplot(all, showLoadings = TRUE, lab = NULL)

# look at axis loadings to interpret them
plotloadings(all, 
             components = getComponents(all, c(1:3))) + 
  coord_cartesian(ylim = c(-0.6, 0.6))
ggsave("figs/pca_loadings.png", width = 6.03, height = 5.74)

# look at loadings as heatmap
heatmap(as.matrix(all$loadings[,1:3]))

# add to islands dataset
perisland <- cbind(perisland, all$rotated[,1:3])