# Script to build and validate linear mixed model
# for phylogenetic compositional uniqueness as a response variable

# load libraries
library(nlme)
library(MuMIn)
library(ggplot2)
library(patchwork)

# load beta-diversity result
beta <- readRDS("~/Documents/GitHub/IslandMammals/data/beta_phylo.RDS")
# prep data
source('~/Documents/GitHub/IslandMammals/scripts/lmm_fullworkflow_prepdata.R')
# load helper functions
source('~/Documents/GitHub/IslandMammals/scripts/lmm_fullworkflow_plotfunctions.R')
# set ggplot theme
theme_set(ggpubr::theme_pubr())

# PCA axes ---------------------------------------------------------------------

# build model 
m1 <- lme(mean_beta ~ 
            PC1 + PC2 + PC3 + GMMC + sesmpd, 
          data = perisland, 
          random =  ~ 1 | group.x
)
broom.mixed::tidy(m1)

# residuals vs. fitted? check
resid_fit_m1 <- resid_fit(m1)
# normality? check
resid_normal_m1 <- resid_normal(m1)
# spatial autocorrelation?
morans_i(perisland, m1) # low, but sig: 0.04, p = 0.04
# plot residuals in space
resid_map_m1 <- resid_map(m1, perisland)

# look at output
lmm_out = broom.mixed::tidy(m1)
coefs <- lmm_coeff_plot(lmm_out, 
                        pooldiv_label = "sesmpd", 
                        plot_title = "b) Phylogenetic",
                        ylimits = c(-1, 1))
write.csv(lmm_out, "output/lmm_phylogenetic_pca.csv")

# plot 2: model residuals
((resid_normal_m1/resid_fit_m1) | resid_map_m1) & 
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))
ggsave("figs/lmm_phylogenetic_pca_residualplots.png", width = 12.4, height = 7)

# plot 3: model results
saveRDS(coefs, "output/lmm_phylogenetic_pca_coefplot.rds")