# Script to build and validate linear mixed model 
# for taxonomic compositional uniqueness as a response variable

# load libraries
library(nlme)
library(ggplot2)
library(patchwork)
library(MuMIn)

# load beta-diversity result
beta <- readRDS("~/Documents/GitHub/IslandMammals/data/beta_t.RDS")

# prep data
source('~/Documents/GitHub/IslandMammals/scripts/lmm_fullworkflow_prepdata.R')
# load helper functions
source('~/Documents/GitHub/IslandMammals/scripts/lmm_fullworkflow_plotfunctions.R')
# set ggplot theme
theme_set(ggpubr::theme_pubr())

# correlation between variables
var_corplot <- subset(perisland, 
       select = c(Dist, SLMP, Area, Elev, Temp, varT, Prec, varP)) %>% 
  as.matrix() %>% cor() %>% corrplot::corrplot(sig.level = 0.05, addCoef.col = "white")
var_corplot

# build model 
m1 <- lme(mean_beta ~ 
            PC1 + PC2 + PC3 + GMMC + sp_rich, 
          data = perisland, 
          random =  ~ 1 | group.x
)

# residuals vs. fitted? check
resid_fit_m1 <- resid_fit(m1)
# normality? check
resid_normal_m1 <- resid_normal(m1)
# spatial autocorrelation?
morans_i(perisland, m1)
# plot residuals in space
resid_map_m1 <- resid_map(m1, perisland)

# look at output
lmm_out = broom.mixed::tidy(m1)
coefs <- lmm_coeff_plot(lmm_out, 
                        pooldiv_label = "sp_rich", 
                        plot_title = "a) Taxonomic",
                        ylimits = c(-1, 1))
write.csv(lmm_out, "output/lmm_taxonomic_pca.csv")

# plots ------------------------------------------------------------------------

# # plot 1: variable selection (will be used for both)
png("figs/lmm_variable_corrplot.png", width = 1000, height = 1000, type = "quartz")
var_corplot %>% corrplot::corrplot(tl.cex = 2, tl.col = "black", cl.cex = 2, addCoef.col = "white")
dev.off()

# plot 2: model residuals
((resid_normal_m1/resid_fit_m1) | resid_map_m1) & 
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))
ggsave("figs/lmm_taxonomic_pca_residualplots.png", width = 12.4, height = 7)

# plot 3: model results
saveRDS(coefs, "output/lmm_taxonomic_pca_coefplot.rds")
