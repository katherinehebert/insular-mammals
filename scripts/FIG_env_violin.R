# Script to create figure of environmental variables variation across
# metacommunities 

# clear workspace
rm(list=ls())

# load packages
require(tidyverse)

# import environmental datasets
env = readRDS("data/env_metacomm.RDS")
names(env)[c(5,6)] = c("MdtLB", "MdtOC")

# combine into one dataset
for(i in 1:length(env)){
  env[[i]]$group = names(env)[i]
  env[[i]]$island = rownames(env[[i]])}
env = do.call(rbind, env)
env$group = gsub("Ind", "Snd", env$group)

# transform variables
env$Area = log10(env$Area)
env$Elev = log10(env$Elev)
env$Prec = env$Prec/1000

# subset to variables used in study
env = subset(env, select = -c(X, Lat, Long, Dist, SLMP, GMMC, CCVT))

# palette from metacommunity/sp pool map
palette <- c("skyblue1", "#FD8E38", "#E942A6", "#FEDA28", "#21A723",
             "#BC43F1", "#28BD92", "#1C38F7", "#D73535")
names(palette) <-  c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", "Phl", "Snd", "Mlk", "Mln")


# plotting function to apply to each variable
envplot <- function(df, xvar = "condition", yvar = "means",
                     fillvar = "group", ylab, title) {
  p <- ggplot(data = df, aes_string(x = xvar, y= yvar, fill = fillvar)) +
    geom_violin(aes_string(col = fillvar), alpha = .7) +
      stat_summary(fun.y=mean, geom="point", size=1.2) + 
      stat_summary(fun.data = mean_se, geom = "errorbar", width = .5) +
    geom_hline(aes(yintercept = mean(yvar)), lty =2) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    theme_classic() +
    xlab(NULL) + ylab(ylab) + ggtitle(title) +
    theme(axis.text = element_text(size = 11),
          axis.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          plot.title = element_text(face = "bold"),
          legend.position = "none")
  return(p)
}

# save variable names and plot titles
vars = colnames(env)[2:7]
ylabs = c("Island area (km²)", "Maximum elevation (m)", 
              "Mean annual temperature (°C)", "Temperature seasonality (°C)", 
              "Precipitation (1000 mm)", "Precipitation seasonality (mm)")
titles = c("A", "B", "C", "D", "E", "F")



# create list of plots
p = list()
for(m in 1:length(vars)){
 p[[m]] = envplot(env, xvar = "group", yvar = vars[m], 
                  fillvar = "group", ylab = ylabs[m], title = titles[m])}
# arrange plots on one screen
ggpubr::ggarrange(plotlist = p, ncol = 2, nrow = 3)

# save figure
ggsave("figs/env_violin.png", width = 7, height = 7)
