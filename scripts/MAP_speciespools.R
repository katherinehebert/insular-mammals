# Script to map regional pools in Figure 1 (speciespools_map.png) 
# The regional pools are produced with MAKE_regionalpool.R
# Author: Katherine Hébert

# set up ----
library(sf)
library(ggplot2)
library(dplyr)

# get world map
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")

# function to read pool grids and dissolve the grid cells
grid_diss <- function(path){
  temp <- readRDS(path)
  temp <- temp$grid %>% st_as_sf() %>% group_by(featurecla) %>% summarise()
  return(temp)
}

# get pool grids
adr <- grid_diss("output/regionalpoolAdr_25p.rds") 
alx <- grid_diss("output/regionalpoolAlx_25p.rds")
clf <- grid_diss("output/regionalpoolClf_25p.rds")
snd <- grid_diss("output/regionalpoolSnd_25p.rds")
mdtlb <- grid_diss("output/regionalpoolMdtLB_25p.rds")
mdtoc <- grid_diss("output/regionalpoolMdtOC_25p.rds")
mlk <- grid_diss("output/regionalpoolMlk_25p.rds")
mln <- grid_diss("output/regionalpoolMln_25p.rds")
phl <- grid_diss("output/regionalpoolPhl_25p.rds")

# function to read and remove unneeded features
read_clean <- function(path){
  temp <- st_read(path)
  temp <- subset(temp, select = c(ID_0))
  return(temp)
}

# get archipelago polygons
# these are clipped polygons of the island shapes
# extracted from GADM shapefiles in QGIS
adr_arch <- read_clean("qgis/outputs/islands/adr_islands.shp")
alx_arch <- read_clean("qgis/outputs/islands/alx_islands.shp")
clf_arch <- read_clean("qgis/outputs/islands/clf_islands.shp")
snd_arch <- read_clean("qgis/outputs/islands/snd_islands.shp")
mdtlb_arch <- read_clean("qgis/outputs/islands/mdtlb_islands.shp")
mdtoc_arch <- read_clean("qgis/outputs/islands/mdtoc_islands.shp")
mlk_arch <- read_clean("qgis/outputs/islands/mlk_islands.shp")
mln_arch <- read_clean("qgis/outputs/islands/mln_islands.shp")
phl_arch <- read_clean("qgis/outputs/islands/phl_islands.shp")


# palette from metacommunity/sp pool map
cols <- c("Alx" = "skyblue1",
         "Clf" = "#FD8E38",
         "MdtLB" = "#E942A6",
         "MdtOC" = "#FEDA28",
         "Adr" = "#21A723",
         "Phl" = "#BC43F1",
         "Snd" = "#28BD92",
         "Mlk" = "#1C38F7",
         "Mln" = "#D73535")

# map
quartz()
ggplot() +
  theme_void() +
  geom_sf(data = world, col = "grey", fill = "grey") +
  #adriatic
  geom_sf(data = adr, fill = "#21A723", col = "#21A723") +
  geom_sf(data = adr_arch, fill = "#21A723", col = "#21A723") +
  # alexander
  geom_sf(data = alx, fill = "skyblue1", col = "skyblue1") +
  geom_sf(data = alx_arch, fill = "skyblue1", col = "skyblue1") +
  # california
  geom_sf(data = clf, fill = "#FD8E38", col = "#FD8E38") +
  geom_sf(data = clf_arch, fill = "#FD8E38", col = "#FD8E38") +
  # sundaland
  geom_sf(data = snd, fill = "#28BD92", col = "#28BD92") +
  geom_sf(data = snd_arch, fill = "#28BD92", col = "#28BD92") +
  # mediterranean (lb)
  geom_sf(data = mdtlb, fill = "#E942A6", col = "#E942A6") +
  geom_sf(data = mdtlb_arch, fill = "#E942A6", col = "#E942A6") +
  # mediterranean (oc)
  geom_sf(data = mdtoc, fill = "#FEDA28", col = "#FEDA28") +
  geom_sf(data = mdtoc_arch, fill = "#FEDA28", col = "#FEDA28") +
  # maluku
  geom_sf(data = mlk, fill = "#1C38F7", col = "#1C38F7") +
  geom_sf(data = mlk_arch, fill = "#1C38F7", col = "#1C38F7") +
  # melanesia
  geom_sf(data = mln, fill = "#D73535", col = "#D73535") +
  geom_sf(data = mln_arch, fill = "#D73535", col = "#D73535") +
  # philippines
  geom_sf(data = phl, fill = "#BC43F1", col = "#BC43F1") +
  geom_sf(data = phl_arch, fill = "#BC43F1", col = "#BC43F1")
ggsave("figs/speciespools_map.png", width = 8.87, height = 7.92)
                    
