# Script to make species pool per island =======================================

# set-up ====
library(dplyr)
library(maptools)
library(vegan)
library(letsR)
library(sf)
library(sp)

# import data ====

# environment of the metacommunity
env_metacomm <- readRDS("data/env_metacomm.RDS") 
env_metacomm <- lapply(env_metacomm, subset, select = c(Temp, varT, Prec, varP))

# import result of Species pools only restricted species in environmentally similar grid cells.R
# i.e. environment of each grid cell where at least 1 species from the metacommunity is present
# (excluding widespread species)
filepath <- "~/Documents/Concordia/Manuscript/R/Species pools/Grid shapefiles/Per archipelago/Restricted species/Bioclim means/"
env_adf <- paste0(filepath, list.files(path = filepath, pattern="*.shp"))
env_adf <- lapply(setNames(env_adf, make.names(gsub(filepath, "", env_adf))), sf::read_sf)
names(env_adf) <- gsub("_MeanBioclim.shp", "", names(env_adf))

# set up coordinate system: geographical, datum WGS84
crs.geo <- CRS("+proj=longlat +ellps=WGS84 + datum=WGS84") 
# import IUCN terrestrial mammals shapefiles
terrestrialmammals <- readShapePoly("~/Documents/Concordia/Manuscript/R/Species pools/IUCN Spatial data/TERRESTRIAL_MAMMALS.shp", 
                                    proj4string = crs.geo)
# keep only extant, native species
terrestrialmammals <- terrestrialmammals[terrestrialmammals$presence == 1,] # keep only extant species
terrestrialmammals <- terrestrialmammals[terrestrialmammals$origin == 1,] # keep only native species
terrestrialmammals <- terrestrialmammals[-(terrestrialmammals$order_name %in% "CHIROPTERA"),] # remove bats


# get grid cells to include in pool =====

# subset to climate variables
env_metacomm <- lapply(env_metacomm, subset, select = c(Temp, varT, Prec, varP))
env_adf_df <- lapply(env_adf, sf::st_drop_geometry) %>%
  lapply(subset, select = c(id, bio1_temp, bio4_varT, bio12_prec, bio15_varP)) %>%
  lapply(rename, 
         "Temp" = "bio1_temp",
         "varT" = "bio4_varT",
         "Prec" = "bio12_prec",
         "varP" = "bio15_varP")
env_adf_unlist <- bind_rows(env_adf_df, .id = "archipelago")


# function to do PCA on all grid cells and the one island point
clim_dist <- function(env_comm, env_adf, percent_sim){
  
  # take mean climate conditions in metacommunity
  env_mean <- apply(env_comm, 2, mean, na.rm = TRUE) %>% t() %>% as.data.frame()
  # PCA 
  env_mean$id <- 1
  env_mean <- subset(env_mean, select = c(id, Temp, varT, Prec, varP))
  temp <- rbind(env_mean, env_adf)
  temp <- temp[,-1]
  temp <- apply(temp, 2, decostand, method = "standardize")
  pca <- vegan::rda(temp)
  
  # Euclidean distance between island (row 1) and each grid cell after
  # function to calculate euclidean distance between 2 points
  dist = data.frame(id = c(rownames(env_mean), env_adf$id), dist = NA)
  for(i in 2:nrow(pca$CA$u)){
    dist[i,2] <- sqrt(sum((pca$CA$u[1,1] - pca$CA$u[i,1])^2)) # euclidean distance
  }
  
  # environmental filter
  if(percent_sim == 25){
    cutoff <- quantile(dist[,2], na.rm = TRUE)[2]
  } else if(percent_sim == 50){
    cutoff <- quantile(dist[,2], na.rm = TRUE)[3]
  }
  
  # pool with environmental filter applied
  regional_pool <- dist[which(dist[,2] <= cutoff),]
  return(regional_pool)
}

# get grid cells!
pool <- vector("list", length(env_metacomm))
for(i in 1:length(env_metacomm)){
  # make list to store distances per island
  pool[[i]] <- clim_dist(env_comm = env_metacomm[[i]], 
                                env_adf = env_adf_df[[i]],
                                percent_sim = 25
    )
  } 

# subset original grids with these selected ids
poolgrid <- pool
for(i in 1:length(poolgrid)){
    poolgrid[[i]] <- env_adf[[i]][which(env_adf[[i]]$id %in% pool[[i]][,1]),] 
  }

# convert to sp object
poolgrid_sp<- lapply(poolgrid, FUN = function(x){as(x, "Spatial")})

# set projection
for(i in 1:length(poolgrid)) {
    proj4string(poolgrid_sp[[i]]) <- crs.geo
}

# get presence/absence matrix per grid cell
names(env_adf) <- gsub("Ind", "Snd", names(env_adf))
pool_presab <- vector("list", length = 9)
for(i in 1:length(pool_presab)){
    pool_presab[[i]] <- lets.presab.grid(terrestrialmammals, 
                                              poolgrid_sp[[i]], 
                                              "id")
    saveRDS(pool_presab[[i]], paste0("output/regionalpool", names(env_adf)[i], "_25p.rds"))
}
