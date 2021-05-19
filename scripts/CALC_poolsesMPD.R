# Taxonomic, functional, and phylogenetic diversity in the species pools

# clear workspace
rm(list=ls())

# load packages
require(vegan)
require(picante)
require(dplyr)

## TAXONOMIC DATA ##
pool_sp <- readRDS("output/pool_perisland_specieslists.RDS")
names(pool_sp) <- c("Adr", "Alx", "Clf", "Snd", "MdtLB", "MdtOC", "Mlk", "Mln", "Phl")
# PHYLOGENETIC DATA ##
pool_phy <- readRDS("output/pool_perisland_phylogenies.RDS")
names(pool_phy) <- c("Adr", "Alx", "Clf", "Snd", "MdtLB", "MdtOC", "Mlk", "Mln", "Phl")

# TREE: Full mammalian phylogenetic tree
phy.fritz <- read.tree("data/raw/Phylogeny/ELE_1307_sm_SA1_EDITTED.nexus")
# remove duplicated species
phy.fritz <- drop.tip(phy.fritz, phy.fritz$tip.label[which(duplicated(phy.fritz$tip.label))])
# remove bats
require(ape)
data(chiroptera)
phy.fritz <- drop.tip(phy.fritz, tip = chiroptera$tip.label)


#### Phylogenetic diversity ####

calc_sesmpd <- function(pool_species, phy = phy.fritz){
  
  # Creating one "community" matrix for the phylogeny pool
  m <- matrix(0, nrow = 2, ncol = length(phy.fritz$tip.label)) %>% as.data.frame()
  colnames(m) <- phy.fritz$tip.label
  m[2,] <- 1 # mark all species as present in the pool site (#2)
  
  # mark each species in the pool as "present"
  m[,which(colnames(m) %in% pool_species)] <- 1
  
  # match community matrix and phylogeny
  m_phy <- match.phylo.comm(phy.fritz, m)
  
  # compute sesMPD
  # null model: phylogeny.pool - Randomize community data matrix by drawing species from pool of species
  # occurring in the distance matrix (phylogeny pool) with equal probability
  SESMPD <- ses.mpd(m_phy$comm, 
                    cophenetic.phylo(m_phy$phy),
                    null.model = "phylogeny.pool", 
                    abundance.weighted = FALSE)
  return(SESMPD)
}

# get sesmpd of each island's pool
pool_phydiv <- vector("list", length = length(pool_sp))
for(i in 2:length(pool_sp)){
  temp <- vector("list", length = length(pool_sp[[i]]))
  names(temp) <- names(pool_sp[[i]])
  
  for(n in 1:length(pool_sp[[i]])){
    temp[[n]] <- calc_sesmpd(pool_species = pool_sp[[i]][[n]])
  }
  saveRDS(temp, 
          paste0("output/pool_perisland_sesmpd_", names(pool_sp)[i], ".RDS"))
}

# get regional pool sesmpd (25p cutoff)
regpool <- readRDS("output/regionalpool_25p_specieslists.RDS")
names(regpool) <- c("Adr","Alx", "Clf", "Snd", "MdtLB", "MdtOC", "Mlk", "Mln", "Phl")

regpool_phydiv <- vector("list", length = length(regpool))
for(i in 1:length(regpool_phydiv)){
    temp <- calc_sesmpd(pool_species = regpool[[i]], phy = phy.fritz)
  saveRDS(temp, 
          paste0("output/regionalpool_25p_sesmpd_", names(regpool)[i], ".RDS"))
}

# get regional pool sesmpd (50p cutoff)
regpool <- readRDS("output/regionalpool_50p_specieslists.RDS")
names(regpool) <- c("Adr","Alx", "Clf", "Snd", "MdtLB", "MdtOC", "Mlk", "Mln", "Phl")

regpool_phydiv <- vector("list", length = length(regpool))
for(i in 1:length(regpool_phydiv)){
  temp <- calc_sesmpd(pool_species = regpool[[i]], phy = phy.fritz)
  saveRDS(temp, 
          paste0("output/regionalpool_50p_sesmpd_", names(regpool)[i], ".RDS"))
}
