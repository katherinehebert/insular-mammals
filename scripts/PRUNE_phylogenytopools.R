# Script to prune phylogenetic tree to the regional species pools (25p)
# Author: Katherine Hébert
# Date: 5/1/18
# Update: 14/01/2021

# clear workspace
rm(list=ls())

# load packages
require(picante)
require(dplyr)
require(tidyr)
require(phytools)

# import phylogenetic tree
phy.fritz <- read.nexus("data/raw/Phylogeny/ELE_1307_sm_SA1.nexus")[[1]] # "best dates" tree

pool <- readRDS("output/pool_perisland_specieslists.RDS")

# import phylogeny pool species synonyms (found manually on ITIS and IUCN)
syn <- read.csv("data/raw/Name corrections/Phylogeny_25pPoolMismatches.csv", row.names = 1)
syn$Mismatched <- gsub(" ", "_", syn$Mismatched)
syn$Synonym <- gsub(" ", "_", syn$Synonym)
syn <- syn[!(is.na(syn$Synonym)),]
for(i in 1:length(pool)){
  for(n in 1:length(pool[[i]])){
    if(pool[[i]][n] %in% syn$Mismatched){
      pool[[i]][n] <- syn[which(syn$Mismatched %in% pool[[i]][n]),"Synonym"]
    }
  }
}

# Add species randomly to root of the genus (newly discovered, or phylogenetically undescribed species)
# check for overlap between these island groups.. do not make table about how many species
# were replaced based on these numbers of lines.

# Philippines:
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_aurorae", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_banahao", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_brownorum", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_camiguinensis", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_gracilirostris", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_iridensis", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_magnus", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_minganensis", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_sierrae", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Apomys_zambalensis", genus = "Apomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Archboldomys_maximus", genus = "Archboldomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Archboldomys_kalinga", genus = "Archboldomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Archboldomys_musseri", genus = "Archboldomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Batomys_hamiguitan", genus = "Batomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Batomys_russatus", genus = "Batomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Batomys_uragon", genus = "Batomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Crocidura_hutanis", genus = "Crocidura", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Crocidura_ninoyi", genus = "Crocidura", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Crocidura_panayensis", genus = "Crocidura", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Crunomys_suncoides", genus = "Crunomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Sus_oliveri", genus = "Sus", where = "random")

# Melanesia:
phy.fritz <- add.species.to.genus(phy.fritz, species = "Dendrolagus_mbaiso", genus = "Dendrolagus", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Microperoryctes_aplini", genus = "Microperoryctes", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Melomys_matambuai", genus = "Melomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Melomys_sp.", genus = "Melomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Microperoryctes_aplini", genus = "Microperoryctes", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Myoictis_leucura", genus = "Myoictis", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Myoictis_wavicus", genus = "Myoictis", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Paramelomys_gressitti", genus = "Paramelomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Phalanger_alexandrae", genus = "Phalanger", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Phalanger_rothschildi", genus = "Phalanger", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Pseudohydromys_germani", genus = "Pseudohydromys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Rattus_arfakienis", genus = "Rattus", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Rattus_sp.", genus = "Rattus", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Spilocuscus_papuensis", genus = "Spilocuscus", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Spilocuscus_wilsoni", genus = "Spilocuscus", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Thylogale_calabyi", genus = "Thylogale", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Uromys_boeadii", genus = "Uromys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Uromys_imperator", genus = "Uromys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Uromys_porculus", genus = "Uromys", where = "random")

# Maluku:
phy.fritz <- add.species.to.genus(phy.fritz, species = "Crocidura_musseri", genus = "Crocidura", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Tarsius_lariang", genus = "Tarsius", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Tarsius_tumpara", genus = "Tarsius", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Tarsius_wallacei", genus = "Tarsius", where = "random")

# Indonesia:
phy.fritz <- add.species.to.genus(phy.fritz, species = "Bullimus_gamay", genus = "Bullimus", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Callosciurus_erythraeus", genus = "Callosciurus", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Macaca_siberu", genus = "Macaca", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Rhynchomys_banahao", genus = "Rhynchomys", where = "random")
phy.fritz <- add.species.to.genus(phy.fritz, species = "Rhynchomys_tapulao", genus = "Rhynchomys", where = "random")

## SECTION FOR REGIONAL SPECIES POOLS ONLY ####

# # Prune and match phylogenetic tree to each regional species pool 
# # create blank list to store mismatched species names
# mismatches <- list()
# # save mismatched species names
# for(i in 1:length(pool)){
#   pruned.pool <- drop.tip(phy.fritz, phy.fritz$tip.label[!(phy.fritz$tip.label %in% pool[[i]])])
#   mismatches[[i]] <- pool[[i]][-which(pool[[i]] %in% intersect(phy.fritz$tip.label, pool[[i]]))]
# }
# # consolidate list into 1 big vector with only unique mismatched species names
# (mismatches.v <- as.data.frame(as.matrix(sort(unique(unlist(mismatches))))))
# write.csv(mismatches.v, "~/Desktop/Phylogeny_PoolMismatches.csv")
# 
# mismatches.split <- separate(mismatches.v, col = "V1", into = c("Genus", "Species"), sep = "_")
# mismatches.split$Binomial <- mismatches.v$V1
# mismatches.split <- mismatches.split[-1,]
# 
# for(i in 1:nrow(mismatches.split)){
#   phy.fritz <- add.species.to.genus(phy.fritz, species = mismatches.split$Binomial[i],
#                                     genus = mismatches.split$Genus[i], where = "random")
# }

# # Prune and match phylogenetic tree to each regional species pool 
# # create blank list to store mismatched species names
# mismatches.new <- list()
# # save mismatched species names
# for(i in 1:length(pool)){
#   pruned.pool <- drop.tip(phy.fritz, phy.fritz$tip.label[!(phy.fritz$tip.label %in% pool[[i]])])
#   mismatches.new[[i]] <- pool[[i]][-which(pool[[i]] %in% intersect(phy.fritz$tip.label, pool[[i]]))]
# }
# # consolidate list into 1 big vector with only unique mismatched species names
# (mismatches.new.v <- sort(unique(unlist(mismatches.new)))) # 14 mismatches! genus not in the tree, so will leave out.

### done! back to any type of pool:

# Prune tree to each 25p pool
pool.phy <- list()
for(i in 1:length(pool)){
  pool.phy[[i]] <- list()
  for(n in 1:length(pool[[i]])){
    pool.phy[[i]][[n]] <- drop.tip(phy.fritz, phy.fritz$tip.label[!(phy.fritz$tip.label %in% pool[[i]][[n]])])
  }
  names(pool.phy[[i]]) <- names(pool[[i]])
}
names(pool.phy) <- names(pool)
saveRDS(pool.phy, "output/pool_perisland_phylogenies.RDS")

# write editted tree to file
write.tree(phy.fritz, "data/raw/Phylogeny/ELE_1307_sm_SA1_EDITTED.nexus")


# prune tree to regional pool --------------------------------------------------
regpool <- readRDS("output/regionalpool_25p_specieslists.RDS")
phy.fritz <- read.tree("data/raw/Phylogeny/ELE_1307_sm_SA1_EDITTED.nexus")

pool.phy.reg <- list()
for(i in 1:length(regpool)){
    pool.phy.reg[[i]] <- drop.tip(phy.fritz, phy.fritz$tip.label[!(phy.fritz$tip.label %in% regpool[[i]])])
  }
names(pool.phy.reg) <- c("Adr","Alx", "Clf", "Snd", "MdtLB", "MdtOC", "Mlk", "Mln", "Phl")
saveRDS(pool.phy.reg, "output/regionalpool_25p_phylogenies.RDS")
