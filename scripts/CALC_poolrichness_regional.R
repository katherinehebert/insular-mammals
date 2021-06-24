# Script to assemble taxonomic and phylogenetic diversity of the regional pools
# Author: Katherine Hébert

library(dplyr)

# load community matrix of the metacommunity as a whole (all sites x all species)
comms <- readRDS("data/comm_metacomm.RDS")

# load assemblage dispersion fields
adfs <- list(
  readRDS("output/regionalpoolAdr_25p.RDS"),
  readRDS("output/regionalpoolAlx_25p.RDS"),
  readRDS("output/regionalpoolClf_25p.RDS"),
  readRDS("output/regionalpoolSnd_25p.RDS"),
  readRDS("output/regionalpoolMdtLB_25p.RDS"),
  readRDS("output/regionalpoolMdtOC_25p.RDS"),
  readRDS("output/regionalpoolMlk_25p.RDS"),
  readRDS("output/regionalpoolMln_25p.RDS"),
  readRDS("output/regionalpoolPhl_25p.RDS")
)

# function to extract list of species in the pool
get_poollist <- function(adf, comm){
  comm_list <- colnames(comm)
  adf_list <- gsub(" ", "_", colnames(adf$PAM))
  pool <- c(comm_list, adf_list) %>% 
    # remove duplicated species
    unique()
  return(pool)
}

# get species list in each island pool
pools <- adfs
for(i in 1:length(adfs)){
    pools[[i]] <- get_poollist(adfs[[i]], comms[[i]])
  }
# save pool list here with island name
saveRDS(pools, "output/regionalpool_25p_specieslists.RDS")

# store in a dataframe
df <- data.frame(
    "group" = names(comms),
    # get taxonomic diversity ----
    "sp_rich" = lapply(pools, length) %>% unlist()
  )
df$group <- gsub("Ind", "Snd", df$group)
df$group[6] <- "MdtOC"

# save
saveRDS(df, "output/regionalpool_25p_diversity.RDS")

# load the diversity data frame
regionalpool_25p_diversity <- readRDS("~/Documents/GitHub/IslandMammals/output/regionalpool_25p_diversity.RDS")
colnames(regionalpool_25p_diversity)[2] <- c("tr_25p") 

# get phylogenetic diversity ----
sesmpd_25p <- list(
  readRDS("output/regionalpool_25p_sesmpd_Adr.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_Alx.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_Clf.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_Snd.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_MdtLB.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_MdtOC.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_Mlk.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_Mln.RDS"),
  readRDS("output/regionalpool_25p_sesmpd_Phl.RDS")
)
names(sesmpd_25p) <- c("Adr", "Alx", "Clf", "Snd", "MdtLB", "MdtOC", "Mlk", "Mln", "Phl")

# extract sesmpd per island

df2 <- data.frame(
  "group" = names(sesmpd_25p),
  "sesmpd_25p" = NA,
  "sesmpd_25p_p" = NA)
for(i in 1:length(sesmpd_25p)){
    df2$sesmpd_25p[i] <- sesmpd_25p[[i]]$mpd.obs.z[1]
    df2$sesmpd_25p_p[i] <- sesmpd_25p[[i]]$mpd.obs.p[1]
  }

# join to pool diversity
df <- left_join(regionalpool_25p_diversity, df2)
# save
saveRDS(df, "output/regionalpool_diversity.RDS")
write_csv(df, "output/regionalpool_diversity.csv")
