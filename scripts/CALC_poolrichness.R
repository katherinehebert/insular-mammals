# Script to get taxonomic and phylogenetic diversity of the per-island pools
# Author: Katherine Hébert

library(dplyr)

# taxonomic diversity ----
comms <- readRDS("data/comm_metacomm.RDS")
adfs <- list(
  readRDS("output/pool_perisland_Adr.RDS"),
  readRDS("output/pool_perisland_Alx.RDS"),
  readRDS("output/pool_perisland_Clf.RDS"),
  readRDS("output/pool_perisland_Snd.RDS"),
  readRDS("output/pool_perisland_MdtLB.RDS"),
  readRDS("output/pool_perisland_MdtOC.RDS"),
  readRDS("output/pool_perisland_Mlk.RDS"),
  readRDS("output/pool_perisland_Mln.RDS"),
  readRDS("output/pool_perisland_Phl.RDS")
)
names(adfs[[1]]) <- rownames(comms[[1]])
adfs[[1]][[14]] <- NULL

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
  for(n in 1:length(adfs[[i]])){
    pools[[i]][[n]] <- get_poollist(adfs[[i]][[n]], comms[[i]])
  }
}
# save pool list here with island name
saveRDS(pools, "output/pool_perisland_specieslists.RDS")

# store in a dataframe
df <- list()
for(i in 1:length(pools)){
  df[[i]] <- data.frame(
    "island" = names(pools[[i]]),
    # get taxonomic diversity ----
    "sp_rich" = lapply(pools[[i]], length) %>% unlist()
  )
}
names(df) <- gsub("Ind", "Snd", names(comms))
names(df)[6] <- "MdtOC"
# collapse into one df
df <- bind_rows(df, .id = "group")
# save
saveRDS(df, "output/pool_perisland_diversity.RDS")



# get phylogenetic diversity ----
sesmpd <- list(
  readRDS("output/pool_perisland_sesmpd_Adr.RDS"),
  readRDS("output/pool_perisland_sesmpd_Alx.RDS"),
  readRDS("output/pool_perisland_sesmpd_Clf.RDS"),
  readRDS("output/pool_perisland_sesmpd_Snd.RDS"),
  readRDS("output/pool_perisland_sesmpd_MdtLB.RDS"),
  readRDS("output/pool_perisland_sesmpd_MdtOC.RDS"),
  readRDS("output/pool_perisland_sesmpd_Mlk.RDS"),
  readRDS("output/pool_perisland_sesmpd_Mln.RDS"),
  readRDS("output/pool_perisland_sesmpd_Phl.RDS")
)
names(sesmpd) <- c("Adr", "Alx", "Clf", "Snd", "MdtLB", "MdtOC", "Mlk", "Mln", "Phl")

# extract sesmpd per island

sesmpd_ls <- vector("list", length = length(sesmpd))
for(i in 1:length(sesmpd_ls)){
  df_arch <- data.frame(
    "group" = rep(names(sesmpd)[i], length(sesmpd[[i]])),
    "island" = names(sesmpd[[i]]),
    "sesmpd" = NA,
    "sesmpd_p" = NA)
  for(n in 1:length(sesmpd[[i]])){
    df_arch$sesmpd[n] <- sesmpd[[i]][[n]]$mpd.obs.z[1]
    df_arch$sesmpd_p[n] <- sesmpd[[i]][[n]]$mpd.obs.p[1]
  }
  sesmpd_ls[[i]] <- df_arch
}
sesmpd_df <- bind_rows(sesmpd_ls)

# join to pool diversity
df <- left_join(df, sesmpd_df)
# save
saveRDS(df, "output/pool_perisland_diversity.RDS")
