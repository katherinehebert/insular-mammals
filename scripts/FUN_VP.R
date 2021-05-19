# Function to do variation partitioning of simpson turnover into
# variation explained by all environmental variables and
# all spatial variables

VP <- function(bsim, env, fract.names, metaname, ses){
  
  require(vegan)
  require(ade4)
  require(adespatial)
  
  # load function to remove variables sequentially according to VIF
  source('scripts/FUN_removeVIF.R')
  
  # Subset environmental dataset
  longlat <- subset(env, select = c(Long, Lat))
  env <- subset(env, select = c(Area, Elev, Temp, varT, Prec, varP))
  # Log-transform environmental variables
  env$Area <- log10(env$Area)
  env$Elev <- log10(env$Elev)
  env[env == -Inf] <- 0
  # Standardize environmental data 
  env.std <- as.matrix(decostand(env, method = "standardize"))
  env.std <- as.matrix(removeVIF(bsim, env = env.std))
  
  # Spatial distance - spatial eigenfunctions
  bsim[is.na(bsim)] <- 0
  # compute dbMEM spatial eigenfunctions
  dbMEM <- as.matrix(dbmem(longlat, silent = FALSE)) 
  rownames(dbMEM) <- rownames(env.std)
  
  # Variation partitioning into fractions explained by environment & space
  (VP <- varpart(as.dist(bsim), dbMEM, env.std, add = ses))
  quartz()
  plot(VP, bg = c("deepskyblue1", "green3"), cex = 1.5, 
       Xnames = c("Space", "Env"), id.size = 1, alpha = 150, digits = 1, 
       cutoff = -1)
  
  # set number of partitions
  parts <- length(fract.names)
  
  # see variation partitioning table
  ind.fract <- VP$part$indfract
  fract <- VP$part$fract
  
  # get significance of each fraction
  fract.sig <- anova.cca(dbrda(as.dist(bsim) ~ dbMEM + env.std, add = ses),
                         by = "terms") 
  # get significance of full model
  full.sig <- anova.cca(dbrda(as.dist(bsim) ~ dbMEM + env.std, add = ses)) 
  
  # significance of each (testable) individual fraction of variation
  X1 <- anova.cca(dbrda(as.dist(bsim) ~ dbMEM + Condition(env.std), add = ses))
  X2 <- anova.cca(dbrda(as.dist(bsim) ~ env.std + Condition(dbMEM), add = ses))
  
  # create list
  ind.ls <- list(X1, X2)
  
  # add p-values to fraction results table
  fract$p <- NA
  for(i in 1:parts){fract$p[i] <- fract.sig$`Pr(>F)`[i]}
  fract$p[nrow(fract)] <- full.sig$`Pr(>F)`[1]
  rownames(fract)[c(1:parts,nrow(fract))] <- c(fract.names, "All")
  
  # Individual fractions
  ind.fract <- VP$part$indfract
  # add p-values to VP table for individual fractions
  ind.fract$p <- NA
  ind.fract$p[1] <- ind.ls[[1]]$`Pr(>F)`[1]
  ind.fract$p[3] <- ind.ls[[2]]$`Pr(>F)`[1]
  
  # write all VP results to table
  VP.res <- rbind(fract, ind.fract)
  
  # simplify rownames to fraction names
  rownames(VP.res) <- gsub(".* = ", "", rownames(VP.res))
  rownames(VP.res) <- gsub("X1", fract.names[1], rownames(VP.res))
  rownames(VP.res) <- gsub("X2", fract.names[2], rownames(VP.res))
  
  # round Radj to 3 decimal places
  VP.res$Adj.R.squared <- round(VP.res$Adj.R.squared, digits = 3)
  VP.res$R.squared <- round(VP.res$R.squared, digits = 3)
  # return the table
  return(VP.res)
}
