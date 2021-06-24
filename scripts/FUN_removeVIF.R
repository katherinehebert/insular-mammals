# Function to sequentially remove highly correlated variables sequentially according to
# their variance inflation factor being over a given threshold
# Author: Katherine Hébert


removeVIF <- function(commdist, env){

for(i in 1:ncol(env)){
  
  commdist <- as.dist(commdist)
  env <- as.data.frame(env)
  
    # write global dbrda model to test for collinearity
    globalrda <- dbrda(commdist ~ ., data = env)
    
    # calculate the VIF of the environmental variables
    VIF <- as.matrix(vif.cca(globalrda))
    
    # if there are VIFs over 3: sequentially remove variables w/ the highest VIF value
    if(max(VIF) > 3){
      
      # order VIF values from largest to smallest
      VIF <- as.data.frame(VIF[order(VIF, decreasing = TRUE),])
      
      # extract name of variable to remove
      remove <- which(colnames(env) %in% rownames(VIF)[1]) 
      
      # remove variable 
      env <- env[,-remove]
    }
}
  return(env)
}
