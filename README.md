# Scalability-using-R
#setting working directory
setwd("C:/Users/faraaz.sheriff/Desktop/Walmart/Pricing Platform/Projects/Elasticity Models/Elasticity Models - Codes & Data")

# Clear the environment and console once before start
rm(list = setdiff(ls(), c()))
cat("\014")

Start.time = Sys.time()

# Loading all the Libraries
if (!"pacman" %in% installed.packages()) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr,glmnet,data.table,doParallel,MASS,foreach,iterators,RJDBC) 

# Importing Dataset
ADS = fread("Dataset.csv")

# Defining the cluster
{ 
  cl = makeCluster(3, type = 'PSOCK')
  cluster_spec = clusterCall(cl, function() Sys.info()[c("nodename","machine")])
  print(cluster_spec)
  registerDoParallel(cl)
}

# Exporting all user defined functions to all the nodes of the cluster
kX <- lsf.str()
list_fun <- as.vector(kX)
for( items in list_fun){
  clusterExport(cl, items)
}
