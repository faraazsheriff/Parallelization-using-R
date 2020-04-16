# =========================================================================================== #
# Title - Scalability of Elasticity Models
# Purpose - Parallel Computation of Elasticity Models
# Date - 24/03/2017
# Version - v.9
# =========================================================================================== #
#setting working directory
setwd("")

# Clear the environment and console once before start
rm(list = setdiff(ls(), c()))
cat("\014")

Start.time = Sys.time()

# Loading all the Libraries
if (!"pacman" %in% installed.packages()) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr,glmnet,data.table,doParallel,MASS,foreach,iterators,RJDBC) 

# Initializing Global variables and Running UDF's
source("User Defined Functions.R")

# Importing Dataset
ADS = fread("bounded_base_data_expanded_wk_iv_d90_yg.csv")

# Renaming column names by removing unnecessary words
col_names <- colnames(ADS)
dot_pos <- gregexpr(pattern ='.',
                    col_names[1], 
                    fixed = TRUE
)[[1]][1] + 1

colnames(ADS) <- substr(col_names,
                        dot_pos,
                        200)
# Defining the cluster
{ 
  cluster_nodes = c(rep("tstr500868",2),rep("tstr500869",2),rep("tstr500870",2),rep("tstr500871",2))
  cl = makeCluster(cluster_nodes, type = 'PSOCK')
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

# -------------------------------------------- Data Transformation & Filteration -------------------------------------------------- #
# Null value treatment and Data Filteration
ADS <- wrangle_at_one_node(ADS)

# -------------------------------------------- Running Regression Models ---------
# Exporting Data to all the nodes of the cluster
clusterExport(cl, "ADS")

# Creating a list of all libraries that need to be exported and loaded onto the nodes of the cluster
packages_list=c("dplyr","doParallel","foreach","glmnet","MASS","data.table")

# Storing the total number of unique UPCs present in the Data
unique_upc=length(unique(ADS$upc_nbr))

# Running the 3 Elasticity Models 
# Foreach function distributes the Modeling tasks of each UPC to one core of a node
foreach(i=1:unique_upc,.packages = packages_list, .verbose = T) %dopar% 
{ADS=as.data.table(ADS)
upc_data=ADS[upc_nbr == unique(ADS$upc_nbr)[i],]
upc_data=as.data.frame(upc_data)

upc_op <- get_upcs_models(upc_data)

upc_op$total_data_points <- as.numeric(upc_op$total_data_points)
upc_op$training_data_points <- as.numeric(upc_op$training_data_points)
upc_op$test_data_points <- as.numeric(upc_op$test_data_points)

# Saving the modeling results of each UPC as an rda file
assign(paste0("upc_op_",i), upc_op, envir = .GlobalEnv)
list_names = paste0("upc_op_",i)
save(list = list_names, file = paste0("upc_op_", i, ".rda"))
}

# Stopping the cluster
stopCluster(cl)

# Loading the rda files onto the global environment
for (i in 1:unique_upc) {
  load(paste0("upc_op_",i,".rda"), envir = .GlobalEnv)
}

# Appending the Modeling results of all UPC's stored as a table now
for (i in 1:unique_upc) {
  output = rbind(output, get(paste0("upc_op_", i)))
}

# Importing the final results
fwrite(output,"RESULTS_FINAL_MODIFIED.csv")

# Calculating Total run time
End.time = Sys.time()
Total_time = difftime(End.time,Start.time,units = "secs")
print(Total_time)
