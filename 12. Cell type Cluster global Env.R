### Marjolen Droog
### September 15th, 2018
### The loop assigns clusters from the cell type 1-merged files to the Global Environment 
### Write the objects and files into the folder: "./07. Results cell type 1"

## Load required packages
source("01. RaceID3_StemID2.R")
library(tidyverse)
source("07. All cells Assign clusters.R")

# create a folder to store result-folders in 
if (!dir.exists(file.path("./07. Results cell type 1"))){
  dir.create(file.path("./07. Results cell type 1"))
}

# create a loop that creates one folder per condition of list_sc
for(n in 1:length(list_sc_CT1)){
  if (!dir.exists(file.path(paste("./07. Results cell type 1/", names(list_sc_CT1[n]), sep ="")))){
    dir.create(file.path(paste("./07. Results cell type 1/", names(list_sc_CT1[n]), sep ="")))
  }
}

# read input
list_CT1_sc <- list(sc_CT1_all_timep = readRDS("./05. Sc objects/sc_CT1_all_timep.rds"),
                   sc_CT1_1 = readRDS("./05. Sc objects/sc_CT1_1.rds"), 
                   sc_CT1_3 = readRDS("./05. Sc objects/sc_CT1_3.rds"))

# Loop 1: Assign clusters from the merged files to the Global Environment 
for(i in 1:length(list_CT1_sc)){
  cell_clusters <- data.frame(CELLID=names(list_CT1_sc[[i]]@cpart),cluster=list_CT1_sc[[i]]@cpart) # assign a cluster to each cell #
  cdiff <- clustdiffgenes(list_CT1_sc[[i]],pvalue=0.05) 
  # order according to fold increase
  cdiff <- lapply(cdiff, function(x) {
    z <- x[order(x[,3], decreasing = TRUE), ]
  })
} 

# Loop: Write clusters of each CT1_condition in an excell file into correspoding folders
for(n in 1:length(list_CT1_sc)) {
  sc <- list_CT1_sc[[n]]
  cell_clusters <- data.frame(CELLID=names(sc@cpart),cluster=sc@cpart)
  cdiff <- clustdiffgenes(sc,pvalue=0.05)
  # output:  ordered according to fold increase
  cdiff <- lapply(cdiff, function(x) {
    z <- x[order(x[,3], decreasing = TRUE), ]
  })
# write excel files
  x <- paste("./07. Results cell type 1/", names(list_CT1_sc[n]), "/clusters ", names(list_CT1_sc[n]), ".xlsx", sep = "")
  require(openxlsx)
  write.xlsx(cdiff, 
             file = x, 
             header = FALSE, 
             row.names = TRUE)
}

rm(list = ls())
