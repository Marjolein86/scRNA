### Marjolen Droog
### September 15th, 2018
### t-SNE map single cells of Time point 1 and corresponding Control, Time point 2, Time point 3 and corresponding Control

options(stringsAsFactors = FALSE)

# Load required packages and functions
source("01. RaceID3_StemID2.R")

### Choose which data you want to analyse   
sc <- readRDS("./05. Sc objects/sc_ctrl1.rds") # Time point 1 control
# sc <- readRDS("./05. Sc objects/sc_ctrl3.rds") # Time point 3 control
# sc <- readRDS("./05. Sc objects/sc_timep1.rds") # Time point 1
# sc <- readRDS("./05. Sc objects/sc_timep2.rds") # Time point 2
# sc <- readRDS("./05. Sc objects/sc_timep3.rds") # Time point 3
# sc <- readRDS("./05. Sc objects/sc_1.rds") # Time point 1 and corresponding Control
# sc <- readRDS("./05. Sc objects/sc_3.rds") # Time point 3 and corresponding Control
# sc <- readRDS("./05. Sc objects/sc_all.rds") # all cells
# sc <- readRDS("./05. Sc objects/sc_all_timep.rds") # all cells of Time points without corresponding Controls
# sc <- readRDS("./05. Sc objects/sc_ctrl1_ctrl3.rds")  # cells Time point 1 Control and Time point 3 Control
# sc <- readRDS("./05. Sc objects/sc_CM_all_timep.rds") # Selected Cardiomyocytes all time-points without corresponding Controls
# sc <- readRDS("./05. Sc objects/sc_CM_1.rds") # Selected Cardiomyocytes Time point 1 and corresponding Control
# sc <- readRDS("./05. Sc objects/sc_CM_3.rds") # Selected Cardiomyocytes Time point 3 and corresponding Control

## functions showing expression of gene of interest in t-SNE map 
hlgene<- function(gene){
  g<-rownames(sc@ndata)[grep(gene, rownames(sc@ndata))]
  if ( length(g) >= 1 ){
    plotexptsne(sc, g, n = gene)
  } else {
    print ("check your gene name")
  }
}
## functions showing expression of gene of interest in t-SNE map on a log scale
hlgene_log<- function(gene){
  g<-rownames(sc@ndata)[grep(gene, rownames(sc@ndata))]
  if ( length(g) == 1 ){
    plotexptsne(sc, g, n = gene, logsc=T)
  } else {
    print ("check your gene name")
  }
}

hlgene("gene_of_interest")
hlgene_log("gene_of_interest")
 test_gene <- sc@ndata[grepl("gene_of_interest",rownames(sc@ndata)),]

# clear the global environment
rm(list = ls())
