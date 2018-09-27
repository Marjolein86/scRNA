### Marjolen Droog
### September 15th, 2018
### A loop for all pdf files of the csingle cell data, used to create and write into the folder: "./06. Results all cell types":
      # an excel file with the parameters used in this analysis
      # an excel file with the number of cells per condition and their corresponding cell types
      # an excel file with the identified differentially regulated genes for each cluster in comparison to the ensemble of all cells
      # t-sne maps of several cell-type markers 
      # t-sne map of cells colored by condition to which the cells belong

# Load required packages and functions
source("01. RaceID3_StemID2.R")
library(tidyverse)
require(openxlsx)

# read in the merged files
list_merged_files <- list(sc_ctrl1 = readRDS("./02. Merged objects/merged_ctrl1.rds") , # It's important to name each object in the list --> this name is used in the for loop
                          sc_ctrl3 = readRDS("./02. Merged objects/merged_ctrl3.rds"), 
                          sc_timep1 = readRDS("./02. Merged objects/merged_timep1.rds"), 
                          sc_timep2 = readRDS("./02. Merged objects/merged_timep2.rds"), 
                          sc_timep3 = readRDS("./02. Merged objects/merged_timep3.rds"), 
                          sc_1 = readRDS("./02. Merged objects/merged_1.rds"),
                          sc_3 = readRDS("./02. Merged objects/merged_3.rds"), 
                          sc_all = readRDS("./02. Merged objects/merged_all.rds"), 
                          sc_all_timep = readRDS("./02. Merged objects/merged_all_timep.rds"), 
                          sc_ctrl1_ctrl3 = readRDS("./02. Merged objects/merged_ctrl1_ctrl3.rds") )

# This list is used for multiple loops in this file 
list_sc <- list(sc_ctrl1 = readRDS("./05. Sc objects/sc_ctrl1.rds") , # It's important to name each object in the list --> this name is used in the for loop
                sc_ctrl3 = readRDS("./05. Sc objects/sc_ctrl3.rds"), 
                sc_timep1 = readRDS("./05. Sc objects/sc_timep1.rds"), 
                sc_timep2 = readRDS("./05. Sc objects/sc_timep2.rds"), 
                sc_timep3 = readRDS("./05. Sc objects/sc_timep3.rds"), 
                sc_1 = readRDS("./05. Sc objects/sc_1.rds"),
                sc_3 = readRDS("./05. Sc objects/sc_3.rds"), 
                sc_all = readRDS("./05. Sc objects/sc_all.rds"), 
                sc_all_timep = readRDS("./05. Sc objects/sc_all_timep.rds"), 
                sc_ctrl1_ctrl3 = readRDS("./05. Sc objects/sc_ctrl1_ctrl3.rds") )

# create a folder to store result-folders in 
if (!dir.exists(file.path("./06. Results all cell types"))){
  dir.create(file.path("./06. Results all cell types"))
}

# create a loop that creates one folder per condition of list_sc
for(n in 1:length(list_sc)){
  if (!dir.exists(file.path(paste("./06. Results all cell types/", names(list_sc[n]), sep ="")))){
    dir.create(file.path(paste("./06. Results all cell types/", names(list_sc[n]), sep ="")))
  }
}

## functions showing expression of gene of interest in t-SNE map ##
hlgene<- function(gene){
  g<-rownames(sc@ndata)[grep(gene, rownames(sc@ndata))]
  if ( length(g) >= 1 ){
    plotexptsne(sc, g, n = gene)
  } else {
    print ("be more specific")
  }
}

hlgene_log<- function(gene){
  g<-rownames(sc@ndata)[grep(gene, rownames(sc@ndata))]
  if ( length(g) == 1 ){
    plotexptsne(sc, g, n = gene, logsc=T)
  } else {
    print ("be more specific")
  }
}

## FUNCTION: Creates pdf images of t-SNE of cell-type marker levels
# x: sc object (created by RACEid3, hence, gene-filtered, k-medioids clustered, tsne-computed and filtered for outliers)
markers.pdf <- function(x){
  vector_markers <- c("1. Gene_1" = "Gene_1",
                      "2. Gene_2" = "Gene_2",
                      "3. Gene_3" = "Gene_3",
                      "4. Gene_4" = "Gene_4",
                      "5. Gene_5" = "Gene_5",
                      "6. Gene_6" = "Gene_6",
                      "7. Gene_7" = "Gene_7",
                      "8. Gene_8" = "Gene_8",
                      "9. Gene_9" = "Gene_9",
                      "10. Gene_10" = "Gene_10",
                      "11. Gene_11" = "Gene_11",
                      "12. Gene_12" = "Gene_12",
                      "13. Gene_13" = "Gene_13",
                      "14. Gene_14" = "Gene_14",
                      "15. Gene_15" = "Gene_15",
                      "16. Gene_16" = "Gene_16",
                      "17. Gene_17" = "Gene_17",
                      "18. Gene_18" = "Gene_18",
                      "19. Gene_19" = "Gene_19")
  for(i in 1:length(vector_markers)){
    pdf(paste("./06. Results all cell types/", names(list_sc[n]), "/", names(vector_markers[i]), ".pdf", sep = ""))
    hlgene_log(vector_markers[i])
    dev.off()
  }
} 



# A loop for figures and excel files
for(n in 1:length(list_sc)) {
  # Activate a specific sc object from list_sc
  sc <- list_sc[[n]]  # read input
  
  # Show in an excel files the paramter settings that were used per analysis
  fpar <- data.frame(parameter = names(sc@filterpar[-c(8,9)]), #these are the filter settings
                     setting = c(sc@filterpar[[1]], 
                                 sc@filterpar[[2]], 
                                 sc@filterpar[[3]], 
                                 sc@filterpar[[4]],  
                                 as.character(sc@filterpar[[5]]),  
                                 sc@filterpar[[6]],  
                                 as.character(sc@filterpar[[7]])))
  cpar <- data.frame(parameter = names(sc@clusterpar), # these are the cluster settings
                     setting = c(sc@clusterpar[[1]], 
                                 sc@clusterpar[[2]], 
                                 sc@clusterpar[[3]], 
                                 as.character(sc@clusterpar[[4]]),  
                                 as.character(sc@clusterpar[[5]]),  
                                 sc@clusterpar[[6]],  
                                 sc@clusterpar[[7]], 
                                 sc@clusterpar[[8]], 
                                 sc@clusterpar[[9]], 
                                 sc@clusterpar[[10]],  
                                 sc@clusterpar[[11]],  
                                 as.character(sc@clusterpar[[12]])))  
  par_list <- list(filter = fpar, clustesr = cpar, outlier = sc@outlierpar)
  require(openxlsx)
  write.xlsx(par_list, 
             file = paste("./06. Results all cell types/", names(list_sc[n]), "/parameters", ".xlsx", sep = ""), 
             header = TRUE, 
             row.names = TRUE)
  
  # Fourth step in the loop: Write the cell cluster size per analysis in an excel file
  cell_clusters <- data.frame(CELLID=names(sc@cpart),cluster=sc@cpart)
  # cells per cluster    #
  cell_cluster_size <- data.frame(table(cell_clusters$cluster))                               
  colnames(cell_cluster_size) <- c("cluster.nr", 
                                   "nr._of_cells")
  write.xlsx(cell_cluster_size, 
             file = paste("./06. Results all cell types/", names(list_sc[n]), "/cluster_size", ".xlsx", sep = ""), 
             header = FALSE, 
             row.names = TRUE)
  
  # Write clusters of each condition in an excell file into correspoding folders
  cell_clusters <- data.frame(CELLID=names(sc@cpart),cluster=sc@cpart)
  cdiff <- clustdiffgenes(sc,pvalue=1)
  # output:  ordered according to fold increase
  cdiff <- lapply(cdiff, function(x) {
    z <- x[order(x[,3], decreasing = TRUE), ]
  })
  write.xlsx(cdiff, 
             file = paste("./06. Results all cell types/", names(list_sc[n]), "/clusters", ".xlsx", sep = ""), 
             header = FALSE, 
             row.names = TRUE)  
  
  # Create RACEid plots
  tryCatch({markers.pdf(sc)},
           error=function(i){return(NA)})
  pdf(paste("./06. Results all cell types/", names(list_sc[n]),"/00. plotsaturation_", names(list_sc[n]), ".pdf", sep = ""))
  plotsaturation(sc)
  dev.off()
  pdf(paste("./06. Results all cell types/", names(list_sc[n]),"/00. plotjaccard_", names(list_sc[n]), ".pdf", sep = ""))
  plotjaccard(sc)
  abline(h= 0.6)
  dev.off()
  pdf(paste("./06. Results all cell types/", names(list_sc[n]),"/00. plotoutliers_", names(list_sc[n]), ".pdf", sep = ""))
  plotoutlierprobs(sc)
  dev.off()
  pdf(paste("./06. Results all cell types/", names(list_sc[n]),"/00. k_medioids clustheatmap_", names(list_sc[n]), ".pdf", sep = ""))
  clustheatmap(sc,final=TRUE,hmethod="single")
  dev.off()
  pdf(paste("./06. Results all cell types/", names(list_sc[n]),"/00. clusters_", names(list_sc[n]), ".pdf", sep = ""))
  plottsne((list_sc[[n]]),final=TRUE)
  dev.off()

  # Histograms of the merged files of the orinial conditions
  pdf(paste("./06. Results all cell types/", names(list_merged_files)[n],"/00. histogram__", ".pdf", sep = ""))
  hist(colSums(list_merged_files[[n]]), xlab = "Transcripts", ylab = "Cells", main = paste("Transcripts counts per cell", names(list_merged_files[n])), breaks = 1000, xlim = c(0, 20000))
  dev.off()
}

# A loop for the cell's origin
for(n in 1:length(list_sc)) {
  
  # Activate a specific sc object from list_sc
  sc <- list_sc[[n]]  
  # Create ggplot with Cell's origin: Prepare a dataframe to be used in ggplot
  tsnecord <- cbind(sc@tsne, CELLID= rownames(sc@tsne))
  tsnecord2  <- separate(data = tsnecord, col = CELLID, into = c("condition"), sep = "_first")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_second")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_third")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_fourth")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_fifth")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_sixth")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_seventh")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_eigth")
  tsnecord2  <- separate(data = tsnecord2, col = "condition", into = c("condition"), sep = "_nineth")
  #
  tsnecord2$condition <- gsub("Time_point_1_ctrl", "Control 1", tsnecord2$condition ) 
  tsnecord2$condition <-  gsub("Time_point_1", "Time point 1", tsnecord2$condition)
  tsnecord2$condition <-  gsub("Time_point_2", "Time point 2", tsnecord2$condition)
  tsnecord2$condition <- gsub("Time_point_3_ctrl", "Control 3", tsnecord2$condition ) 
  tsnecord2$condition <-  gsub("Time_point_3", "Time point 3", tsnecord2$condition)
  #
  tsnecord2$condition <- factor(tsnecord2$condition, levels = 
                                  c("Control 1", 
                                    "Control 3", 
                                    "Time point 1",
                                    "Time point 2",
                                    "Time point 3"))
  # create ggplot
  cells_origin <- ggplot(tsnecord2, aes(V1, V2, color = condition)) + 
    geom_point(alpha = 0.6) + 
    ggtitle("Cell's origin") +
    labs(x = "dim1", y="dim2") +
    labs(colour="Treatment") +
    theme_bw(base_size=20) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust=0.5)) 
  # save a ggplot to a pdf file 
  ggsave(paste("./06. Results all cell types/", names(list_sc[n]), "/01. Cells origin.pdf", sep =""), plot = last_plot(), device = "pdf", path =  ".",
         dpi = 300, limitsize = FALSE, scale = 1, width = NA, height = NA)
}

# clear the global environment
rm(list = ls())
