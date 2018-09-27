### Marjolen Droog
### September 15th, 2018
### Assign clusters according to their cell type: # - Cell type 1 (CT1)
                                                  # - Cell type 2 (CT2)
                                                  # - Cell type 3 (CT3)
                                                  # - Cell type 4 (CT4)
                                                  # - Cell type 5 (CT5)
                                                  # - Cell type 6 (CT6)
                                                  # - Cell type 7 (CT7)
### To assign clusters according to their cell type you need to look into 
###  the differentially upregulated genes that can be viewed in the excel files
###   obtained from the R file "06. All cells pdf.R"

# load in the sc objCT4ts
sc_ctrl1 <- readRDS("./05. Sc objCT4ts/sc_ctrl1.rds")
sc_ctrl3 <- readRDS("./05. Sc objCT4ts/sc_ctrl3.rds")
sc_timep1 <- readRDS("./05. Sc objCT4ts/sc_timep1.rds")
sc_timep2 <- readRDS("./05. Sc objCT4ts/sc_timep2.rds")
sc_timep3 <- readRDS("./05. Sc objCT4ts/sc_timep3.rds")

### Time point 1 control
CT1_ctrl1 <- c(1,3,5,9,10,13)
CT2_ctrl1 <- c(11, 12)
CT3_ctrl1 <- c(6)
CT4_ctrl1 <- c(4,7)
CT5_ctrl1 <- NULL
CT6_ctrl1 <- NULL
CT7_ctrl1 <- 14
# GRK4 cluster is cluster 2
# No idea what cluster 8 is

list_ctrl1 <- list(CT1_ctrl1 = CT1_ctrl1, 
                  CT2_ctrl1 = CT2_ctrl1, 
                  CT3_ctrl1 = CT3_ctrl1, 
                  CT4_ctrl1 = CT4_ctrl1,
                  CT5_ctrl1 = CT5_ctrl1,
                  CT6_ctrl1 = CT6_ctrl1)
# Assign which cells (plate-well position) belong to which cell-type
for (i in 1:length(list_ctrl1)){
  cell_clusters <- data.frame(CELLID=names(sc_ctrl1@cpart),cluster=sc_ctrl1@cpart)
  assign(x = paste("cells_", names(list_ctrl1)[i], sep = ""),
         value = rownames(cell_clusters[cell_clusters[,2]%in%list_ctrl1[[i]],]),
         envir = .GlobalEnv)
         }

### Time point 3 control
CT1_ctrl3 <- c(1, 2, 3, 7, 9)
CT2_ctrl3 <- c(4)
CT3_ctrl3 <- c(10)
CT4_ctrl3 <- c(6, 8)
CT5_ctrl3 <- NULL
CT6_ctrl3 <- NULL
CT7_ctrl3 <- 11
# GRK4 cluster is cluster 5

list_ctrl3 <- list(CT1_ctrl3 = CT1_ctrl3, 
                  CT2_ctrl3 = CT2_ctrl3, 
                  CT3_ctrl3 = CT3_ctrl3, 
                  CT4_ctrl3 = CT4_ctrl3,
                  CT5_ctrl3 = CT5_ctrl3,
                  CT6_ctrl3 = CT6_ctrl3)
# Assign which cells (plate-well position) belong to which cell-type
for (i in 1:length(list_ctrl3)){
  cell_clusters <- data.frame(CELLID=names(sc_ctrl3@cpart),cluster=sc_ctrl3@cpart)
  assign(x = paste("cells_", names(list_ctrl3)[i], sep = ""),
         value = rownames(cell_clusters[cell_clusters[,2]%in%list_ctrl3[[i]],]),
         envir = .GlobalEnv)
}

### Time point 1
CT1_timep1 <- c(6,10,13)
CT2_timep1 <- c(4, 12)
CT3_timep1 <- c(8)
CT4_timep1 <- c(7,9) # cluster 7 might be due to Malat1 artifact
CT5_timep1 <- c(1,2)
CT6_timep1 <- 5
CT7_timep1 <- 11
# GRK4 cluster is cluster 3

list_timep1 <- list(CT1_timep1 = CT1_timep1, 
                  CT2_timep1 = CT2_timep1, 
                  CT3_timep1 = CT3_timep1, 
                  CT4_timep1 = CT4_timep1,
                  CT5_timep1 = CT5_timep1,
                  CT6_timep1 = CT6_timep1)

# Assign which cells (plate-well position) belong to which cell-type
for (i in 1:length(list_timep1)){
  cell_clusters <- data.frame(CELLID=names(sc_timep1@cpart),cluster=sc_timep1@cpart)
  assign(x = paste("cells_", names(list_timep1)[i], sep = ""),
         value = rownames(cell_clusters[cell_clusters[,2]%in%list_timep1[[i]],]),
         envir = .GlobalEnv)
}

# Time point 2
CT1_timep2 <- c(4,10,12,13,14,16)
CT2_timep2 <- c(6,9,15)
CT3_timep2 <- c(1,2,8)
CT4_timep2 <- c(5)
CT5_timep2 <- NULL
CT6_timep2 <- 11
CT7_timep2 <- NULL
# cluster 3 is between macrophage and myocyte
# GRK4 cluster is cluster 7
# cluster is cluster 3 is hard to say

list_timep2 <- list(CT1_timep2 = CT1_timep2, 
                   CT2_timep2 = CT2_timep2, 
                   CT3_timep2 = CT3_timep2, 
                   CT4_timep2 = CT4_timep2,
                   CT5_timep2 = CT5_timep2,
                   CT6_timep2 = CT6_timep2)
# Assign which cells (plate-well position) belong to which cell-type
for (i in 1:length(list_timep2)){
  cell_clusters <- data.frame(CELLID=names(sc_timep2@cpart),cluster=sc_timep2@cpart)
  assign(x = paste("cells_", names(list_timep2)[i], sep = ""),
         value = rownames(cell_clusters[cell_clusters[,2]%in%list_timep2[[i]],]),
         envir = .GlobalEnv)
}

 # Time point 3
CT1_timep3 <- c(1,3,5,6,8,9,10) # cluster 3 is debatable
CT2_timep3 <- c(11) 
CT3_timep3 <- 7
CT4_timep3 <- 2
CT5_timep3 <- NULL
CT6_timep3 <- NULL
CT7_timep3 <-NULL
# GRK4 cluster is cluster 4

list_timep3 <- list(CT1_timep3 = CT1_timep3, 
                   CT2_timep3 = CT2_timep3, 
                   CT3_timep3 = CT3_timep3, 
                   CT4_timep3 = CT4_timep3,
                   CT5_timep3 = CT5_timep3,
                   CT6_timep3 = CT6_timep3)
# Assign which cells (plate-well position) belong to which cell-type
for (i in 1:length(list_timep3)){
  cell_clusters <- data.frame(CELLID=names(sc_timep3@cpart),cluster=sc_timep3@cpart)
  assign(x = paste("cells_", names(list_timep3)[i], sep = ""),
         value = rownames(cell_clusters[cell_clusters[,2]%in%list_timep3[[i]],]),
         envir = .GlobalEnv)
}