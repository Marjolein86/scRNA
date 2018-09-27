### Marjolen Droog
### September 15th, 2018
### Select Cell type 1 on which to perform k-medoids clustering, compute t-SNE map, detect outliers and redefine clusters
### Store objects in the folder ./05. Sc objects

## Load required packages and functions
source("01. RaceID3_StemID2.R")
source("07. All cells Assign clusters.R")

## read input
merged_all = readRDS("./02. Merged objects/merged_all.rds") 

# CT1: Cell type 1
# all Time point cell_type_1 minus outlier cells
cell_type_1_all_time_points <- c(cells_CT1_timep1, cells_CT1_timep2, cells_CT1_timep3)
cells <- c("Time_point_1_third_D3", "Time_point_1_ctrl_fourth_O6")
cell_type_1_all_time_points <- setdiff(cell_type_1_all_time_points, cells)
cell_type_1_all_time_points<- merged_all[,cell_type_1_all_time_points]
input_merged_CT1_all_time_points <- cell_type_1_all_time_points[!grepl("ERCC", row.names(cell_type_1_all_time_points))&!grepl("__chrM", row.names(cell_type_1_all_time_points))&!grepl("Rn45s", row.names(cell_type_1_all_time_points)),]
sc_CT1_al_time_points <- SCseq(input_merged_CT1_all_time_points)
sc_CT1_al_time_points <- filterdata(sc_CT1_al_time_points, mintotal=1000, minexpr=5, minnumber=1, maxexpr=Inf, downsample=T, dsn=1, rseed=17000) 
sc_CT1_al_time_points <- clustexp(sc_CT1_al_time_points,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
sc_CT1_al_time_points <- comptsne(sc_CT1_al_time_points,rseed=15555)
sc_CT1_al_time_points <- findoutliers(sc_CT1_al_time_points, outminc=5,outlg=4,probthr=1e-15,thr=2**-(1:40),outdistquant=.95)
sc_CT1_al_time_points <- write_rds(sc_CT1_al_time_points, "./05. Sc objects/sc_CT1_all_time_points.rds")

# Time point 1 control and Time point 1 cell_type_1 minus two outlier cells
cell_type_1_1 <- c(cells_CT1_ctrl1, cells_CT1_timep1)
cells <- c("Time_point_1_third_D3", "Time_point_1_ctrl_fourth_O6")
cell_type_1_1 <- setdiff(cell_type_1_1, cells)
cell_type_1_1<- merged_all[,cell_type_1_1]
input_merged_CT1_1 <- cell_type_1_1[!grepl("ERCC", row.names(cell_type_1_1))&!grepl("__chrM", row.names(cell_type_1_1))&!grepl("Rn45s", row.names(cell_type_1_all_time_points)),]
sc_CT1_1 <- SCseq(input_merged_CT1_1)
sc_CT1_1 <- filterdata(sc_CT1_1, mintotal=1000, minexpr=5, minnumber=1, maxexpr=Inf, downsample=T, dsn=1, rseed=17000) 
sc_CT1_1 <- clustexp(sc_CT1_1,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
sc_CT1_1 <- comptsne(sc_CT1_1,rseed=15555)
sc_CT1_1 <- findoutliers(sc_CT1_1, outminc=5,outlg=4,probthr=1e-15,thr=2**-(1:40),outdistquant=.95)
sc_CT1_1 <- write_rds(sc_CT1_1, "./05. Sc objects/sc_CT1_1.rds")

# Time point 3 control and Time point 3
cell_type_1_3 <- c(cells_CT1_ctrl3, cells_CT1_timep3)
cell_type_1_3<- merged_all[,cell_type_1_3]
input_merged_CT1_3 <- cell_type_1_3[!grepl("ERCC", row.names(cell_type_1_3))&!grepl("__chrM", row.names(cell_type_1_3))&!grepl("Rn45s", row.names(cell_type_1_all_time_points)),]
sc_CT1_3 <- SCseq(input_merged_CT1_3)
sc_CT1_3 <- filterdata(sc_CT1_3, mintotal=1000, minexpr=5, minnumber=1, maxexpr=Inf, downsample=T, dsn=1, rseed=17000) 
sc_CT1_3 <- clustexp(sc_CT1_3,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=13,rseed=17000,FUNcluster="kmedoids")
sc_CT1_3 <- comptsne(sc_CT1_3,rseed=15555)
sc_CT1_3 <- findoutliers(sc_CT1_3, outminc=5,outlg=4,probthr=1e-5,thr=2**-(1:40),outdistquant=.95)
sc_CT1_3 <- write_rds(sc_CT1_1, "./05. Sc objects/sc_CT1_3.rds")

# clear the global environment
rm(list = ls())