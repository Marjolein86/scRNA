### Marjolen Droog
### September 15th, 2018
### Perform k-medioids clustering, compute t-SNE map, detect outliers, and write single cell objects into a folder
      ## For each condition (Time point 1 and corresponding control, Time point 2, Time point 3 and corresponding control):
            # read input
            # specify parameters for k-medioid clustering
            # use function (final.sc) below to compute t-sne map, detect outliers 
            # write objects into the folder: "./05. Sc objects/"

## Load required packages 
source("01. RaceID3_StemID2.R")

## FUNCTION: computes t-SNE map,  detects outliers, and writes them as an rds object into a predefined folder
final.sc <- function(x){
  y <- comptsne(x, rseed=15555) # using RACEid3 compute t-SNE map
  z <- findoutliers(y, outminc=5,outlg=4,probthr=1e-15,thr=2**-(1:40),outdistquant=.95) # using RACEid3 detect outliers
  write_rds(z,
            paste("./05. Sc objects/",deparse(substitute(x)) , ".rds", sep = ""))   # write results into a folder
 }

# create a folder to store input merged files 
if (!dir.exists(file.path("./05. Sc objects"))){
  dir.create(file.path("./05. Sc objects"))
}
  
# Time point 1 control
sc_ctrl1 <- readRDS("./04. Sc_input_merged objects/sc_input_merged_ctrl1.rds")
sc_ctrl1 <- clustexp(sc_ctrl1,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_ctrl1)
rm(sc_ctrl1)
# Time point 3 control
sc_ctrl3 <- readRDS("./04. Sc_input_merged objects/sc_input_merged_ctrl3.rds")
sc_ctrl3 <- clustexp(sc_ctrl3,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_ctrl3)
rm(sc_ctrl3)
# Time point 1
sc_timep1 <- readRDS("./04. Sc_input_merged objects/sc_input_merged_timep1.rds")
sc_timep1 <- clustexp(sc_timep1,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=13,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_timep1)
rm(sc_timep1)
# Time point 2
sc_timep2 <- readRDS("./04. Sc_input_merged objects/sc_input_merged_timep2.rds")
sc_timep2 <- clustexp(sc_timep2,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_timep2)
rm(sc_timep2)
# Time point 3
sc_timep3 <- readRDS("./04. Sc_input_merged objects/sc_input_merged_timep3.rds")
sc_timep3 <- clustexp(sc_timep3,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_timep3)
rm(sc_timep3)
# Time point 1 and corresponding control
sc_1dp <- readRDS("./04. Sc_input_merged objects/sc_input_merged_1.rds")
sc_1dp <- clustexp(sc_1dp,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_1dp)
rm(sc_1dp)
# Time point 3 and corresponding control
sc_14dp <- readRDS("./04. Sc_input_merged objects/sc_input_merged_3.rds")
sc_14dp <- clustexp(sc_14dp,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_14dp)
rm(sc_14dp)
# All cells
sc_all <- readRDS("./04. Sc_input_merged objects/sc_input_merged_all.rds")
sc_all <- clustexp(sc_all,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_all)
rm(sc_all)
# All Time point cells
sc_all_IR <- readRDS("./04. Sc_input_merged objects/sc_input_merged_all_timep.rds")
sc_all_IR <- clustexp(sc_all_IR,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_all_IR)
rm(sc_all_IR)
# Time point 1 control and Time point 3 control
sc_ctrl1_ctrl3 <- readRDS("./04. Sc_input_merged objects/sc_input_merged_ctrl1_ctrl3.rds")
sc_ctrl1_ctrl3 <- clustexp(sc_ctrl1_ctrl3,clustnr=30,bootnr=50,metric="pearson",do.gap=FALSE,sat=TRUE,SE.method="Tibs2001SEmax",SE.factor=.25,B.gap=50,cln=0,rseed=17000,FUNcluster="kmedoids")
final.sc(sc_ctrl1_ctrl3)
rm(sc_ctrl1_ctrl3)

# clear the global environment
rm(list = ls())
