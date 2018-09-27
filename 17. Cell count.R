### Marjolein Droog
### September 15th, 2018
### Count cell size per cell type
### Write files into the folder: "./06. Results all cell types/sc_all/nr_data.xlsx"

## Load required packages
source("07. All cells Assign clusters.R")

nr_CT1 <- c(length(cells_CT1_ctrl1),
           length(cells_CT1_timep1),
           length(cells_CT1_timep2),
           length(cells_CT1_ctrl3),
           length(cells_CT1_timep3))
nr_CT2 <- c(length(cells_CT2_ctrl1),
           length(cells_CT2_timep1),
           length(cells_CT2_timep2),
           length(cells_CT2_ctrl3),
           length(cells_CT2_timep3))
nr_CT3 <- c(length(cells_CT3_ctrl1),
           length(cells_CT3_timep1),
           length(cells_CT3_timep2),
           length(cells_CT3_ctrl3),
           length(cells_CT3_timep3))
nr_CT4 <- c(length(cells_CT4_ctrl1),
           length(cells_CT4_timep1),
           length(cells_CT4_timep2),
           length(cells_CT4_ctrl3),
           length(cells_CT4_timep3))
nr_CT5 <- c(length(cells_CT5_ctrl1),
           length(cells_CT5_timep1),
           length(cells_CT5_timep2),
           length(cells_CT5_ctrl3),
           length(cells_CT5_timep3))
nr_CT6 <- c(length(cells_CT6_ctrl1),
           length(cells_CT6_timep1),
           length(cells_CT6_timep2),
           length(cells_CT6_ctrl3),
           length(cells_CT6_timep3))

nr_data <- data.frame(nr_CT1, nr_CT2, nr_CT3, nr_CT4, nr_CT5, nr_CT6)
rownames(nr_data) <- c("ctrl1", "timep1", "timep2", "ctrl3", "timep3")
colnames(nr_data) <- c("CT1", "CT2", "CT3", "CT4", "CT5", "CT6")
colSums <- colSums(nr_data)

nr_data <- rbind(nr_data, 
                 colSums)
rowSums <- rowSums(nr_data)
nr_data <- cbind(nr_data, 
                 rowSums)

write.xlsx(nr_data, 
           "./06. Results all cell types/sc_all/nr_data.xlsx", 
           header = FALSE, 
           row.names = TRUE) 

# clear the global environmnet
rm(list = ls())
