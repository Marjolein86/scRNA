### Marjolein Droog
### September 15th, 2018
### create barplots for cell types with all conditions
### Write the files into the folder: "./06. Results all cell types/sc_all/

## Load required packages
source("07. All cells Assign clusters.R")
library(extrafont) 

# create lists for each condition per cell type 
CT1 <- list(cells_CT1_ctrl1 = cells_CT1_ctrl1, 
           cells_CT1_timep1 = cells_CT1_timep1, 
           cells_CT1_timep2 = cells_CT1_timep2, 
           cells_CT1_ctrl3 = cells_CT1_ctrl3, 
           cells_CT1_timep3 = cells_CT1_timep3)
CT2 <- list(cells_CT2_ctrl1 = cells_CT2_ctrl1, 
           cells_CT2_timep1 = cells_CT2_timep1, 
           cells_CT2_timep2 = cells_CT2_timep2, 
           cells_CT2_ctrl3 = cells_CT2_ctrl3,
           cells_CT2_timep3 = cells_CT2_timep3)
CT3 <- list(cells_CT3_ctrl1 = cells_CT3_ctrl1, 
           cells_CT3_timep1 = cells_CT3_timep1, 
           cells_CT3_timep2 = cells_CT3_timep2, 
           cells_CT3_ctrl3 = cells_CT3_ctrl3, 
           cells_CT3_timep3 = cells_CT3_timep3)
CT4 <- list(cells_CT4_ctrl1 = cells_CT4_ctrl1, 
           cells_CT4_timep1 = cells_CT4_timep1, 
           cells_CT4_timep2 = cells_CT4_timep2, 
           cells_CT4_ctrl3 = cells_CT4_ctrl3, 
           cells_CT4_timep3 = cells_CT4_timep3)
CT5 <- list(cells_CT5_ctrl1 = cells_CT5_ctrl1, 
           cells_CT5_timep1 = cells_CT5_timep1, 
           cells_CT5_timep2 = cells_CT5_timep2, 
           cells_CT5_ctrl3 = cells_CT5_ctrl3, 
           cells_CT5_timep3 = cells_CT5_timep3)
CT6 <- list(cells_CT6_ctrl1 = cells_CT6_ctrl1, 
           cells_CT6_timep1 = cells_CT6_timep1, 
           cells_CT6_timep2 = cells_CT6_timep2, 
           cells_CT6_ctrl3 = cells_CT6_ctrl3, 
           cells_CT6_timep3 = cells_CT6_timep3)
# combine all lists for a loop
list_cells <- list(CT1 = CT1, CT2 = CT2, CT3 = CT3, CT4 = CT4, CT5 = CT5, CT6 = CT6)

# Obtain absolute cell count per cell type and condition
for(i in 1:length(list_cells)){
  Celltype_size <- c(length(list_cells[[i]][[1]]), # ctrl1
                     length(list_cells[[i]][[2]]), # timep1
                     length(list_cells[[i]][[3]]), # timep2
                     length(list_cells[[i]][[4]]), # ctrl3
                     length(list_cells[[i]][[5]])) # timep3
  x <- data.frame(condition = names(list_cells[[i]]), cell_nr = Celltype_size) # add condition names
  # assign values to the global enviornment
  assign(x = paste("cells_abs_", names(list_cells[i]), sep = ""),
         value = x,
         envir = .GlobalEnv)
}

for(i in 1:length(list_cells)) {
  list_cells_abs <- list(cells_abs_CT1, cells_abs_CT2, cells_abs_CT3, cells_abs_CT4, cells_abs_CT5, cells_abs_CT6)
  # normalized relative cell count 
  population_size <- c(length(rownames(sc_ctrl1@tsne)), 
                       length(rownames(sc_timep1@tsne)), 
                       length(rownames(sc_timep2@tsne)), 
                       length(rownames(sc_ctrl3@tsne)), 
                       length(rownames(sc_timep3@tsne)))
  normalize <- rep(length(rownames(sc_ctrl1@tsne)), length(list_cells[[1]])) 
  # normalize for cell size of the population
  y <- normalize/population_size*list_cells_abs[[i]][,2] # normalize (divide by one condition --> ctrl1)
  y2 <- y/sum(y)*100 # Turn normalized numbers into percentages (sum is 100%)
  x <- data.frame(condition = names(list_cells[[i]]), cell_nr = y2) # add condition names
  # assign values to the global environment
  assign(x = paste("cells_norm", names(list_cells[i]), sep = ""),
         value = x,
         envir = .GlobalEnv)
}

# loop for barplots of each condition per celltype (normalized)
for(i in 1:length(list_cells)) {

  list_cells_norm <- list("CT1" = cells_normCT1, 
                          "CT2" = cells_normCT2, 
                          "CT3" = cells_normCT3, 
                          "CT4 cells" = cells_normCT4, 
                          "CT5" = cells_normCT5, 
                          "CT6" = cells_normCT6)
  
  pdf(paste("./06. Results all cell types/sc_all/celltype_bar_", names(list_cells[i]), ".pdf", sep = ""))
  # create barplot
  par(mar=c(4, 10, 8, 3))
  barplot(list_cells_norm[[i]][,2], 
          main = names(list_cells_norm[i]), 
          ylab = "Distribution of cells (%)", 
          ylim = c(0, max(list_cells_norm[[i]][,2] + 10)),
          names.arg = c("ctrl1", "timep1", "timep2", "ctrl3", "timep3"), 
          cex.names = 1.25, 
          cex.axis = 1, 
          cex.lab = 2, 
          cex.main = 2,
          mgp = c(5, 2, 1),
          col = "black")
  dev.off()
}

rm(list = ls())
