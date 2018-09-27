### Marjolein Droog
### September 15th, 2018
### To create barplots per cell type for just the Timepoints (timep1, timep2, timep3) 
### Write the files into the folder: "./06. Results all cell types/sc_all_timep/"

## Load required packages
source("07. All cells Assign clusters.R")
library(extrafont)

# create lists for each condition per cell type 
CT1 <- list(cells_CT1_timep1 = cells_CT1_timep1, 
           cells_CT1_timep2 = cells_CT1_timep2, 
           cells_CT1_timep3 = cells_CT1_timep3)
CT2 <- list(cells_CT2_timep1 = cells_CT2_timep1, 
           cells_CT2_timep2 = cells_CT2_timep2, 
           cells_CT2_timep3 = cells_CT2_timep3)
CT3 <- list(cells_CT3_timep1 = cells_CT3_timep1, 
           cells_CT3_timep2 = cells_CT3_timep2, 
           cells_CT3_timep3 = cells_CT3_timep3)
CT4 <- list(cells_CT4_timep1 = cells_CT4_timep1, 
           cells_CT4_timep2 = cells_CT4_timep2, 
           cells_CT4_timep3 = cells_CT4_timep3)
CT5 <- list(cells_CT5_timep1 = cells_CT5_timep1, 
           cells_CT5_timep2 = cells_CT5_timep2, 
           cells_CT5_timep3 = cells_CT5_timep3)
CT6 <-  list(cells_CT6_timep1 = cells_CT6_timep1, 
            cells_CT6_timep2 = cells_CT6_timep2, 
            cells_CT6_timep3 = cells_CT6_timep3)
# combine all lists for a loop
list_cells <- list(CT1 = CT1, CT2 = CT2, CT3 = CT3, CT4 = CT4, CT5 = CT5, CT6 = CT6)

# Obtain absolute cell count per cell type and condition
for(i in 1:length(list_cells)){
  Celltype_size <- c(length(list_cells[[i]][[1]]), 
                     length(list_cells[[i]][[2]]), 
                     length(list_cells[[i]][[3]])) 
  x <- data.frame(condition = names(list_cells[[i]]), cell_nr = Celltype_size) # add condition names
  assign(x = paste("cells_abs_", names(list_cells[i]), sep = ""),
         value = x,
         envir = .GlobalEnv)
}

# a loop to normalize the absolute cell counts
for(i in 1:length(list_cells)) {
  list_cells_abs <- list(cells_abs_CT1, cells_abs_CT2, cells_abs_CT3, cells_abs_CT4, cells_abs_CT5, cells_abs_CT6)
  
  # normalized relative cell count 
  population_size <- c(length(rownames(sc_timep1@tsne)), 
                       length(rownames(sc_timep2@tsne)), 
                       length(rownames(sc_timep3@tsne)))
  normalize <- rep(length(rownames(sc_ctrl1@tsne)), length(list_cells[[1]])) 
  
  y <- normalize/population_size*list_cells_abs[[i]][,2] # normalize (divide by one condition --> ctrl1)
  y2 <- y/sum(y)*100 # Turn normalized numbers into percentages (sum is 100%)
  x <- data.frame(condition = names(list_cells[[i]]), cell_nr = y2) # add condition names
  
  assign(x = paste("cells_norm", names(list_cells[i]), sep = ""),
         value = x,
         envir = .GlobalEnv)
}

# loop for barplots of each condition per celltype (normalized)
for(i in 1:length(list_cells)) {
list_cells_norm <- list("CT1" = cells_normCT1, 
                          "CT2" = cells_normCT2, 
                          "CT3" = cells_normCT3, 
                          "CT4" = cells_normCT4, 
                          "CT5" = cells_normCT5,
                          "CT6" = cells_normCT5)
  pdf(paste("./06. Results all cell types/sc_all_timep/celltype_bar_timep_", names(list_cells[i]), ".pdf", sep = ""))
  par(mar=c(4, 10, 8, 3))
  barplot(list_cells_norm[[i]][,2], 
          main = names(list_cells_norm[i]), 
          ylab = "Distribution of cells Time points (%)", 
          ylim = c(0, max(list_cells_norm[[i]][,2] + 10)),
          names.arg = c("timep1", "timep2", "timep3"), 
          cex.names = 1.25, 
          cex.axis = 1, 
          cex.lab = 2, 
          cex.main = 2,
          mgp = c(5, 2, 1),
          col = "black")
  dev.off()
}

# clear the global environment
rm(list = ls())
