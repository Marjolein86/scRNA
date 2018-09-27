### Marjolein Droog
### Septebmer 15th, 2018
### Create Venn diagrams to illustrate overlapping genes in different conditions

## Load required packages
library(Vennerable)
library(graph)
library(RBGL)

Venn_CM_up<- compute.Venn(Venn(SetNames= c("Time point 1", "Time point 3"), Weight=c(0,1058,115,31)))
gp <- VennThemes(Venn_CM_up)
gp[["Face"]][["11"]]$fill <-  "white"
gp[["Face"]][["01"]]$fill <-  "white"
gp[["Face"]][["10"]]$fill <-  "white"
plot(Venn_CM_up, gp = gp)

Venn_CM_d<- compute.Venn(Venn(SetNames= c("Time point 1", "Time point 3"), Weight=c(0,133,16,1)))
gp <- VennThemes(Venn_CM_d)
gp[["Face"]][["11"]]$fill <-  "white"
gp[["Face"]][["01"]]$fill <-  "white"
gp[["Face"]][["10"]]$fill <-  "white"
plot(Venn_CM_d, gp = gp)

# clear the global environment
rm(list = ls())