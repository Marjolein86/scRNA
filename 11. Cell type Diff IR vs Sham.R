### Marjolen Droog
### September 15th, 2018
### Analyze differentially regulated genes in cell type 1 of time-points to corresponding controls
### Write the objects and files into the folder: "./07. Results cell type 1"

## Load required packages
require(openxlsx)
library(tidyverse)
library("ggrepel")
source("01. RaceID3_StemID2.R")
source("07. All cells Assign clusters.R")

## Function: differential expression in clusters between cells that have high expression of a gene vs a low expression of gene
    # x = data frame with unique reads (raw) of all cells
    # g1 = cell names group 1
    # g2 = cell names group 2
diffexpnb.cellsvscells.DESeq2 <- function(x,g1,g2){
  x <- x[,c(g1,g2)]
  x <- x[!grepl("ERCC",row.names(x)),]
  des <- data.frame(row.names=colnames(x),
                    condition= c(rep("con1",length(g1)),rep("con2",length(g2))))
  dds <- DESeqDataSetFromMatrix(
    countData=round(x,0),
    colData=des,
    design= ~ condition)
  dds <- DESeq(dds)
  output <- as.data.frame(results(dds))
  write.table(output, paste("diff_expr_",substitute(cell_type),".xls"),row.names=TRUE, col.names=NA, sep = "\t", dec = ",")
  return(output)
}

# Read input: merged file
merged_all = readRDS("./02. Merged objects/merged_all.rds") 

# Read input: sc objects
sc_ctrl1 <- readRDS("./05. Sc objects/sc_ctrl1.rds")
sc_timep1 <- readRDS("./05. Sc objects/sc_timep1.rds")
sc_ctrl3 <- readRDS("./05. Sc objects/sc_ctrl3.rds")
sc_timep3 <- readRDS("./05. Sc objects/sc_timep3.rds")

## Differentially regulated genes Time point 1 vs. corresponding Control
diff_CT1_1_1 <- diffexpnb.cellsvscells.DESeq2(merged_all, cells_CT1__ctrl1, cells_CT1__timep1)

# write an rds file and an excel file for each differentially regulated condition
write_rds(diff_CT1_1_1, "./07. Results cell type 1/sc_CT1__1/diff_CT1_1_1.rds")
write.xlsx(diff_CT1_1_1, 
           file = "./07. Results cell type 1/sc_CT1__1/diff_CT1__1_1.xlsx", 
           header = TRUE, 
           row.names = TRUE)

# Ggplot2 differential gene expression ctrl1ham vs timep1
# create a dataframe
df_1 <- cbind(gene = rownames(diff_CT1_1_1), diff_CT1_1_1)
df_1[is.na(df_1)] <- 0.1
df_1 <- mutate(df_1, sig=ifelse(df_1$padj<0.05 | df_1$padj<0.05, "FDR<0.05", "Not Sig"))  
df_1 <- separate(data = df_1, col = "gene", into = c("gene","Chr"), sep = "__")
# Create a plot
ggplot(df_1, aes(x = log(baseMean), y = log2FoldChange, col = sig)) +
  geom_point() +
  xlim(-10,8) +
  ylim(-5,5) +
  scale_color_manual(values=c("purple", "grey")) +
  ggtitle("Differential gene expression between timep1 and ctrl1ham") +
  theme_bw(base_size = 16) +
  geom_text_repel(
    data = filter(df_1, sig == "FDR<0.05" & log2FoldChange < -2 & log(baseMean) > 2 | sig == "FDR<0.05" & log(baseMean)> 2.5 & log(baseMean)<5.5 & log2FoldChange > 1.5 ), 
    aes(label = gene),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5)) 
# save a ggplot to a pdf file 
ggsave("./07. Results cell type 1/sc_CT1__1/diff_CT1_1_1.pdf", plot = last_plot(), device = "pdf", path = ".",
       dpi = 300, limitsize = TRUE)

## Differentially regulated genes Time point 3 vs. corresponding Control
diffCT1_3_3 <- diffexpnb.cellsvscells.DESeq2(merged_all , cells_CT1__ctrl3, cells_CT1__timep3)write_rds(diffCT1_3_3, "./07. Results cell type 1/diffCT1_3_3.rds")
write.xlsx(diffCT1_3_3, 
           file = paste("./07. Results cell type 1/sc_CT1__3/", "diffCT1_3_3.xlsx", sep = ""), 
           header = TRUE, 
           row.names = TRUE)

# Ggplot2 differential gene expression ctrl1ham vs timep1
# create a dataframe
df_3 <- cbind(gene = rownames(diffCT1_3_3), diffCT1_3_3)
df_3[is.na(df_3)] <- 0.1
df_3 <- mutate(df_3, sig=ifelse(df_3$padj<0.05 | df_3$padj<0.05, "FDR<0.05", "Not Sig"))  
df_3 <- separate(data = df_3, col = "gene", into = c("gene","Chr"), sep = "__")
# Create a plot
ggplot(df_3, aes(x = log(baseMean), y = log2FoldChange, col = sig)) +
  geom_point() +
  xlim(-10,8) +
  ylim(-5,5) +
  scale_color_manual(values=c("purple", "grey")) +
  ggtitle("Differential gene expression between timep3 and ctrl3ham") +
  theme_bw(base_size = 16) +
  geom_text_repel(
    data = filter(df_3, sig == "FDR<0.05" & log2FoldChange > 1.5 |sig == "FDR<0.05" & log2FoldChange < -1.2 | sig == "FDR<0.05" & log(baseMean)>2.9 & log(baseMean)<5.5 & log2FoldChange > 1.5 |sig == "FDR<0.05" & log(baseMean)>3.5 & log(baseMean)<5.5 & log2FoldChange > 0.2), 
    aes(label = gene),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5)) 
# save a ggplot to a pdf file --> If you don't save it like this, the grid will not be proportioned right 
ggsave("./07. Results cell type 1/sc_CT1__3/diffCT1_3_3.pdf", plot = last_plot(), device = "pdf", path = ".",
       dpi = 300, limitsize = TRUE)

# clear global environment
rm(list = ls())
