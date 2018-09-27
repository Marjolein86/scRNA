### Marjolein Droog
### September 15th, 2018
### create barplot and scatterplot for GO analysis 
### Write the objects and files into the folder: "./08. GO analysis/"

## Load required packages
library(tidyverse)
library(extrafont)

# create a folder to store result-folders in 
if (!dir.exists(file.path("./08. GO analysis"))){
  dir.create(file.path("./08. GO analysis"))
}

# genes upregulated Time point 1 vs. corresponding Control
# read in the GO term file (obtained with DAVID_6.8)
Time_point_1_up_GO <- read.csv("Time_point_1_up_GO.tsv", header = TRUE, "\t")

# create bar plot
ggplot(Time_point_1_up_GO, aes(x=reorder(pathway.description, -false.discovery.rate), y = -log10(false.discovery.rate))) +
  geom_bar(stat="identity") + 
  labs(y = "-log10(P-value)") +
  coord_flip() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(size=20)) +
  scale_y_continuous(position = "top", breaks = NULL)  
#dev.off()
# save a ggplot to a pdf file --> If you don't save it like this, the grid's proportions will be incorrect 
ggsave("./08. GO analysis/01. KEGG 1", plot = last_plot(), device = "pdf", path =   "/Users/temp/Documents",
       dpi = 300, limitsize = FALSE, scale = 1, width = NA, height = NA)

# create scatter plot
ggplot(Time_point_1_up_GO, aes(x=Count, y = -log10(Benjamini), fill = Function)) +
  scale_fill_manual(values=c("lightcoral","dodgerblue", "springgreen3","lightgoldenrodyellow")) +
  geom_point(size = 4, shape = 21) +
  geom_hline(aes(yintercept = -log10(0.05)), linetype="dashed") +
  geom_text_repel(
    data = filter(Time_point_1_up_GO, (Benjamini < 1.1e-17 | Count  > 50 )), 
    aes(label = Term),
    size = 5,
    box.padding = unit(0.5, "lines"),
    point.padding = unit(0.3, "lines"))

# genes upregulated Time point 3 vs. corresponding Control
# read in the GO term file (obtained with DAVID_6.8)
Time_point_3_up_GO <- read.csv("Time_point_3_up_GO.txt", header = TRUE, "\t")
Time_point_3_up_GO_sig <- Time_point_3_up_GO[Time_point_3_up_GO$Benjamini < 0.05,]
# create barplot
ggplot(Time_point_3_up_GO_sig, aes(x=reorder(Term, -Benjamini), y = -log10(Benjamini), fill = factor(Function), col = "black")) +
  scale_fill_manual(values=c("lightcoral","lightgoldenrodyellow")) +
  scale_color_manual(values= "black") +
  geom_bar(stat="identity") + 
  labs(y = "-log10(P-value)") +
  coord_flip() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(family="Arial", size=12)) +
  scale_y_continuous(position = "top")  +
  geom_hline(aes(yintercept = -log10(0.05)), linetype="dashed") +
  geom_text(aes(label=Count),position="stack",vjust=0.5, hjust = -3, size = 3)
  
# create scatter plot
ggplot(Time_point_3_up_GO, aes(x=Count, y = -log10(Benjamini), fill = Function)) +
  scale_fill_manual(values=c("lightcoral", "lightgoldenrodyellow")) +
  geom_point(size = 4, shape = 21) +
  geom_hline(aes(yintercept = -log10(0.05)), linetype="dashed") +
  geom_text_repel(
    data = filter(Time_point_3_up_GO, (Benjamini < 5.0e-03 | Count > 11 )), 
    aes(label = Term),
    size = 5,
    box.padding = unit(0.5, "lines"),
    point.padding = unit(0.3, "lines"))

# clear global environment
rm(list = ls())