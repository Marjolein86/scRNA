### Marjolen Droog
### September 15th, 2018
### Filter out genes from the dataframe for reads caused by miss-alignments or for mitochondrial genes, and spiked genes
### Write objects into the folder: "./03. Filtered merged Files"

# Read input
list_merged <- list(merged_ctrl1 = readRDS("./02. Merged Files/merged_ctrl1.rds") ,
                    merged_ctrl3 = readRDS("./02. Merged Files/merged_ctrl3.rds"), 
                    merged_timep1 = readRDS("./02. Merged Files/merged_timep1.rds"), 
                    merged_timep2 = readRDS("./02. Merged Files/merged_timep2.rds"), 
                    merged_timep3 = readRDS("./02. Merged Files/merged_timep3.rds"), 
                    merged_1 = readRDS("./02. Merged Files/merged_1.rds"),
                    merged_3 = readRDS("./02. Merged Files/merged_3.rds"), 
                    merged_all = readRDS("./02. Merged Files/merged_all.rds"), 
                    merged_all_timep = readRDS("./02. Merged Files/merged_all_timep.rds"), 
                    merged_ctrl1_ctrl3 = readRDS("./02. Merged Files/merged_ctrl1_ctrl3.rds") )
# create a folder to store input merged files 
if (!dir.exists(file.path("./03. Filtered merged Files"))){
  dir.create(file.path("./03. Filtered merged Files"))
}
# create a for loop that writes rds files into above mentioned folder
for(n in max(length(list_merged))){
  for(n in 1:length(list_merged)){
    y <- list_merged[[n]]
    # write an rds file for each input that filters out mitochondrial genes and RN45s
    write_rds(y[!grepl("ERCC", row.names(y))&!grepl("__chrM", row.names(y))&!grepl("Rn45s", row.names(y)),],
              paste("./03. Filtered merged Files/", "input_", names(list_merged[n]), ".rds", sep = ""))
    rm(y)
  }
  # remove merged files from the global environment
rm(list_merged)
rm(n)
}