### Marjolen Droog
### September 15th, 2018
### Use RACEid3 package (by Dominic Grun) to inititate SCseq and filter for wells that contained cells
## List is used in subsequently used for loop so that for each condition:
    # input is read
    # SCseq (Single Cell sequence) object is initiated
    # data is filtered
    # objects are written into the folder: "./04. Sc_input_merged objects/"

# Load required packages and functions
source("01. RaceID3_StemID2.R")

# read input
list_input <- list(input_merged_ctrl1 = readRDS("./03. Input_merged objects/input_merged_ctrl1.rds") , 
                   input_merged_ctrl3 = readRDS("./03. Input_merged objects/input_merged_ctrl3.rds"), 
                   input_merged_timep1 = readRDS("./03. Input_merged objects/input_merged_timep1.rds"), 
                   input_merged_timep2 = readRDS("./03. Input_merged objects/input_merged_timep2.rds"), 
                   input_merged_timep3 = readRDS("./03. Input_merged objects/input_merged_timep3.rds"), 
                   input_merged_1 = readRDS("./03. Input_merged objects/input_merged_1.rds"),
                   input_merged_3 = readRDS("./03. Input_merged objects/input_merged_3.rds"), 
                   input_merged_all = readRDS("./03. Input_merged objects/input_merged_all.rds"), 
                   input_merged_all_timep = readRDS("./03. Input_merged objects/input_merged_all_timep.rds"), 
                   input_merged_ctrl1_ctrl3 = readRDS("./03. Input_merged objects/input_merged_ctrl1_ctrl3.rds") )
# create a folder to store sc files 
if (!dir.exists(file.path("./04. Sc_input_merged objects"))){
  dir.create(file.path("./04. Sc_input_merged objects"))
}
# create a for loop to initate and store Single Cell object 
for(n in max(length(list_input))){
  for(n in 1:length(list_input)){
    
    # Read in the file and initiate SCseq and filter from the RACEid package
    merged_file <- readRDS(paste("./03. Filtered merged Files/", names(list_input[n]), ".rds", sep = ""))
    sc_merged_file <- SCseq(merged_file)
    sc_merged_file_filtered <- filterdata(sc_merged_file, mintotal=1000, minexpr=5, minnumber=1, maxexpr=Inf, downsample=T, dsn=1, rseed=17000)
    
    # make a filtered SCseq file into the environment
    assign(x = paste("sc_", names(list_input[n]), sep = ""),
           value = sc_merged_file_filtered ,
           envir = .GlobalEnv)
    
    # write an rds file for each SC file
    write_rds(sc_merged_file_filtered,
              paste("./04. Sc_input_merged objects/","sc_", names(list_input[n]), ".rds", sep = ""))
  }
  # clear the global environment
  rm(list = ls())
}
