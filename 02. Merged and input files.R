### Marjolen Droog
### September 15th, 2018
### Merge single cell RNA transcript files derived from 384-wells plate into required dataframe(s)
### Place dataframes into the folder: ".02. Merged objects"

# Load required packages
library(data.table)
library(tidyverse)

#     Read input files:  Time point 1
Time_point_1_first <- read.table("./01. Transcript Counts/plate1.TranscriptCounts.tsv",sep="\t",header = TRUE, row.names = 1) 
Time_point_1_second <- read.table("./01. Transcript Counts/plate2.TranscriptCounts.tsv",sep="\t",header = TRUE, row.names = 1)
Time_point_1_third <- read.table("./01. Transcript Counts/plate3.TranscriptCounts.tsv",sep="\t",header = TRUE, row.names = 1)
Time_point_1_fourth <- read.table("./01. Transcript Counts/plate4.ReadCounts.tsv",sep="\t",header = TRUE, row.names = 1) 
Time_point_1_fifth <- read.table("./01. Transcript Counts/plate5.ReadCounts.tsv",sep="\t",header = TRUE, row.names = 1)
Time_point_1_sixth <- read.table("./01. Transcript Counts/plate6.ReadCounts.tsv",sep="\t",header = TRUE, row.names = 1)
Time_point_1_seventh <- read.table("./01. Transcript Counts/plate7.ReadCounts.tsv",sep="\t",header = TRUE, row.names = 1) 
Time_point_1_eigth <- read.table("./01. Transcript Counts/plate8.ReadCounts.tsv",sep="\t",header = TRUE, row.names = 1)
Time_point_1_nineth <- read.table("./01. Transcript Counts/plate9.ReadCounts.tsv",sep="\t",header = TRUE, row.names = 1)
#     Read input files:  Time point 2
Time_point_2_first <- read.table("./01. Transcript Counts/plate10.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_2_second <- read.table("./01. Transcript Counts/plate11.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_2_third <- read.table("./01. Transcript Counts/plate12.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_2_fourth <- read.table("./01. Transcript Counts/plate13.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_2_fifth <- read.table("./01. Transcript Counts/plate14.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_2_sixth <- read.table("./01. Transcript Counts/plate15.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_2_seventh <- read.table("./01. Transcript Counts/plate16.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
#     Read input files: Time point 3
Time_point_3_first <- read.table("./01. Transcript Counts/plate17.TranscriptCounts.tsv",sep="\t",header = TRUE, row.names = 1)
Time_point_3_second <- read.table("./01. Transcript Counts/plate18.TranscriptCounts.tsv",sep="\t",header = TRUE, row.names = 1)
Time_point_3_third <- read.table("./01. Transcript Counts/plate19.TranscriptCounts.tsv",sep="\t",header = TRUE, row.names = 1)
#     Read input files:  Time point 1 control
Time_point_1_ctrl_first <- read.table("./01. Transcript Counts/plate20.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_1_ctrl_second <- read.table("./01. Transcript Counts/plate21.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_1_ctrl_third <- read.table("./01. Transcript Counts/plate22.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_1_ctrl_fourth <- read.table("./01. Transcript Counts/plate23.ReadCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
#     Read input files:  Time point 3 control
Time_point_3_ctrl_first <- read.table("./01. Transcript Counts/plate24.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1) 
Time_point_3_ctrl_second <- read.table("./01. Transcript Counts/plate25.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_3_ctrl_third <- read.table("./01. Transcript Counts/plate26.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_3_ctrl_fourth <- read.table("./01. Transcript Counts/plate27.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_3_ctrl_fifth <- read.table("./01. Transcript Counts/plate28.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_3_ctrl_sixth <- read.table("./01. Transcript Counts/plate29.TranscriptCounts.tsv", sep = "\t", header = TRUE,  row.names = 1)
Time_point_3_ctrl_seventh <- read.table("./01. Transcript Counts/plate30.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_3_ctrl_eigth <- read.table("./01. Transcript Counts/plate31.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)
Time_point_3_ctrl_nineth <- read.table("./01. Transcript Counts/plate32.TranscriptCounts.tsv", sep = "\t", header = TRUE, row.names = 1)

###     Transform column names to match plate-position (384 wells plate contains rows A:P, and columns 1:24)   ###
plate_position <- paste(sort(rep((LETTERS)[1:16], 24)), 1:24, sep = "")
##      Tranform the columnnames with the vector a
#       Time point 1
colnames(Time_point_1_first) <- paste("Time_point_1_first_", plate_position , sep = "")
colnames(Time_point_1_second) <- paste("Time_point_1_second_", plate_position , sep = "")
colnames(Time_point_1_third) <- paste("Time_point_1_third_", plate_position , sep = "")
colnames(Time_point_1_fourth) <- paste("Time_point_1_fourth_", plate_position , sep = "")
colnames(Time_point_1_fifth) <- paste("Time_point_1_fifth_", plate_position , sep = "")
colnames(Time_point_1_sixth) <- paste("Time_point_1_sixth_", plate_position , sep = "")
colnames(Time_point_1_seventh) <- paste("Time_point_1_seventh_", plate_position , sep = "")
colnames(Time_point_1_eigth) <- paste("Time_point_1_eigth_", plate_position , sep = "")
colnames(Time_point_1_nineth) <- paste("Time_point_1_nineth_", plate_position , sep = "")
#       Time point 2
colnames(Time_point_2_first) <- paste("Time_point_2_first_", plate_position , sep = "")
colnames(Time_point_2_second) <- paste("Time_point_2_second_", plate_position , sep = "")
colnames(Time_point_2_third) <- paste("Time_point_2_third_", plate_position , sep = "")
colnames(Time_point_2_fourth) <- paste("Time_point_2_fourth_", plate_position , sep = "")
colnames(Time_point_2_fifth) <- paste("Time_point_2_fifth_", plate_position , sep = "")
colnames(Time_point_2_sixth) <- paste("Time_point_2_sixth_", plate_position , sep = "")
colnames(Time_point_2_seventh) <- paste("Time_point_2_seventh_", plate_position , sep = "")
#       Time point 3
colnames(Time_point_3_first) <- paste("Time_point_3_first_", plate_position , sep = "")
colnames(Time_point_3_second) <- paste("Time_point_3_second_", plate_position , sep = "")
colnames(Time_point_3_third) <- paste("Time_point_3_third_", plate_position , sep = "")
#       Time point 1 control
colnames(Time_point_1_ctrl_first) <- paste("Time_point_1_ctrl_first_", plate_position , sep = "")
colnames(Time_point_1_ctrl_second) <- paste("Time_point_1_ctrl_second_", plate_position , sep = "")
colnames(Time_point_1_ctrl_third) <- paste("Time_point_1_ctrl_third_", plate_position , sep = "")
colnames(Time_point_1_ctrl_fourth) <- paste("Time_point_1_ctrl_fourth_", plate_position , sep = "")
#       Time point 3 control
colnames(Time_point_3_ctrl_first) <- paste("Time_point_3_ctrl_first_", plate_position , sep = "")
colnames(Time_point_3_ctrl_second) <- paste("Time_point_3_ctrl_second_", plate_position , sep = "")
colnames(Time_point_3_ctrl_third) <- paste("Time_point_3_ctrl_third_", plate_position , sep = "")
colnames(Time_point_3_ctrl_fourth) <- paste("Time_point_3_ctrl_fourth_", plate_position , sep = "")
colnames(Time_point_3_ctrl_fifth) <- paste("Time_point_3_ctrl_fifth_", plate_position , sep = "")
colnames(Time_point_3_ctrl_sixth) <- paste("Time_point_3_ctrl_sixth_", plate_position , sep = "")
colnames(Time_point_3_ctrl_seventh) <- paste("Time_point_3_ctrl_seventh_", plate_position , sep = "")
colnames(Time_point_3_ctrl_eigth) <- paste("Time_point_3_ctrl_eigth_", plate_position , sep = "")
colnames(Time_point_3_ctrl_nineth) <- paste("Time_point_3_ctrl_nineth_", plate_position , sep = "")
# Remove 'plate_position' from the global environment
rm(plate_position) 

##   For some data an additional space was added by the facility after every gene-name: "gene_name " 
##   Transform rownames of these files 
#     Time point 2
row.names(Time_point_2_first) <- gsub(" ", "", row.names(Time_point_2_first))
row.names(Time_point_2_second) <- gsub(" ", "", row.names(Time_point_2_second))
row.names(Time_point_2_third) <- gsub(" ", "", row.names(Time_point_2_third))
row.names(Time_point_2_fourth) <- gsub(" ", "", row.names(Time_point_2_fourth))
row.names(Time_point_2_fifth) <- gsub(" ", "", row.names(Time_point_2_fifth))
row.names(Time_point_2_sixth) <- gsub(" ", "", row.names(Time_point_2_sixth))
row.names(Time_point_2_seventh) <- gsub(" ", "", row.names(Time_point_2_seventh))
#     Time point 3 control
row.names(Time_point_3_ctrl_first) <- gsub(" ", "", row.names(Time_point_3_ctrl_first))
row.names(Time_point_3_ctrl_second) <- gsub(" ", "", row.names(Time_point_3_ctrl_second))
row.names(Time_point_3_ctrl_third) <- gsub(" ", "", row.names(Time_point_3_ctrl_third))
row.names(Time_point_3_ctrl_fourth) <- gsub(" ", "", row.names(Time_point_3_ctrl_fourth))
row.names(Time_point_3_ctrl_fifth) <- gsub(" ", "", row.names(Time_point_3_ctrl_fifth))
row.names(Time_point_3_ctrl_sixth) <- gsub(" ", "", row.names(Time_point_3_ctrl_sixth))
row.names(Time_point_3_ctrl_seventh) <- gsub(" ", "", row.names(Time_point_3_ctrl_seventh))
row.names(Time_point_3_ctrl_eigth) <- gsub(" ", "", row.names(Time_point_3_ctrl_eigth))
row.names(Time_point_3_ctrl_nineth) <- gsub(" ", "", row.names(Time_point_3_ctrl_nineth))

## Transform rownames into a column so you can select it in the merge-pipe (next step)
#   Time point 1 
Time_point_1_first <- setDT(Time_point_1_first, keep.rownames = TRUE)
Time_point_1_second <- setDT(Time_point_1_second, keep.rownames = TRUE)
Time_point_1_third <- setDT(Time_point_1_third, keep.rownames = TRUE)
Time_point_1_fourth <- setDT(Time_point_1_fourth, keep.rownames = TRUE)
Time_point_1_fifth <- setDT(Time_point_1_fifth, keep.rownames = TRUE)
Time_point_1_sixth <- setDT(Time_point_1_sixth, keep.rownames = TRUE)
Time_point_1_seventh <- setDT(Time_point_1_seventh, keep.rownames = TRUE)
Time_point_1_eigth <- setDT(Time_point_1_eigth, keep.rownames = TRUE)
Time_point_1_nineth <- setDT(Time_point_1_nineth, keep.rownames = TRUE)
#   Time point 2
Time_point_2_first <- setDT(Time_point_2_first, keep.rownames = TRUE)
Time_point_2_second <- setDT(Time_point_2_second, keep.rownames = TRUE)
Time_point_2_third <- setDT(Time_point_2_third , keep.rownames = TRUE)
Time_point_2_fourth <-setDT(Time_point_2_fourth, keep.rownames = TRUE)
Time_point_2_fifth <- setDT(Time_point_2_fifth, keep.rownames = TRUE)
Time_point_2_sixth <- setDT(Time_point_2_sixth, keep.rownames = TRUE)
Time_point_2_seventh <- setDT(Time_point_2_seventh, keep.rownames = TRUE)
#   Time point 3
Time_point_3_first <- setDT(Time_point_3_first, keep.rownames = TRUE)
Time_point_3_second <- setDT(Time_point_3_second, keep.rownames = TRUE)
Time_point_3_third <- setDT(Time_point_3_third, keep.rownames = TRUE)
#   Time point 1 control
Time_point_1_ctrl_first <- setDT(Time_point_1_ctrl_first, keep.rownames = TRUE)
Time_point_1_ctrl_second <- setDT(Time_point_1_ctrl_second, keep.rownames = TRUE)
Time_point_1_ctrl_third <- setDT(Time_point_1_ctrl_third, keep.rownames = TRUE)
Time_point_1_ctrl_fourth <- setDT(Time_point_1_ctrl_fourth, keep.rownames = TRUE)
#   Time point 3 control
Time_point_3_ctrl_first <- setDT(Time_point_3_ctrl_first, keep.rownames = TRUE)
Time_point_3_ctrl_second <- setDT(Time_point_3_ctrl_second, keep.rownames = TRUE)
Time_point_3_ctrl_third <- setDT(Time_point_3_ctrl_third, keep.rownames = TRUE)
Time_point_3_ctrl_fourth <- setDT(Time_point_3_ctrl_fourth, keep.rownames = TRUE)
Time_point_3_ctrl_fifth <- setDT(Time_point_3_ctrl_fifth, keep.rownames = TRUE)
Time_point_3_ctrl_sixth <- setDT(Time_point_3_ctrl_sixth, keep.rownames = TRUE)
Time_point_3_ctrl_seventh <- setDT(Time_point_3_ctrl_seventh, keep.rownames = TRUE)
Time_point_3_ctrl_eigth <- setDT(Time_point_3_ctrl_eigth, keep.rownames = TRUE)
Time_point_3_ctrl_nineth <- setDT(Time_point_3_ctrl_nineth, keep.rownames = TRUE)

## Merge-pipe: To merge plates into 1 dataframe  ####
#   Time point 1 control
merged_ctrl1 <- full_join(Time_point_1_ctrl_first, Time_point_1_ctrl_second) %>%
  full_join(Time_point_1_ctrl_third) %>%
  full_join(Time_point_1_ctrl_fourth) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   Time point 3 control
merged_ctrl3 <- full_join(Time_point_3_ctrl_first, Time_point_3_ctrl_second) %>%
  full_join(Time_point_3_ctrl_third) %>%
  full_join(Time_point_3_ctrl_fourth) %>%
  full_join(Time_point_3_ctrl_fifth) %>%
  full_join(Time_point_3_ctrl_sixth) %>%
  full_join(Time_point_3_ctrl_seventh) %>%
  full_join(Time_point_3_ctrl_eigth) %>%
  full_join(Time_point_3_ctrl_nineth) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)  

#   Time point 1
merged_timep1 <- full_join(Time_point_1_first, Time_point_1_second) %>%
  full_join(Time_point_1_third) %>%
  full_join(Time_point_1_fourth) %>%
  full_join(Time_point_1_fifth) %>%
  full_join(Time_point_1_sixth) %>%  
  full_join(Time_point_1_seventh) %>%
  full_join(Time_point_1_eigth) %>%
  full_join(Time_point_1_nineth) %>% 
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   Time point 2
merged_timep2 <- full_join(Time_point_2_first, Time_point_2_second) %>%
  full_join(Time_point_2_third) %>%
  full_join(Time_point_2_fourth) %>%
  full_join(Time_point_2_fifth) %>%
  full_join(Time_point_2_sixth) %>%
  full_join(Time_point_2_seventh) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   Time point 3
merged_timep3 <- full_join(Time_point_3_first, Time_point_3_second) %>%
  full_join(Time_point_3_third) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   all conditions
merged_all <- full_join(Time_point_1_ctrl_first, Time_point_1_ctrl_second) %>%
  full_join(Time_point_1_ctrl_third) %>%
  full_join(Time_point_1_ctrl_fourth) %>%
  full_join(Time_point_3_ctrl_first) %>% 
  full_join(Time_point_3_ctrl_second) %>%
  full_join(Time_point_3_ctrl_third) %>%
  full_join(Time_point_3_ctrl_fourth) %>%
  full_join(Time_point_3_ctrl_fifth) %>%
  full_join(Time_point_3_ctrl_sixth) %>%
  full_join(Time_point_3_ctrl_seventh) %>%
  full_join(Time_point_3_ctrl_eigth) %>%
  full_join(Time_point_3_ctrl_nineth) %>%
  full_join(Time_point_1_first) %>%
  full_join(Time_point_1_second) %>%
  full_join(Time_point_1_third) %>%
  full_join(Time_point_1_fourth) %>%
  full_join(Time_point_1_fifth) %>%
  full_join(Time_point_1_sixth) %>%  
  full_join(Time_point_1_seventh) %>%
  full_join(Time_point_1_eigth) %>%
  full_join(Time_point_1_nineth) %>%  
  full_join(Time_point_2_first) %>%
  full_join(Time_point_2_second) %>%
  full_join(Time_point_2_third) %>%
  full_join(Time_point_2_fourth) %>%
  full_join(Time_point_2_fifth) %>%
  full_join(Time_point_2_sixth) %>%
  full_join(Time_point_2_seventh) %>%
  full_join(Time_point_3_first) %>%
  full_join(Time_point_3_second) %>%
  full_join(Time_point_3_third) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   Time point 1 and Time point 1 control
merged_1 <- full_join(Time_point_1_ctrl_first, Time_point_1_ctrl_second) %>%
  full_join(Time_point_1_ctrl_third) %>%
  full_join(Time_point_1_ctrl_fourth) %>%
  full_join(Time_point_1_first) %>%
  full_join(Time_point_1_second) %>%
  full_join(Time_point_1_third) %>%
  full_join(Time_point_1_fourth) %>%
  full_join(Time_point_1_fifth) %>%
  full_join(Time_point_1_sixth) %>%  
  full_join(Time_point_1_seventh) %>%
  full_join(Time_point_1_eigth) %>%
  full_join(Time_point_1_nineth) %>%  
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   Time point 3 and Time point 3 control
merged_14 <- full_join(Time_point_3_ctrl_first, Time_point_3_ctrl_second) %>%
  full_join(Time_point_3_ctrl_third) %>%
  full_join(Time_point_3_ctrl_fourth) %>%
  full_join(Time_point_3_ctrl_fifth) %>%
  full_join(Time_point_3_ctrl_sixth) %>%
  full_join(Time_point_3_ctrl_seventh) %>%
  full_join(Time_point_3_ctrl_eigth) %>%
  full_join(Time_point_3_ctrl_nineth) %>%
  full_join(Time_point_3_first) %>%
  full_join(Time_point_3_second) %>%
  full_join(Time_point_3_third) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   all time points 
merged_all_time_points <- full_join(Time_point_1_first, Time_point_1_second) %>%
  full_join(Time_point_1_third) %>%
  full_join(Time_point_1_fourth) %>%
  full_join(Time_point_1_fifth) %>%
  full_join(Time_point_1_sixth) %>%  
  full_join(Time_point_1_seventh) %>%
  full_join(Time_point_1_eigth) %>%
  full_join(Time_point_1_nineth) %>%  
  full_join(Time_point_2_first) %>%
  full_join(Time_point_2_second) %>%
  full_join(Time_point_2_third) %>%
  full_join(Time_point_2_fourth) %>%
  full_join(Time_point_2_fifth) %>%
  full_join(Time_point_2_sixth) %>%
  full_join(Time_point_2_seventh) %>%
  full_join(Time_point_3_first) %>%
  full_join(Time_point_3_second) %>%
  full_join(Time_point_3_third) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

#   Time point 1 control and Time point 3 control
merged_ctrl1_ctrl3 <- full_join(Time_point_1_ctrl_first, Time_point_1_ctrl_second) %>%
  full_join(Time_point_1_ctrl_third) %>%
  full_join(Time_point_1_ctrl_fourth) %>%
  full_join(Time_point_3_ctrl_first) %>% 
  full_join(Time_point_3_ctrl_second) %>%
  full_join(Time_point_3_ctrl_third) %>%
  full_join(Time_point_3_ctrl_fourth) %>%
  full_join(Time_point_3_ctrl_fifth) %>%
  full_join(Time_point_3_ctrl_sixth) %>%
  full_join(Time_point_3_ctrl_seventh) %>%
  full_join(Time_point_3_ctrl_eigth) %>%
  full_join(Time_point_3_ctrl_nineth) %>%
  column_to_rownames("rn") %>%
  replace(is.na(.), 0)

### write the merged files and the input files into a folder ###
list_merged <- list(merged_ctrl1 = merged_ctrl1, 
                    merged_ctrl3 = merged_ctrl3, 
                    merged_timep1 = merged_timep1, 
                    merged_timep2 = merged_timep2, 
                    merged_timep3 = merged_timep3, 
                    merged_1 = merged_1,
                    merged_3 = merged_3, 
                    merged_all = merged_all, 
                    merged_all_time_points = merged_all_time_points, 
                    merged_ctrl1_ctrl3 = merged_ctrl1_ctrl3)
# create a folder to store merged files into
if (!dir.exists(file.path("./02. Merged objects"))){
  dir.create(file.path("./02. Merged objects"))
}
# for loop writes merged files into abovementioned folder
for(n in 1:length(list_merged)){
  # write an rds file for each merged file
  write_rds(list_merged[[n]],
            paste(".02. Merged objects/",names(list_merged[n]), ".rds", sep = ""))
}

# Clean the global environment
rm(list = ls())
