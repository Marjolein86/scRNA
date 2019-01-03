# scRNA
These scripts to analyze single cell RNA-seq (following CEL-Seq2 protocol) data require the following:


1.  
The required packages to run these scripts can be found in the script '00. Required packages".  


2.  
In order for the pipeline to work, you will also need to source RacdeID3_StemID2, created by Dominik Grun:
https://github.com/dgrun/RaceID3_StemID2


3.  
In the working directory there should be:
- the R-project file 

- a csv file named 'Genes.csv':
  - column A: A list of genes you wish to analyze
  - column B: corresponding marker types of genes in column A
  
- A folder named ‘01. Transcript Counts’: 
  - In this folder you will place one folder per condition 
    - Each condition-folder contains the corresponding '.tsv' files (transcript counts).

- The script '01. RNA-seq single cells' 
  - numerous treatments can be analyzed using this script
  - produces an 'output' folder in your working directory with:
    - RACE plots
      - tsne map of the data
      - k-medioids cluster heatmap
      - Jaccard plot
      - Outlier plot
      - Saturation plot
    - tsne plots of genes specified in Genes.csv
    - tnse plot of cell, color-coded according to their origin/treatment
    - excel file with differentially regulated genes per cluster
    - excel file with number of cells per cluster
    - excel file with the filter parameters
    - merged file that merged all transcriptcount data together
    - input file is similar to merged file but removed mitochondrial genes and Rn45s
    - sc file that is used for RACEid analysis

4. 
- The script in '02. Differential gene expression':
  - This script compares two conditions (for example control versus treatment)
  - Continues upon the analysis from '01. RNA-seq single cells' and is only usuable if two conditions were used 
    - After the script '01. RNA-seq single cells' is done, it has produced a file named 'clusters.xlsx'.
      - Analyze these clusters to identify cell-types
      - Create a .csv file names 'clusters.csv' and place it in the working directory 
        - 'clusters.csv' should contain: 
          - in column A: per row a celltype abbreviation
          - specify which cluster belongs to which cell-type: 
            - Each column after A should contain only one cluster-number per column
        
  - The script '02. Differential gene expression' creates an 'Ouput2' folder:
    - in this folder there will be other folders created, named after each cell type provided in 'clusters.csv'. 
      - Per cell type - folder: 
        - similar plots and files as '01. RNA-seq single cells'. 
        - In  addition, this script will produce:
          - a plot of differentially regulated genes (basemeans vs. fold change)
          - an excel file with the foldchange of the treatment compared to the control 

