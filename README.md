# scRNA
The pipeline requires the following:

1.  
In the working directory there should be the R-project file, a csv file with a list of genes you wish to analyze, and a              folder named ‘01. Transcript Counts’. In this folder you will place one folder per condition with .tsv files of transcript counts.

2.  
The required packages can be found in the script '00. Packages for pipeline".  

3.  
In order for the pipeline to work, you will also need to source RacdeID3_StemID2 which is created by Dominik Grun:
https://github.com/dgrun/RaceID3_StemID2

4.
For the file '01.' numerous treatments can be added. If you want t-sne maps of specific genes, add a csv file named Genes2.csv to the same folder as the R-project file. In column A



File '02.' will continue upon the analysis from '01.' and is only usuable if two conditions are being compared (for example, a treatment versus a control). The script in '02.' will create similar plots and files as '01.'. In addition, it will produce a plot and an excel file with the foldchange of the treatment compared to the control. 

