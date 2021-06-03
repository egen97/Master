# Code and appendix for, *From Pub Fights to International Conflict: How values and mass opinion affect war""

### Eric. G. E. Nilsen

This repository contains all R-code used in the research to my master thesis, "From Pub Fights to International Conflict: How values and mass opinion affect war". The data used is *not* included due to the providers different copyright-limitations. All data is is cited in the thesis, and easily available online from the original providers. In addition this repository contains the thesis' appendix as a markdown file. 
Any questions can be directed to the author at: [mailto](mailto:egnilsen@student.sv.uio.no)

## Structure 

The folder contains two main types of scripts. Firstly, most data-sources have their own script used for data-wrangling as to create a common structure between the data. If the user should want to replicate the study these must first be run on each dataset, and the new data stored to be used later. These data will then be used in the script "SetteSammenData" to create the final complete dataset. This data will contain *several* variables not used in the following analysis. The imputations are performed in the "imputering_2" script. Several of the scripts contain a number after the name. These are scripts which have gone through major changes, and I have found it usefull to keep the old versions. The script with the highest number are, logically, the latest script and that which has been used in the results presented. Having run the imputation the resulting data are those used in all further analysis at the country-year level. 

Having created the data, there are several different scripts performing the different forms of data analysis:

The main models on the country-year level is run in the script "Analyse_2.R", however most tables from these model are printed in the "Tables" script. This last script as well includes the function used to create the AIC for the different models. For the models utilising the post-materialist variables the models and tables can be found in "Inglehardt.R" and "Inglehardt_Tables.R"


On the individual-level most models are created in the "senncdanal_2.R" scripts. This script as well contains the code used to create the different LaTex tables. 

In addition to these scripts, several plots have been created in specific scripts made for this purpose. These scripts will be named based on the plot created. 



