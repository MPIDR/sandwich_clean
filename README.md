# Scripts for reproducible analysis 

Code for reproducible results for paper Alburez-Gutierrez, D., Mason, C., and Zagheni, E. (2021). *The 'Sandwich Generation' Revisited: Global Demographic Drivers of Care Time Demands*. Population and Development Review. DOI:[10.1111/padr.12436](https://doi.org/10.1111/padr.12436).

Use in combination with the Harvard Dataverse repository that includes the SOCSIM simulation outputs for each country:

Alburez-Gutierrez, Diego, 2021, "Replication Data for: The “Sandwich Generation” Revisited: Global Demographic Drivers of Care Time Demands", Harvard Dataverse. DOI:[10.7910/DVN/SSZL6U](https://doi.org/10.7910/DVN/SSZL6U)

**Please note** that if you want to re-run the analysis from scratch (i.e., starting from the simulation output files in the Harvard Dataverse), some data management is required.
Follow these steps:

1. Download this repository to your computer
1. Download all the data from DOI:[10.7910/DVN/SSZL6U](https://doi.org/10.7910/DVN/SSZL6U)
1. For the analysis, run the scripts in the RStudio projects provided. 
1. In the R scripts, adjust the parameters that locate the simulation files in your computer. Unfortunately, this might vary depending on your operating system and to location of your scripts and simulation output files. In theory, you only need to adjust the variable `Cave` in all scripts, but something else might need adjusting as well. 
1. Run the scripts sequentially in the order suggested by the naming of the files. 
1. The scripts are parallelized using up to 25 cores - it is highly recommended that you use a High-Performance Cluster to run the scripts! They will not work on a regular laptop. 

Please get in touch [@d_alburez](https://twitter.com/d_alburez) if you find any issues. 