# Paspalum
REU Summer 2022 paspalum project

This project consisted of analyzing 22,000 images from a salt tolerance experiment done on Paspalum vaginatum at the Bellwether Phenotyping Facility located at the Donald Danforth Plant Science Center. 

The images taken by the phenotyper were analyzed utilizing PlantCV, which uses a computer vision framework to differentiate between plant and background pixels. PlantCV creates a csv file with trait data such as plant area, height, solidity, yellowing, etc. which we can then use to analyze the result of our experiment.

Using R, we wrote this set of scripts that will take the information from PlantCV and perform statistical analysis to assess the salt tolerance of Paspalum. 

These scripts were meant to be run in the following order: 

(1) 1.Process_Results_TV.R

(2) 2.Water_Loss_TV.R

(3) 3.Combine_Water_Loss_with_Zoom_Data_TV.R

(4) 4.Exploratory_Linegraphs_&_Groundtruth_TV.R

(5) 5.Plotting_&_Making_Tables_for_CI_Ttest_&_Pvals_TV.R

(6) 6.Exploring_Ecotype_Differences_Across_Genotypes_TV.R
