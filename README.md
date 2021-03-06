## ROS Data Processing
Scripts to fit calibration curves and calculate the DTT loss rates for the ROS UV-Vs analysis 

**Project:** ECHO LUR

**Date created:** December 3, 2018

**Author:** Sheena Martenies

**Contact:** Sheena.Martenies@colostate.edu

#### Project Overview
This project takes the .xlsx workbooks generated by the HPLC software to:

- fit a calibration curve
- estimate the concentrations of DTT in each sample
- fit a linear model to estimate the loss rate of DTT in each sample

#### Folder Directory
The following folders are needed to make sure the scripts run smoothly

- Results
    - This folder will contain all of the processed ROS results
    - Results are in the form of a .csv file containing sample names, DTT concentrations, loss rates, and some analytical notes (e.g., calibration curve)
- Data/Calibration
    - This directory holds the raw calibration curve data as well as the linear regression results and some diagnostic outputs
- Data/Raw_Data
    - This directory holds the raw spreadsheets from the HPLC
- Figs
    - This data holds all of the figures generated by the ROS_Calibration_Curve.R script and the ROS_Data_Processing script

#### Script Dictionary
The following scripts are found in the R directory

- 01_ROS_Calibration_Curve.R
    - This sript fits a linear regression model to the calibration data
    - Each time a new calibration curve is generated, you'll need to replace the .xslx file in the "Data/Calibration" director and update the file name in the script (Line 41)
    - Once the file name is updated, run the script to generate the new calibration curve
    - You can choose to run this script before each data processing step if you'd like, but just make sure the right calibration file is used
    
- 02_ROS_Data_Processing.R
    - This script reads in the data from the HPLC and estimates DTT concentrations in each sample. 
    - The script also fits a linear model to each of the samples to estimate the DTT loss rate over time
    - The raw data are stored in the "Data/Raw_Data" directory
    - Before each run, make sure you update the "raw_name" object (Line 58) and ensure that the curve_name (Line 43) indicates the right calibration curve for the data run
    - The script will process samples and field/solution blanks and report results in a single .csv file

