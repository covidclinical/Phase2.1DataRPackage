# Phase2DataRPackage
This repository contains R utility functions for Phase 2 Data Pre-Processing for the 4CE Consortium.

# Installation

To install this package in R:

``` R
devtools::install_github("https://github.com/ChuanHong/Phase2DataRPackage", subdir="FourCePhase2Data", upgrade=FALSE)
```

# 4CE Phase 2 Data Pre-Processing Overview

The Phase 2 Data Pre-Processing is recommended for all sites before running the 4CE consortium Phase 2 project. The Phase 2 Data Pre-Processing consists of two parts: quality control and data cleaning. 


## Quality Control for Phase 2 Data

The R library (`FourCePhase2Data`) in this repository contains functions that conduct quality control for the Phase 2 Data: 
1. **QC for Phase 2 patient-level data**. We will check if the summary statistics obtained from Phase 2 patient-level data is consistent with the aggreated results generated for Phase1.1.
2. **QC for Phase 2 summary statistics**. We will check the following critera: 
+ Demographics:  
  + if there is missing demographic groups (e.g., missing age group); 
  + if there is negative patient numbres or counts; 
  + if there is number of all patients less than the number of severe patients. 
+ Medications and Diagnoses:
  + if there is any code not belong to the list of medclass or ICD10; 
  + if there is negative patient numbers or counts; 
  + if the number of all patients is greater than the number of severe patients.
+ Labs: 
  + if there is any labs not belong to the list of loinc codes;
  + if there is negative patients nubmers; 
  + if there is negative measures in the original scale.
+ Cross over comparison: 
  + if the number of patients in DailyCounts, ClincalCourse, and Demographic are consistent; 
  + if the number of severe patients in DailyCounts, ClinialCourse, and Demographic are consistent;
  + if in any of Medications, Diagnoses or Labs, the number of patients with the code is greater than number of all patients. 
+ Lab units: check if the lab measures are consistently outside the confidence range.
3. **Generate QC report**
A QC report will be generated in word format. 


## Data Cleaning for Phase 2 Data

The R library (`FourCePhase2Data`) in this repository also contains functions for data clean, which generates data in specific formats to fit into different analyses:  
1. **Longitudinal data for Labs**: days since admission 
2. **Longitudinal data for Medications**: before and since admission 
3. **Longitudinal data for Diagnoses**: before and since admission 
4. **Baseline covariates**: demographic variables, lab measures at day 0, medications and diagnoses before admission. 
5. **Time varying covariates**: to be added
6. **Event time data**: for each event outcome (severe, deceased, and severe AND deceased), we derive observed event time, and the indicator of obsering the event. 

# Get Started

1. Run the Docker container and launch the localhost Rstudio. The details can be found in (https://github.com/covidclinical/Phase2.0_Docker_Analysis).


2. Install and load the R package:

``` R
devtools::install_github("https://github.com/covidclinical/Phase2DataRPackage", subdir="FourCePhase2Data", upgrade=FALSE)
library(FourCePhase2Data)
```
3. Set the working, input and output directories. In my example, I set the working directory to "/4ceData/RDevelopment", and specified "Input/" and "Output/". The users can specify different directories as needed. The input.dir should be where you save the Phase 2 patient level data and Phase 2 summary data, and the output dir will be the destination of the QC report and cleaned data. 

``` R
setwd("/4ceData/RDevelopment")
input.dir = "Input/" 
output.dir = "Output/" 
```

4. Read in the Phase 2 patient level data, and Phase 2 summary data. 
``` R
phase2.cc=read.csv(paste0(input.dir, "Phase2LocalPatientClinicalCourse.csv"))
phase2.po=read.csv(paste0(input.dir, "Phase2LocalPatientObservations.csv"))
phase2.ps=read.csv(paste0(input.dir, "Phase2LocalPatientSummary.csv"))
phase1.lab=read.csv(paste0(input.dir, "Phase2Labs.csv"))
phase1.med=read.csv(paste0(input.dir, "Phase2Medications.csv"))
phase1.diag=read.csv(paste0(input.dir, "Phase2Diagnoses.csv"))
phase1.dem=read.csv(paste0(input.dir, "Phase2Demographics.csv"))
phase1.dc=read.csv(paste0(input.dir, "Phase2DailyCounts.csv"))
phase1.cc=read.csv(paste0(input.dir, "Phase2ClinicalCourse.csv"))

```

5. Conduct QC. 
``` R
Phase2QC_Tab_Labs=runQC_tab_lab(phase2.cc, phase2.po, phase1.lab, output.dir)
Phase2QC_Tab_Medications=runQC_tab_med(phase2.cc, phase2.po, phase1.med, output.dir)
Phase2QC_Tab_Diagnoses=runQC_tab_diag(phase2.cc, phase2.po, phase1.diag, output.dir)
Phase2QC_Tab_Demographic=runQC_tab_dem(phase2.ps, phase2.po, phase1.dem, output.dir)
Phase2QC_Tab_ClinicalCourse=runQC_tab_cc(phase2.cc, phase1.cc, output.dir)
runQC_report(phase1.dc,phase1.cc, phase1.dem,phase1.diag, phase1.lab, phase1.med, output.dir,site.nm="MGB")
```
**If there is any issue identified in Step 5, please fix the issue first before going to Step 6.**

6. Data Cleaning.
``` R
Phase2Data_Labs_Longitudinal=runData_Labs_Longitudinal(phase2.po, output.dir)
Phase2Data_Medications_Longitudinal=runData_Medications_Longitudinal(phase2.po, output.dir)
Phase2Data_Diagnoses_Longitudinal=runData_Diagnoses_Longitudinal(phase2.po, output.dir)
Phase2Data_Covariates_Baseline=runData_Covariates_Baseline(phase2.cc, phase2.po, phase2.ps, output.dir)
Phase2Data_EventTime=runData_EventTime(phase2.cc, output.dir)
```

7. If all the above steps have worked correctly, you should be able to see the following files in the output directory:

+ Phase2QC_Tab_ClinicalCourse.csv
+ Phase2QC_Tab_Demographics.csv
+ Phase2QC_Tab_Diagnosees.csv
+ Phase2QC_Tab_Labs.csv
+ Phase2QC_Tab_Medications.csv
+ Phase2QC_Report.doc

+ Phase2Data_Labs_Longitudinal.csv
+ Phase2Data_Medications_Longitudinal.csv
+ Phase2Data_Diagnoses_Longitudinal.csv
+ Phase2Data_Covariates_Baseline.csv
+ Phase2Data_EventTime.csv




