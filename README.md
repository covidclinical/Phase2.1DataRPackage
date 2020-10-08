# Phase2.1DataRPackage
This repository contains R functions for Phase 2.1 Data Pre-Processing for the 4CE Consortium.

# Installation

To install this package in R:

``` R
devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", upgrade=FALSE)
```

# 4CE Phase 2.1 Data Pre-Processing Overview

The Phase 2.1 Data Pre-Processing consists of two parts: Quality Control (QC) and Data Pivot. The QC is recommended for ALL sites; the Data Pivot is optional. 


## Quality Control 

The R library (`FourCePhase2.1Data`) in this repository contains functions that conduct quality control for the Phase1.1 and Phase2.1. Each site should pass the QC for both Phase1.1 and Phase2.1 before conducting Phase 2.1 project. 

1. **QC for Phase1.1**. We will check the following critera: 
+ Demographics:  
  + if there is missing demographic groups (e.g., missing age group); 
  + if there is negative patient numbres or counts; 
  + if there is number of all patients less than the number of severe patients. 
  + if the sum of different demopgrahic groups is equal to the patient numbers for "all". 
+ Medications and Diagnoses:
  + if there is any code not belong to the list of medclass or ICD codes; 
  + if there is negative patient numbers or counts; 
  + if the number of all patients is less than the number of severe patients.
+ Labs: 
  + if there is any labs not belong to the list of loinc codes;
  + if there is negative patients nubmers; 
  + if there is negative measures in the original scale.
+ Cross over comparison: 
  + if the number of patients in DailyCounts, ClincalCourse, and Demographic are consistent; 
  + if the number of severe patients in DailyCounts, ClinialCourse, and Demographic are consistent;
  + if in any of Medications, Diagnoses or Labs, the number of patients with the code is greater than number of all patients. 
+ Lab units: check if the lab measures are consistently outside the confidence range.

2. **QC for Phase2.1**. We will check if the summary statistics obtained from Phase 2.1 patient-level data is consistent with Phase1.1 aggregated data:
+ Demographcis:
  + if there is duplicated row for the same demographic group;
  + if n_all_before, n_all_since, n_severe_before, n_severe_since are different between phase2.1 and phase1.1;
+ Labs:
  + if there is duplicated row for the same lab on the same day but with different counts/measures;
  + if there is any LOINC codes not belong to the Phase1.1 LOINC list;
  + if n_all, mean_all, stdev_all, n_severe, mean_severe, stdev_severe are different between phase2.1 and phase1.1;
+ Medications:
  + if there is duplicated row for the same MEDCLASS on the same day but with different counts;
  + if there is any MEDCLASS not belong to the Phase1.1 MEDCLASS list;
  + if n_all_before, n_all_since, n_severe_before, n_severe_since are different between phase2.1 and phase1.1;
+ Diagnoses:
  + if there is duplicated row for the same ICD code on the same day but with different counts;
  + if n_all_before, n_all_since, n_severe_before, n_severe_since are different between phase2.1 and phase1.1; 
+ ClinicalCourse:
  + if there is duplicated row for days_since_admission but with different counts;
  + if num_patients_all_still_in_hospital, num_patients_ever_severe_still_in_hospital are different between phase2.1 and phase1.1; 
  
3. **Generate QC report**
The QC reports for Phase1.1 and Phase2.1 will be generated in word format. 


## Data pivot 

The R library (`FourCePhase2.1Data`) in this repository also contains functions for data manipulation, which generates data in specific formats to fit into different analyses:  
1. **Longitudinal data for Labs**: days since admission 
2. **Longitudinal data for Medications**: before and since admission 
3. **Longitudinal data for Diagnoses**: before and since admission 
4. **Baseline covariates**: demographic variables, lab measures at day 0, medications and diagnoses before admission. 
5. **Time varying covariates**: to be added
6. **Event time data**: for each event outcome (severe, deceased, and severe AND deceased), we derive observed event time, and the indicator of obsering the event. 

# Get Started

1. Run the Docker container and launch the localhost Rstudio. The details can be found in (https://github.com/covidclinical/Phase2.1DockerAnalysis).


2. Install and load the R package:

``` R
devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", upgrade=FALSE)
```
3. Store the Phase1.1 data and Phase2.1 data in the /4ceData/Input directory that is mounted to the container. The list of .csv files is as follows:
## Phase 1.1
+ Labs-[siteid].csv
+ Medications-[siteid].csv
+ Diagnoses-[siteid].csv
+ Demographics-[siteid].csv
+ DailyCounts-[siteid].csv
+ ClinicalCourse-[siteid].csv
## Phase 2.1
+ LocalPatientClinicalCourse.csv
+ LocalPatientObservations.csv
+ LocalPatientSummary.csv
+ LocalPatientMapping.csv

The above files are outputs Phase2.1SqlDataExtract (https://github.com/covidclinical/Phase2.1SqlDataExtraction). Make sure you have the correct file names. 

4. Conduct QC. 
``` R
currSiteId = FourCePhase2.1Data::getSiteId()
FourCePhase2.1Data::runQC(currSiteId)
```

5. If the above steps have worked correctly (no matter issues identified or not), you should be able to see the following QC reports in the /4ceData/Input directory:

+ Phase1.1DataQCReport.[siteid].doc
+ Phase2.1DataQCReport.[siteid].doc

**If there is any issue identified in Step 5, besides the QC reports, there will be an error message returned by the function. Please fix the issue before going to Data Pivot or downstream analysis.**

6. Data Pivot. Please note the Data Pivot step is option. You can either use functions in this package to generate data pivot or use your own funciton. The list of data pivots is as follows:

+ Labs_Longitudinal
+ Medications_Longitudinal
+ Diagnoses_Longitudina
+ Covariates_Baseline
+ EventTime

``` R
pivotData_Labs_Longitudinal(siteid)
pivotData_Medications_Longitudinal(siteid)
pivotData_Diagnoses_Longitudinal(siteid)
pivotData_Covariates_Baseline(siteid)
pivotData_EventTime(siteid)

```

9. If step 6 has worked correctly, you should be able to see the following files in the output directory:

+ Phase2.1DataPivot_Labs_Longitudinal.csv
+ Phase2.1DataPivot_Medications_Longitudinal.csv
+ Phase2.1DataPivot_Diagnoses_Longitudinal.csv
+ Phase2.1DataPivota_Covariates_Baseline.csv
+ Phase2.1DataPivot_EventTime.csv




