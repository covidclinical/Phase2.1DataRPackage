# Phase2.1DataRPackage
This repository contains R functions for Phase 2.1 Data Pre-Processing for the 4CE Consortium.

# Installation

To install this package in R:

``` R
devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", upgrade=FALSE, ref="ChuanHong-testing)
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


## Getting the raw data

The R library (`FourCePhase2.1Data`) in this repository contains functions to read in the Phase2.1 Data. 

## Getting the obfuscation level

The R library (`FourCePhase2.1Data`) in this repository contains functions to get the obfuscation level of a specific site.

# Get Started

1. Run the Docker container and launch the localhost Rstudio. The details can be found in (https://github.com/covidclinical/Phase2.1DockerAnalysis).


2. Install and load the R package:

``` R
devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", ref="ChuanHong-testing", upgrade=FALSE)
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

5. If the above steps have worked correctly, you should be able to see the following QC report in the /4ceData/Input directory:

+ Phase2.1QC_Report.doc

**If there is any issue identified in Step 5, please fix the issue before going to next step.**

6. Getting the Phase2.1 Data. In this step, the functions simply read in the csv files. If the column names were upper case in the original csv files, the Data Pivot functions change the column names to lower case. The list of data pivots is as follows:

+ All Phase1.1 csv files
+ All Phase2.1 csv files

``` R
LocalPatientClinicalCourse=getLocalPatientClinicalCourse(currSiteId)
LocalPatientObservations=getLocalPatientObservations(currSiteId)
LocalPatientSummary=getLocalPatientSummary(currSiteId)
LocalPatientMapping=getLocalPatientMapping(currSiteId)
Labs=getLabs(currSiteId)
Medications=getMedications(currSiteId)
Diagnoses=getDiagnoses(currSiteId)
Demographics=getDemographics(currSiteId)
DailyCounts=getDailyCounts(currSiteId)
ClinicalCourse=getClinicalCourse(currSiteId)

```

7. Getting obfuscation level. In this step, the function returns the obfuscation level of a specific site. 
``` R
obfuscation_level=getObfuscation(currSiteId)

```

