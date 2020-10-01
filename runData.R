
#' Cleans the Phase2 data
#'
#' @keywords 4CE Phase2 Project
#' @export

pivotData_Labs_Longitudinal <- function(siteid) {
  dir.input=getInputDataDirectoryName()
  phase2.ClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  phase2.PatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  phase2.PatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  
  data(code.dict, package="FourCePhase2.1Data")
    dat.x.raw=Phase2LocalPatientObservations
    res=lapply(0:max(dat.x.raw$days_since_admission), function(days_since_admission) data_lab_clean(dat.x.raw, code.dict, days_since_admission))
    res=do.call(rbind,res)
    res=res[,c("patient_num", "days_since_admission", setdiff(colnames(res), c("patient_num", "days_since_admission")))]
    res=res[order(res$patient_num, res$days_since_admission), ]
    row.names(res)=NULL
    res
}

pivotData_Medications_Longitudinal <- function(siteid) {
  dir.input=getInputDataDirectoryName()
  phase2.ClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  phase2.PatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  phase2.PatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  
  
  dat.x.raw=Phase2LocalPatientObservations
  res=NULL
  res=lapply(c("before_admission", "since_admission"), function(days) data_med_clean(dat.x.raw, days))
  res=do.call(rbind,res)
  res=res[order(res$patient_num, res$days), ]
  row.names(res)=NULL
  res
}

pivotData_Diagnoses_Longitudinal <- function(siteid) {
  dir.input=getInputDataDirectoryName()
  phase2.ClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  phase2.PatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  phase2.PatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  
  dat.x.raw=Phase2LocalPatientObservations
  res=NULL
  nm.icd.all=sort(as.character(unique(dat.x.raw$concept_code[dat.x.raw$concept_type=="DIAG-ICD10"])))
  res=lapply(c("before_admission", "since_admission"), function(days) data_icd_clean(dat.x.raw, nm.icd.all, days))
  res=do.call(rbind,res)
  res=res[order(res$patient_num, res$days), ]
  res=res[,c("patient_num", "days", setdiff(colnames(res), c("patient_num", "days")))]
  row.names(res)=NULL
  res
}

pivotData_Covariates_Baseline <- function(siteid) {
  dir.input=getInputDataDirectoryName()
  phase2.ClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  phase2.PatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  phase2.PatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  
  data(code.dict, package="FourCePhase2.1Data")
  dat.surv.raw=Phase2LocalPatientClinicalCourse
  dat.x.raw=Phase2LocalPatientObservations
  dat.dem.raw=Phase2LocalPatientSummary
  res=data_baseline_clean(code.dict, dat.surv.raw, dat.x.raw, dat.dem.raw)
  res
}

pivotData_EventTime <- function(siteid) {
  dir.input=getInputDataDirectoryName()
  phase2.ClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  phase2.PatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  phase2.PatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  
  res=data_event_clean(nm.event, Phase2LocalPatientClinicalCourse, daymax=30)
  res
}


