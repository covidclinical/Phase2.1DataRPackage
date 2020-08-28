
#' Cleans the Phase2 data
#'
#' @keywords 4CE Phase2 Project
#' @export

runData_Labs_Longitudinal <- function(Phase2LocalPatientObservations, output.dir) {
    data(code.dict)
    dat.x.raw=Phase2LocalPatientObservations
    res=lapply(0:max(dat.x.raw$days_since_admission), function(days_since_admission) data_lab_clean(dat.x.raw, code.dict, days_since_admission))
    res=do.call(rbind,res)
    res=res[,c("patient_num", "days_since_admission", setdiff(colnames(res), c("patient_num", "days_since_admission")))]
    res=res[order(res$patient_num, res$days_since_admission), ]
    row.names(res)=NULL
    write.csv(res, file=paste0(output.dir, "Phase2Data_Labs_Longitudinal.csv"), row.names=F)
    res
}

runData_Medications_Longitudinal <- function(Phase2LocalPatientObservations, output.dir) {
  dat.x.raw=Phase2LocalPatientObservations
  res=NULL
  res=lapply(c("before_admission", "since_admission"), function(days) data_med_clean(dat.x.raw, days))
  res=do.call(rbind,res)
  res=res[order(res$patient_num, res$days), ]
  row.names(res)=NULL
  write.csv(res, file=paste0(output.dir, "Phase2Data_Medications_Longitudinal.csv"), row.names=F)
  
  res
}

runData_Diagnoses_Longitudinal <- function(Phase2LocalPatientObservations, output.dir) {
  dat.x.raw=Phase2LocalPatientObservations
  res=NULL
  nm.icd.all=sort(as.character(unique(dat.x.raw$concept_code[dat.x.raw$concept_type=="DIAG-ICD10"])))
  res=lapply(c("before_admission", "since_admission"), function(days) data_icd_clean(dat.x.raw, nm.icd.all, days))
  res=do.call(rbind,res)
  res=res[order(res$patient_num, res$days), ]
  res=res[,c("patient_num", "days", setdiff(colnames(res), c("patient_num", "days")))]
  row.names(res)=NULL
  write.csv(res, file=paste0(output.dir, "Phase2Data_Diagnoses_Longitudinal.csv"), row.names=F)
  
  res
}

runData_Covariates_Baseline <- function(Phase2LocalPatientClinicalCourse, Phase2LocalPatientObservations, Phase2LocalPatientSummary, output.dir) {
  data(code.dict)
  dat.surv.raw=Phase2LocalPatientClinicalCourse
  dat.x.raw=Phase2LocalPatientObservations
  dat.dem.raw=Phase2LocalPatientSummary
  res=data_baseline_clean(code.dict, dat.surv.raw, dat.x.raw, dat.dem.raw)
  write.csv(res, file=paste0(output.dir, "Phase2Data_Covariates_Baseline.csv"), row.names=F)
  
  res
}

runData_EventTime <- function(Phase2LocalPatientClinicalCourse, output.dir) {
  res=data_event_clean(nm.event, Phase2LocalPatientClinicalCourse, daymax=30)
  write.csv(res, file=paste0(output.dir, "Phase2Data_EventTime.csv"), row.names=F)
  res
}


