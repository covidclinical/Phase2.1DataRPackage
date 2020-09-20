
#' Cleans the Phase2 data
#'
#' @keywords 4CE Phase2 Project
#' @export

runData=function(phase2.ClinicalCourse, phase2.PatientObservations, phase2.PatientSummary, data.type, output.dir){
  if(data.type=="Labs_Longitudinal"){
    data_pivot=runData_Labs_Longitudinal(phase2.PatientObservations, output.dir)}
if(data.type=="Medications_Longitudinal"){
  data_pivot=runData_Medications_Longitudinal(phase2.PatientObservations, output.dir)}
if(data.type=="Diagnoses_Longitudinal"){
  data_pivot=runData_Diagnoses_Longitudinal(phase2.PatientObservations, output.dir)}
if(data.type=="Covariates_Baseline"){
  data_pivot=runData_Covariates_Baseline(phase2.ClinicalCourse, phase2.PatientObservations, phase2.PatientSummary, output.dir)}
if(data.type=="EventTime"){
  data_pivot=runData_EventTime(phase2.ClinicalCourse, output.dir)}
  write.csv(data_pivot, file=paste0(output.dir, "Phase2.1DataPivot_", data.type,".csv"), row.names=F)
}

runData_Labs_Longitudinal <- function(Phase2LocalPatientObservations, output.dir) {
    data(code.dict)
    dat.x.raw=Phase2LocalPatientObservations
    res=lapply(0:max(dat.x.raw$days_since_admission), function(days_since_admission) data_lab_clean(dat.x.raw, code.dict, days_since_admission))
    res=do.call(rbind,res)
    res=res[,c("patient_num", "days_since_admission", setdiff(colnames(res), c("patient_num", "days_since_admission")))]
    res=res[order(res$patient_num, res$days_since_admission), ]
    row.names(res)=NULL
    res
}

runData_Medications_Longitudinal <- function(Phase2LocalPatientObservations, output.dir) {
  dat.x.raw=Phase2LocalPatientObservations
  res=NULL
  res=lapply(c("before_admission", "since_admission"), function(days) data_med_clean(dat.x.raw, days))
  res=do.call(rbind,res)
  res=res[order(res$patient_num, res$days), ]
  row.names(res)=NULL
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
  res
}

runData_Covariates_Baseline <- function(Phase2LocalPatientClinicalCourse, Phase2LocalPatientObservations, Phase2LocalPatientSummary, output.dir) {
  data(code.dict)
  dat.surv.raw=Phase2LocalPatientClinicalCourse
  dat.x.raw=Phase2LocalPatientObservations
  dat.dem.raw=Phase2LocalPatientSummary
  res=data_baseline_clean(code.dict, dat.surv.raw, dat.x.raw, dat.dem.raw)
  res
}

runData_EventTime <- function(Phase2LocalPatientClinicalCourse, output.dir) {
  res=data_event_clean(nm.event, Phase2LocalPatientClinicalCourse, daymax=30)
  res
}


