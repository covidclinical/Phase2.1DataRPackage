
#' Obtain the phase2.1 data
#'
#' @keywords 4CE Phase2 Project
#' @export

pivotData_LocalPatientClinicalCourse<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  colnames(LocalPatientClinicalCourse)=tolower(colnames(LocalPatientClinicalCourse))
  LocalPatientClinicalCourse
}

pivotData_LocalPatientObservations<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  colnames(LocalPatientObservations)=tolower(colnames(LocalPatientObservations))
  LocalPatientObservations
}

pivotData_LocalPatientSummary<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  colnames(LocalPatientSummary)=tolower(colnames(LocalPatientSummary))
  LocalPatientSummary
}

pivotData_LocalPatientMapping<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientMapping=read.csv(paste0(dir.input, "/LocalPatientMapping.csv"))
  colnames(LocalPatientMapping)=tolower(colnames(LocalPatientMapping))
  LocalPatientMapping
}

pivotData_Labs<- function(siteid) {
  Labs=read.csv(paste0(dir.input,"/Labs-", siteid,".csv"))
  Labs
}

pivotData_Medications<- function(siteid) {
  Medications=read.csv(paste0(dir.input,"/Medications-", siteid,".csv"))
  Medications
}

pivotData_Diagnoses<- function(siteid) {
  Diagnoses=read.csv(paste0(dir.input,"/Diagnoses-", siteid,".csv"))
  Diagnoses
}

pivotData_Demographics<- function(siteid) {
  Demographics=read.csv(paste0(dir.input,"/Demographics-", siteid,".csv"))
  Demographics
}

pivotData_DailyCounts<- function(siteid) {
  DailyCounts=read.csv(paste0(dir.input,"/DailyCounts-", siteid,".csv"))
  DailyCounts
}

pivotData_ClinicalCourse<- function(siteid) {
  ClinicalCourse=read.csv(paste0(dir.input,"/ClinicalCourse-", siteid,".csv"))
  ClinicalCourse
}

