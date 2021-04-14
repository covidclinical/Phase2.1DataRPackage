
#' Obtain the phase2.1 data
#'
#' @keywords 4CE Phase2 Project
#' @export

getLocalPatientClinicalCourse<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  colnames(LocalPatientClinicalCourse)=tolower(colnames(LocalPatientClinicalCourse))
  LocalPatientClinicalCourse
}

getLocalPatientObservations<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  colnames(LocalPatientObservations)=tolower(colnames(LocalPatientObservations))
  LocalPatientObservations
}

getLocalPatientSummary<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  colnames(LocalPatientSummary)=tolower(colnames(LocalPatientSummary))
  LocalPatientSummary
}

getLocalPatientMapping<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  LocalPatientMapping=read.csv(paste0(dir.input, "/LocalPatientMapping.csv"))
  colnames(LocalPatientMapping)=tolower(colnames(LocalPatientMapping))
  LocalPatientMapping
}

getLabs<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  Labs=read.csv(paste0(dir.input,"/Labs-", siteid,".csv"))
  Labs
}

getMedications<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  Medications=read.csv(paste0(dir.input,"/Medications-", siteid,".csv"))
  Medications
}

getDiagnoses<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  Diagnoses=read.csv(paste0(dir.input,"/Diagnoses-", siteid,".csv"))
  Diagnoses
}

getDemographics<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  Demographics=read.csv(paste0(dir.input,"/Demographics-", siteid,".csv"))
  Demographics
}

getDailyCounts<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  DailyCounts=read.csv(paste0(dir.input,"/DailyCounts-", siteid,".csv"))
  DailyCounts
}

getClinicalCourse<- function(siteid) {
  dir.input=getInputDataDirectoryName()
  ClinicalCourse=read.csv(paste0(dir.input,"/ClinicalCourse-", siteid,".csv"))
  ClinicalCourse
}

