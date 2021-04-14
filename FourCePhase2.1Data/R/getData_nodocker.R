
#' Obtain the phase2.1 data
#'
#' @keywords 4CE Phase2 Project
#' @export

getLocalPatientClinicalCourse_nodocker<- function(siteid, dir.input) {
  LocalPatientClinicalCourse=read.csv(file.path(dir.input, "LocalPatientClinicalCourse.csv"))
  colnames(LocalPatientClinicalCourse)=tolower(colnames(LocalPatientClinicalCourse))
  LocalPatientClinicalCourse
}

getLocalPatientObservations_nodocker<- function(siteid, dir.input) {
  LocalPatientObservations=read.csv(file.path(dir.input, "LocalPatientObservations.csv"))
  colnames(LocalPatientObservations)=tolower(colnames(LocalPatientObservations))
  LocalPatientObservations
}

getLocalPatientSummary_nodocker<- function(siteid, dir.input) {
  LocalPatientSummary=read.csv(file.path(dir.input, "LocalPatientSummary.csv"))
  colnames(LocalPatientSummary)=tolower(colnames(LocalPatientSummary))
  LocalPatientSummary
}

getLocalPatientMapping_nodocker<- function(siteid, dir.input) {
  LocalPatientMapping=read.csv(file.path(dir.input, "LocalPatientMapping.csv"))
  colnames(LocalPatientMapping)=tolower(colnames(LocalPatientMapping))
  LocalPatientMapping
}

getLabs_nodocker<- function(siteid,dir.input) {
  Labs=read.csv(file.path(dir.input,paste0("Labs-", siteid,".csv")))
  Labs
}

getMedications_nodocker<- function(siteid,dir.input) {
  Medications=read.csv(file.path(dir.input,paste0("Medications-", siteid,".csv")))
  Medications
}

getDiagnoses_nodocker<- function(siteid.dir.input) {
  Diagnoses=read.csv(file.path(dir.input,paste0("Diagnoses-", siteid,".csv")))
  Diagnoses
}

getDemographics_nodocker<- function(siteid,dir.input) {
  Demographics=read.csv(file.path(dir.input,paste0("Demographics-", siteid,".csv")))
  Demographics
}

getDailyCounts_nodocker<- function(siteid,dir.input) {
  DailyCounts=read.csv(file.path(dir.input,paste0("DailyCounts-", siteid,".csv")))
  DailyCounts
}

getClinicalCourse_nodocker<- function(siteid,dir.input) {
  ClinicalCourse=read.csv(file.path(dir.input,paste0("ClinicalCourse-", siteid,".csv")))
  ClinicalCourse
}

