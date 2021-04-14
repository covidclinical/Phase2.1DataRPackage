
#' Conducts QC for Phase2 Data and Generates QC Reports
#'
#' @keywords 4CE Phase2 Project
#' @export

runQC_nodocker <- function(siteid, dir.input){
  dir.output=dir.input
  # read Phase1.1 and Phase2.1 data
  phase1.Labs=getLabs_nodocker(siteid,dir.input)
  phase1.Medications=getMedications_nodocker(siteid,dir.input)
  phase1.Diagnoses=getDiagnoses_nodocker(siteid,dir.input)
  phase1.Demographics=getDemographics_nodocker(siteid,dir.input)
  phase1.DailyCounts=getDailyCounts_nodocker(siteid,dir.input)
  phase1.ClinicalCourse=getClinicalCourse_nodocker(siteid,dir.input)
  
  phase2.ClinicalCourse=getLocalPatientClinicalCourse_nodocker(siteid,dir.input)
  phase2.PatientObservations=getLocalPatientObservations_nodocker(siteid,dir.input)
  phase2.PatientSummary=getLocalPatientSummary_nodocker(siteid,dir.input)
  
  colnames(phase1.Labs)=tolower(colnames(phase1.Labs))
  colnames(phase1.Medications)=tolower(colnames(phase1.Medications))
  colnames(phase1.Diagnoses)=tolower(colnames(phase1.Diagnoses))
  colnames(phase1.Demographics)=tolower(colnames(phase1.Demographics))
  colnames(phase1.DailyCounts)=tolower(colnames(phase1.DailyCounts))
  colnames(phase1.ClinicalCourse)=tolower(colnames(phase1.ClinicalCourse))

  colnames(phase2.ClinicalCourse)=tolower(colnames(phase2.ClinicalCourse))
  colnames(phase2.PatientObservations)=tolower(colnames(phase2.PatientObservations))
  colnames(phase2.PatientSummary)=tolower(colnames(phase2.PatientSummary))
  
  # QC for Phase1.1
  file.nm1=file.path(dir.output, paste0("Phase1.1DataQCReport.", siteid,".txt"))
  phase1.1.res=runQC_Phase1.1_report(file.nm1, phase1.DailyCounts,phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, output.dir,site.nm=siteid)
  nm.res.print=c("Column Names", "Demographics", "ClinicalCourse", "DailyCounts", "Crossover", "Diagnoses", "Medications", "Labs", "Lab units")
  is.error1=0
  for(ii in 1:length(phase1.1.res)){
    nm=names(phase1.1.res)[ii]
    nm.print=nm.res.print[ii]
    print(paste0("Checking Phase1.1 ", nm.print, " ..."))
    tmp=phase1.1.res[[nm]]
    if(dim(tmp$err.report)[1]!=0 & any(is.na(tmp$err.report)!=1)){print("Error found."); is.error1=is.error1+1}else{print("Ok ...")}
  }
  
  # QC for Phase2.1
  file.nm2=file.path(dir.output, paste0("Phase2.1DataQCReport.", siteid,".txt"))
  is.error2=runQC_Phase2.1_report(file.nm2, phase2.ClinicalCourse, phase2.PatientObservations, phase2.PatientSummary, phase1.DailyCounts, phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, output.dir, site.nm=siteid)
}
  

