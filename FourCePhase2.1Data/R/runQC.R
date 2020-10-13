
#' Conducts QC for Phase2 Data and Generates QC Reports
#'
#' @keywords 4CE Phase2 Project
#' @export

runQC <- function(siteid){
  dir.input=getInputDataDirectoryName()
  dir.output=dir.input
  # read Phase1.1 and Phase2.1 data
  phase1.Labs=read.csv(paste0(dir.input,"/Labs-", siteid,".csv"))
  phase1.Medications=read.csv(paste0(dir.input, "/Medications-", siteid,".csv"))
  phase1.Diagnoses=read.csv(paste0(dir.input, "/Diagnoses-", siteid,".csv"))
  phase1.Demographics=read.csv(paste0(dir.input, "/Demographics-", siteid,".csv"))
  phase1.DailyCounts=read.csv(paste0(dir.input, "/DailyCounts-", siteid,".csv"))
  phase1.ClinicalCourse=read.csv(paste0(dir.input, "/ClinicalCourse-", siteid,".csv"))
  
  phase2.ClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  phase2.PatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  phase2.PatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  
  # QC for Phase1.1
  file.nm1=paste0(dir.output, "/Phase1.1DataQCReport.", siteid,".doc")
  rtffile <- RTF(file.nm1)  
  phase1.1.res=runQC_Phase1.1_report(rtffile, phase1.DailyCounts,phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, output.dir,site.nm=siteid)
  done(rtffile)
  nm.res.print=c("Demographics", "ClinicalCourse", "DailyCounts", "Crossover", "Diagnoses", "Medications", "Labs", "Lab units")
  is.error1=0
  for(ii in 1:length(phase1.1.res)){
    nm=names(phase1.1.res)[ii]
    nm.print=nm.res.print[ii]
    print(paste0("Checking Phase1.1 ", nm.print, " ..."))
    tmp=phase1.1.res[[nm]]
    if(dim(tmp$err.report)[1]!=0 & any(is.na(tmp$err.report)!=1)){print("Error found."); is.error1=is.error1+1}else{print("Ok ...")}
  }
  
  # QC for Phase2.1
  file.nm2=paste0(dir.output, "/Phase2.1DataQCReport.", siteid,".doc")
  rtffile <- RTF(file.nm2)  
  is.error2=runQC_Phase2.1_report(rtffile,phase2.ClinicalCourse, phase2.PatientObservations, phase2.PatientSummary, phase1.DailyCounts, phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, output.dir, site.nm=siteid)
    
  done(rtffile)
  
  #if((is.error1+is.error2)!=0){
  #stop('QC isssues identified. Details can be find in "Phase1.1DataQCReport.doc" and "Phase2.1DataQCReport.doc" in the input directory. Please fix all the issues before conducting analysis.')
#}
}
  

