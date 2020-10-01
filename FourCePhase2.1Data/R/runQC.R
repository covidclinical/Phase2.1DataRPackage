
#' Conducts QC for Phase2 Data and Generates QC Reports
#'
#' @keywords 4CE Phase2 Project
#' @export

runQC <- function(siteid){
  dir.input=getInputDataDirectoryName()
  phase1.Labs=read.csv(paste0(dir.input,"/Labs-", siteid,".csv"))
  phase1.Medications=read.csv(paste0(dir.input, "/Medications-", siteid,".csv"))
  phase1.Diagnoses=read.csv(paste0(dir.input, "/Diagnoses-", siteid,".csv"))
  phase1.Demographics=read.csv(paste0(dir.input, "/Demographics-", siteid,".csv"))
  phase1.DailyCounts=read.csv(paste0(dir.input, "/DailyCounts-", siteid,".csv"))
  phase1.ClinicalCourse=read.csv(paste0(dir.input, "/ClinicalCourse-", siteid,".csv"))
  
  phase2.ClinicalCourse=read.csv(paste0(dir.input, "/LocalPatientClinicalCourse.csv"))
  phase2.PatientObservations=read.csv(paste0(dir.input, "/LocalPatientObservations.csv"))
  phase2.PatientSummary=read.csv(paste0(dir.input, "/LocalPatientSummary.csv"))
  
  file.nm1="Phase1.1QC_Report.doc"
  rtffile <- RTF(file.nm1)  
  output.dir=getOutputDataDirectoryName()

  runQC_Phase1.1_report(rtffile, phase1.DailyCounts,phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, output.dir,site.nm="MGB")
  done(rtffile)
  
  file.nm2=paste0(output.dir, "Phase2.1QC_Report.doc")
  rtffile <- RTF(file.nm2)  
  tryCatch(addParagraph(rtffile, "Phase2.1 QC Report\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, paste0(Sys.Date(),"\n")), error=function(e) NA)
  addParagraph(rtffile, paste0("+++++++++++++++++++++++++++++++++++++++++++++++\n"))
  Phase2QC_Tab_Labs=runQC_tab_lab(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Labs, output.dir)
  addParagraph(rtffile, paste0("+++++++++++++++++++++++++++++++++++++++++++++++\n"))
  Phase2QC_Tab_Medications=runQC_tab_med(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Medications, output.dir)
  addParagraph(rtffile, paste0("+++++++++++++++++++++++++++++++++++++++++++++++\n"))
  Phase2QC_Tab_Diagnoses=runQC_tab_diag(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Diagnoses, output.dir)
  addParagraph(rtffile, paste0("+++++++++++++++++++++++++++++++++++++++++++++++\n"))
  Phase2QC_Tab_Demographic=runQC_tab_dem(rtffile, phase2.PatientSummary, phase2.PatientObservations, phase1.Demographics, output.dir)
  addParagraph(rtffile, paste0("+++++++++++++++++++++++++++++++++++++++++++++++\n"))
  Phase2QC_Tab_ClinicalCourse=runQC_tab_cc(rtffile, phase2.ClinicalCourse, phase1.ClinicalCourse, output.dir)
  done(rtffile)
}
  
runQC_tab_lab <- function(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Labs, output.dir) {
  junk=tab_compare_lab(myday=14, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Labs)
  res=junk$res
  nm.lab.add=junk$nm.lab.add
  nm.duplicated=res[duplicated(res[,"labname"]),c("labname")]
  tryCatch(addParagraph(rtffile, "Labs\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "LOINCs not in Phase1.1:"), error=function(e) NA)
  if(length(nm.lab.add)!=0){
    tryCatch(addParagraph(rtffile, paste0(paste(nm.lab.add, collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")  
    }
  
  #for (nm in c("n_all", "mean_all", "stdev_all", "mean_log_all", "stdev_log_all", 
  #             "n_severe", "mean_severe", "stdev_severe", "mean_log_severe", "stdev_log_severe")){
  for (nm in c("n_all", "mean_all", "stdev_all",
                "n_severe", "mean_severe", "stdev_severe")){
  nm1=paste0("p1.", nm)
  nm2=paste0("p2.", nm)
  nm.check=paste0("nm.diff.",nm)
  nm.labname=unique(res[which(round(res[,nm1],5)!=round(res[,nm2],5)),"labname"])
  tryCatch(addParagraph(rtffile, paste0("Labs with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
  if(length(nm.labname)!=0){
    tryCatch(addParagraph(rtffile, paste0(paste(nm.labname,collapse=";"),"\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  }
}

runQC_tab_med <- function(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Medications, output.dir) {
    junk=tab_compare_med(phase2.PatientObservations, phase2.ClinicalCourse, phase1.Medications)
    res=junk$res
    nm.med.add=junk$nm.med.add
    nm.duplicated=res[duplicated(res[,"medclass"]),c("medclass")]
    tryCatch(addParagraph(rtffile, "Medications\n"), error=function(e) NA)
    tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
    if(length(nm.duplicated)!=0){
      tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
    tryCatch(addParagraph(rtffile, "MEDCLASS not in Phase1.1:"), error=function(e) NA)
    if(length(nm.med.add)!=0){
    tryCatch(addParagraph(rtffile, paste0(paste(nm.med.add, collapse=";"),"\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
    
    for (nm in c("n_all_before", "n_all_since", "n_severe_before", "n_severe_since")){
      nm1=paste0("p1.", nm)
      nm2=paste0("p2.", nm)
      nm.check=paste0("nm.diff.",nm)
      nm.medclass=unique(res[which(res[,nm1]!=res[,nm2]),"medclass"])
      tryCatch(addParagraph(rtffile, paste0("Medclass with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
      if(length(nm.medclass)!=0){
        tryCatch(addParagraph(rtffile, paste0(paste(nm.medclass,collapse=";"),"\n")), error=function(e) NA)}else{
          addParagraph(rtffile, "no issue identified\n")
        }
    }
}

runQC_tab_diag <- function(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Diagnoses, output.dir) {
  res=tab_compare_diag(phase2.PatientObservations, phase2.ClinicalCourse, phase1.Diagnoses)
  nm.duplicated=res[duplicated(res[,"diag-icd"]),c("diag-icd")]
  tryCatch(addParagraph(rtffile, "Diagnoses\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  for (nm in c("n_all_before", "n_all_since", "n_severe_before", "n_severe_since")){
    nm1=paste0("p1.", nm)
    nm2=paste0("p2.", nm)
    nm.check=paste0("nm.diff.",nm)
    nm.medclass=unique(res[which(res[,nm1]!=res[,nm2]),c("diag-icd")])
    tryCatch(addParagraph(rtffile, paste0("Diagnoses with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
    if(length(nm.medclass)!=0){
      tryCatch(addParagraph(rtffile, paste0(paste(nm.medclass,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
}

runQC_tab_dem <- function(rtffile, phase2.PatientSummary, phase2.PatientObservations, phase1.Demographics, output.dir) {
  res=tab_compare_dem(phase2.PatientSummary, phase2.PatientObservations, phase1.Demographics)
  nm.duplicated=res[duplicated(res[,c("sex", "age_group", "race")]),c(c("sex", "age_group", "race"))]
  tryCatch(addParagraph(rtffile, "Demographics\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(dim(nm.duplicated)[1]!=0){
    nm.duplicated=unlist(lapply(1:dim(nm.duplicated)[1], function(ll) paste(paste0(colnames(nm.duplicated),"=",nm.duplicated[ll,]),collapse=":")))
    tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  for (nm in c("n_all",  "n_severe")){
    nm1=paste0("p1.", nm)
    nm2=paste0("p2.", nm)
    nm.check=paste0("nm.diff.",nm)
    nm.group=res[which(res[,nm1]!=res[,nm2]),c("sex", "age_group", "race")]
    tryCatch(addParagraph(rtffile, paste0("Demographic Group with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
    if(dim(nm.group)[1]!=0){
      nm.group=unlist(lapply(1:dim(nm.group)[1], function(ll) paste(paste0(colnames(nm.group),"=",nm.group[ll,]),collapse=":")))
      tryCatch(addParagraph(rtffile, paste0(paste(nm.group,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
}

runQC_tab_cc <- function(rtffile, phase2.ClinicalCourse, phase1.ClinicalCourse, output.dir) {
  res=tab_compare_cc(phase2.ClinicalCourse, phase1.ClinicalCourse)
  nm.duplicated=res[duplicated(res[,"days_since_admission"]),c("days_since_admission")]
  tryCatch(addParagraph(rtffile, "ClinicalCourse\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  for (nm in c("num_patients_all_still_in_hospital", "num_patients_all_still_in_hospital", 
               "num_patients_ever_severe_still_in_hospital", "num_patients_ever_severe_still_in_hospital")){
    nm1=paste0("p1.", nm)
    nm2=paste0("p2.", nm)
    nm.check=paste0("nm.diff.",nm)
    nm.day=unique(res[which(res[,nm1]!=res[,nm2]),c("days_since_admission")])
    tryCatch(addParagraph(rtffile, paste0("ClinicalCourse with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
    if(length(nm.day)!=0){
      tryCatch(addParagraph(rtffile, paste0(paste(nm.day,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
}

runQC_Phase1.1_report=function(rtffile,phase1.DailyCounts, phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, output.dir, site.nm){
  qc.res=qc_site(phase1.DailyCounts, phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, site.nm)
  colnames(qc.res$qc.dm$err.report)=
    colnames(qc.res$qc.cc$err.report)=
    colnames(qc.res$qc.dc$err.report)=
    colnames(qc.res$qc.crossover$err.report)=
    colnames(qc.res$qc.icd$err.report)=
    colnames(qc.res$qc.med$err.report)=
    colnames(qc.res$qc.lab$err.report)=
    colnames(qc.res$qc.lab.unit$err.report)=
    c("SiteID", "Possible Issues")
  
  tryCatch(addParagraph(rtffile, "Phase1.1 QC Report\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, paste0(Sys.Date(),"\n")), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Demographics\n"), error=function(e) NA)
  if(dim(qc.res$qc.dm$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.dm$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "\n\nClinicalCourse:\n"), error=function(e) NA)
  if(dim(qc.res$qc.cc$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.cc$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "\n\nDailyCounts:\n"), error=function(e) NA)
  if(dim(qc.res$qc.dc$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.dc$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "\n\nCrossover:\n"), error=function(e) NA)
  if(dim(qc.res$qc.crossover$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.crossover$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "\n\nDiagnoses:\n"), error=function(e) NA)
  if(dim(qc.res$qc.icd$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.icd$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "\n\nMedications:\n"))
  if(dim(qc.res$qc.med$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.med$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "\n\nLabs:\n"))
  if(dim(qc.res$qc.lab$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.lab$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  tryCatch(addParagraph(rtffile, "\n\nLabs unit:\n"))
  if(dim(qc.res$qc.lab.unit$err.report)[1]!=0 & is.na(qc.res$qc.lab.unit$err.report[1])!=1){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.lab.unit$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
}


