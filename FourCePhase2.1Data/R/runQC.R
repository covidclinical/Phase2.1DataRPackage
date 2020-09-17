
#' Conducts QC for Phase2 Data and Generates QC Reports
#'
#' @keywords 4CE Phase2 Project
#' @export

runQC_tab_lab <- function(phase2.cc, phase2.po, phase1.lab, output.dir) {
  res=NULL
  for(myday in c(0,14) ){
  res=rbind(res,cbind(days_since_admission=myday, tab_compare_lab(myday, phase2.cc, phase2.po, phase1.lab)))
  }
  write.csv(res, file=paste0(output.dir, "Phase2QC_Tab_Labs.csv"), row.names=F)
  res
}

runQC_tab_med <- function(phase2.cc, phase2.po, phase1.med, output.dir) {
    res=tab_compare_med(phase2.po, phase2.cc, phase1.med)
    write.csv(res, file=paste0(output.dir, "Phase2QC_Tab_Medications.csv"), row.names=F)
    res
}

runQC_tab_diag <- function(phase2.cc, phase2.po, phase1.diag, output.dir) {
  res=tab_compare_diag(phase2.po, phase2.cc, phase1.diag)
  write.csv(res, file=paste0(output.dir, "Phase2QC_Tab_Diagnosees.csv"), row.names=F)
  res
}

runQC_tab_dem <- function(phase2.ps, phase2.po, phase1.dem, output.dir) {
  res=tab_compare_dem(phase2.ps, phase2.po, phase1.dem)
  write.csv(res, file=paste0(output.dir, "Phase2QC_Tab_Demographics.csv"), row.names=F)
  
  res
}

runQC_tab_cc <- function(phase2.cc, phase1.cc, output.dir) {
  res=tab_compare_cc(phase2.cc, phase1.cc)
  write.csv(res, file=paste0(output.dir, "Phase2QC_Tab_ClinicalCourse.csv"), row.names=F)
  
  res
}

runQC_tab_dc <- function(phase2.cc, phase1.dc, output.dir) {
  res=tab_compare_dc(phase2.cc, phase1.dc)
  write.csv(res, file=paste0(output.dir, "Phase2QC_Tab_DailyCounts.csv"), row.names=F)
  res
}

runQC_report=function(phase1.dc, phase1.cc, phase1.dem,phase1.diag, phase1.lab, phase1.med, output.dir, site.nm){
  qc.res=qc_site(phase1.dc, phase1.cc, phase1.dem,phase1.diag, phase1.lab, phase1.med, site.nm)
  colnames(qc.res$qc.dm$err.report)=
    colnames(qc.res$qc.cc$err.report)=
    colnames(qc.res$qc.dc$err.report)=
    colnames(qc.res$qc.crossover$err.report)=
    colnames(qc.res$qc.icd$err.report)=
    colnames(qc.res$qc.med$err.report)=
    colnames(qc.res$qc.lab$err.report)=
    colnames(qc.res$qc.lab.unit$err.report)=
    c("SiteID", "Possible Issues")
  
  file.nm=paste0(output.dir, "Phase2QC_Report.doc")
  rtffile <- RTF(file.nm)  # this can be an .rtf or a .doc
  tryCatch(addParagraph(rtffile, "QC Metrics\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, paste0(Sys.Date(),"\n")), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Demographic\n"), error=function(e) NA)
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
  if(dim(qc.res$qc.lab.unit$err.report)[1]!=0){
    tryCatch(addTable(rtffile, as.data.frame(qc.res$qc.lab.unit$err.report)), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  done(rtffile)
}




