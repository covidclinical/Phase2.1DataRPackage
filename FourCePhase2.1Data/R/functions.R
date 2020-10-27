
runQC_Phase2.1_report=function(rtffile,phase2.ClinicalCourse, phase2.PatientObservations, phase2.PatientSummary, phase1.DailyCounts, phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, output.dir, site.nm){
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
  Phase2QC_Tab_Labs+Phase2QC_Tab_Medications+Phase2QC_Tab_Diagnoses+Phase2QC_Tab_Demographic+Phase2QC_Tab_ClinicalCourse
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
  qc.res
}

runQC_tab_lab <- function(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Labs, output.dir) {
  print("Checking Phase2.1 Labs ...")
  junk=tab_compare_lab(myday=0, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Labs)
 
  res=junk$res
  nm.duplicated=res[duplicated(res[,"labname"]),c("labname")]
  tryCatch(addParagraph(rtffile, "Labs\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    print(paste0("Duplicated rows for: ", paste(nm.duplicated,collapse=";")))
    tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  
  for (nm in c("n_all", "mean_all", "stdev_all",
               "n_severe", "mean_severe", "stdev_severe")){
    nm1=paste0("p1.", nm)
    nm2=paste0("p2.", nm)
    nm.check=paste0("nm.diff.",nm)
    nm.labname=unique(res[which(round(res[,nm1],5)>round(res[,nm2]+res[,nm2]*0.01,5)|round(res[,nm1],5)<round(res[,nm2]-res[,nm2]*0.01,5)),"labname"])
    tryCatch(addParagraph(rtffile, paste0("Labs with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
    if(length(nm.labname)!=0){
      print(paste0("Labs with Different ", nm, " between Phase1.1 and Phase2.1: ", nm.labname))
      tryCatch(addParagraph(rtffile, paste0(paste(nm.labname,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
  is.error=length(c(nm.labname,nm.duplicated))!=0
  is.error
}

runQC_tab_med <- function(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Medications, output.dir) {
  print("Checking Phase2.1 Medications ...")
  junk=tab_compare_med(phase2.PatientObservations, phase2.ClinicalCourse, phase1.Medications)
  res=junk$res

  nm.duplicated=res[duplicated(res[,"medclass"]),c("medclass")]
  tryCatch(addParagraph(rtffile, "Medications\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    print(paste0("Duplicated rows for : ", paste(nm.duplicated, collapse=";")))
    tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  
  nm.medclass.all=NULL
  for (nm in c("n_all_before", "n_all_since", "n_severe_before", "n_severe_since")){
    nm1=paste0("p1.", nm)
    nm2.1=paste0("p2.", nm, "1")
    nm2.2=paste0("p2.", nm, "2")
    
    nm.check=paste0("nm.diff.",nm)
    nm.medclass=unique(res[which(res[,nm1]<res[,nm2.1]|res[,nm1]>res[,nm2.2]),"medclass"])
    
    nm.medclass.all=c(nm.medclass.all, nm.medclass)
    tryCatch(addParagraph(rtffile, paste0("Medclass with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
    if(length(nm.medclass)!=0){
      print(paste0("Medclass with Different ", nm, " between Phase1.1 and Phase2.1: ", paste(nm.medclass, collapse=";")))
      tryCatch(addParagraph(rtffile, paste0(paste(nm.medclass,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
  is.error=length(c(nm.medclass.all,nm.duplicated))!=0
  is.error
}

runQC_tab_diag <- function(rtffile, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Diagnoses, output.dir) {
  print("Checking Phase2.1 Diagnoses")
  res=tab_compare_diag(phase2.PatientObservations, phase2.ClinicalCourse, phase1.Diagnoses)
  nm.duplicated=res[duplicated(res[,"diag-icd"]),c("diag-icd")]
  tryCatch(addParagraph(rtffile, "Diagnoses\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    print(paste0("Duplicated rows for: ", nm.duplicated))
    tryCatch(addParagraph(rtffile, paste0(paste(nm.duplicated,collapse=";"), "\n")), error=function(e) NA)}else{
      addParagraph(rtffile, "no issue identified\n")
    }
  nm.medclass.all=NULL
  for (nm in c("n_all_before", "n_all_since", "n_severe_before", "n_severe_since")){
    nm1=paste0("p1.", nm)
    nm2.1=paste0("p2.", nm,1)
    nm2.2=paste0("p2.", nm,2)
    
    nm.check=paste0("nm.diff.",nm)
    nm.medclass=unique(res[which(res[,nm1]<res[,nm2.1]|res[,nm1]>res[,nm2.2]),"diag-icd"])
    
    nm.medclass.all=c(nm.medclass.all, nm.medclass)
    tryCatch(addParagraph(rtffile, paste0("Diagnoses with Different ", nm, " between Phase1.1 and Phase2.1:")), error=function(e) NA)
    if(length(nm.medclass)!=0){
      print(paste0("Diagnoses with Different ", nm, " between Phase1.1 and Phase2.1: ", nm.medclass))
      tryCatch(addParagraph(rtffile, paste0(paste(nm.medclass,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
  is.error=length(c(nm.medclass.all,nm.duplicated))!=0
  is.error
}

runQC_tab_dem <- function(rtffile, phase2.PatientSummary, phase2.PatientObservations, phase1.Demographics, output.dir) {
  print("Checking Phase2.1 Demographics ...")
  res=tab_compare_dem(phase2.PatientSummary, phase2.PatientObservations, phase1.Demographics)
  nm.duplicated=res[duplicated(res[,c("sex", "age_group", "race")]),c(c("sex", "age_group", "race"))]
  tryCatch(addParagraph(rtffile, "Demographics\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(dim(nm.duplicated)[1]!=0){
    nm.duplicated=unlist(lapply(1:dim(nm.duplicated)[1], function(ll) paste(paste0(colnames(nm.duplicated),"=",nm.duplicated[ll,]),collapse=":")))
    print(paste0("Duplicated rows for:", paste(nm.duplicated, collapse=';')))
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
      print(paste0("Demographic Group with Different ", nm, " between Phase1.1 and Phase2.1: ", paste(nm.group, collapse=';')))
      tryCatch(addParagraph(rtffile, paste0(paste(nm.group,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
  is.error=any(is.na(c(nm.group[1,1],nm.duplicated[1,1]))!=1)
  is.error
}

runQC_tab_cc <- function(rtffile, phase2.ClinicalCourse, phase1.ClinicalCourse, output.dir) {
  print("Checking Phase2.1 ClinicalCourse ...")
  res=tab_compare_cc(phase2.ClinicalCourse, phase1.ClinicalCourse)
  nm.duplicated=res[duplicated(res[,"days_since_admission"]),c("days_since_admission")]
  tryCatch(addParagraph(rtffile, "ClinicalCourse\n"), error=function(e) NA)
  tryCatch(addParagraph(rtffile, "Duplicated rows:"), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    print(paste0("Duplicated rows for: ", paste(nm.duplicated,collapse=";")))
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
      print(paste0("ClinicalCourse with Different ", nm, " between Phase1.1 and Phase2.1 on day: ", paste(nm.day,collapse=";")))
      tryCatch(addParagraph(rtffile, paste0(paste(nm.day,collapse=";"),"\n")), error=function(e) NA)}else{
        addParagraph(rtffile, "no issue identified\n")
      }
  }
  is.error=length(c(nm.duplicated,nm.day))!=0
  is.error
}

data_dem_clean=function(dat.dem.raw, nm.patient_num, nm.gender, nm.age, nm.race){
    dat.sub=data.frame(dat.dem.raw[,c(nm.patient_num, nm.gender, nm.age, nm.race)])
    dat.sub[,nm.age]=as.character(dat.sub[,nm.age])
    dat.sub[,nm.gender]=as.character(dat.sub[,nm.gender])
    dat.sub[,nm.race]=as.character(dat.sub[,nm.race])
    dat.sub[,nm.race]=str_to_title(dat.sub[,nm.race])
    dat.sub
}

data_surv_clean=function(dat.surv.raw, nm.patient_num, nm.days_since_admission, daymax, nm.event){
    dat.sub=dat.surv.raw[dat.surv.raw[,nm.days_since_admission]<=daymax,]
    patient_num.list=sort(unique(dat.sub[,nm.patient_num]))
    data.surv.clean.kern=function(dat.sub, patient_num){
        dat.tmp=dat.sub[dat.sub[,nm.patient_num]==patient_num,]
        dat.event.tmp=dat.tmp[which(dat.tmp[,nm.event]==1),]
        if(dim(dat.event.tmp)[1]!=0){
            dat.end.tmp=dat.event.tmp[which.min(dat.event.tmp[,nm.days_since_admission]),c(nm.patient_num, nm.days_since_admission, nm.event)]
        }else{
            dat.end.tmp=dat.tmp[which.max(dat.tmp[,nm.days_since_admission]),c(nm.patient_num, nm.days_since_admission, nm.event)]}
        dat.end.tmp
    }
    dat.res=do.call(rbind,lapply(patient_num.list, function(xx) data.surv.clean.kern(dat.sub, xx)))
    rownames(dat.res)=NULL
    dat.res
}

data_lab_clean=function(dat.x.raw, code.dict, days_since_admission){
    code.dict.new=code.dict
    colnames(code.dict.new)[1]="concept_code"
    code.dict.new$labname=gsub(" ", "_", code.dict.new$labname)
    code.dict.new$labname=gsub("\\(", "", code.dict.new$labname)
    code.dict.new$labname=gsub("\\)", "", code.dict.new$labname)
    code.dict.new$labname=gsub("-", "_", code.dict.new$labname)
    code.dict.new$labname=tolower(code.dict.new$labname)
    code.dict.new$concept_code=as.character(code.dict.new$concept_code)
    
    dat.sub=dat.x.raw[dat.x.raw[,"days_since_admission"]==days_since_admission & dat.x.raw[,"concept_type"]%in%"LAB-LOINC",]
    dat.sub=dat.sub[,c("patient_num","concept_code", "value")]
    dat.sub.wide0<- spread(dat.sub, concept_code, value)
    dat.sub.wide0=dat.sub.wide0[,c("patient_num", colnames(dat.sub.wide0)[colnames(dat.sub.wide0)%in%code.dict.new$concept_code])]
    if(is.null(dim(dat.sub.wide0))!=1){
    dat.sub.wide=data.frame(matrix(NA, nrow=dim(dat.sub.wide0)[1], ncol=length(code.dict.new[,1])+1))
    colnames(dat.sub.wide)=c("patient_num",code.dict.new[,1])
    dat.sub.wide[,colnames(dat.sub.wide0)]=dat.sub.wide0
    dat.sub.wide[dat.sub.wide==-99]=NA;dat.sub.wide[dat.sub.wide==-999]=NA

    if("48065-7"%in%colnames(dat.sub.wide)){
        dat.sub.wide[,"48065-7:48066-5"]=0.5*dat.sub.wide[,"48065-7"]
    }
    if("LAB-LOINC:48066-5"%in%colnames(dat.sub.wide)){
        dat.sub.wide[,"48065-7:48066-5"]=dat.sub.wide[,"48066-5"]
    }
    dat.sub.wide=dat.sub.wide[,setdiff(colnames(dat.sub.wide), c("48065-7", "48066-5"))]
    nm.lab=colnames(dat.sub.wide)[-1]
    code.dict.new2=suppressMessages(left_join(data.frame(concept_code=nm.lab), code.dict.new))
    code.dict.new2[which(code.dict.new2$concept_code=="48065-7:48066-5"),"labname"]="d_dimer"
    colnames(dat.sub.wide)=c(colnames(dat.sub.wide)[1], code.dict.new2$labname)
    rownames(dat.sub.wide)=NULL
    res=cbind(days_since_admission,dat.sub.wide)}else{res=NULL}
    res
}

data_med_clean=function(dat.x.raw, days){
    dat=dat.x.raw[dat.x.raw[,"concept_type"]%in%"MED-CLASS",]
    dat=dat[,c("patient_num","days_since_admission","concept_code")]
    if(days=="before_admission"){dat.sub=dat[which(dat$days_since_admission<= (-15) & dat$days_since_admission>= (-365)),]}
    if(days=="since_admission"){dat.sub=dat[which(dat$days_since_admission>=0),]}
    dat.sub=dat.sub[,setdiff(colnames(dat.sub),"days_since_admission")]
    dat.sub=dat.sub[duplicated(dat.sub)!=1,]
    dat.sub$value=1    
    dat.sub$days=days
    dat.sub<- spread(dat.sub, concept_code, value)
    dat.sub[is.na(dat.sub)]=0
  dat.sub
}

data_icd_clean=function(dat.x.raw, nm.icd.all, days){
    dat=dat.x.raw[dat.x.raw[,"concept_type"]%in%"DIAG-ICD10",]
    dat=dat[,c("patient_num","days_since_admission","concept_code")]
    if(days=="before_admission"){dat.sub0=dat[which(dat$days_since_admission<= (-15) & dat$days_since_admission>= (-365)),]}
    if(days=="since_admission"){dat.sub0=dat[which(dat$days_since_admission>=0),]}
    dat.sub0=dat.sub0[,setdiff(colnames(dat.sub0),"days_since_admission")]
    dat.sub0=dat.sub0[duplicated(dat.sub0)!=1,]
    dat.sub0$value=1    
    dat.sub0$days=days
    dat.sub0<- spread(dat.sub0, concept_code, value)
    dat.sub=data.frame(matrix(NA, nrow=dim(dat.sub0)[1], ncol=length(nm.icd.all)+1))
    colnames(dat.sub)=c("patient_num", nm.icd.all)
    dat.sub[,colnames(dat.sub0)]=dat.sub0
    dat.sub[is.na(dat.sub)]=0
    dat.sub
}

data_baseline_clean=function(code.dict, dat.surv.raw, dat.x.raw, dat.dem.raw){
    code.dict=apply(code.dict, 2, as.character)
    combine.set=c("48065-7","48066-5")
    combine.nm=paste(combine.set, collapse=":")
    code.dict=data.frame(rbind(code.dict, c(combine.nm, "D-dimer")))
    dat.lab=data_lab_clean(dat.x.raw, code.dict, days_since_admission=0)
    dat.med=data_med_clean(dat.x.raw, days="before_admission")
    nm.icd.all=sort(as.character(unique(dat.x.raw$concept_code[dat.x.raw$concept_type=="DIAG-ICD10"])))
    dat.icd=data_icd_clean(dat.x.raw, nm.icd.all, days="before_admission")
    dat.dem=data_dem_clean(dat.dem.raw, "patient_num", nm.gender="sex", nm.age="age_group", nm.race="race")
    dat.analysis=suppressMessages(left_join(dat.dem, dat.lab, by="patient_num"))
    dat.analysis=suppressMessages(left_join(dat.analysis, dat.med, by="patient_num"))
    dat.analysis=suppressMessages(left_join(dat.analysis, dat.icd, by="patient_num"))
    dat.analysis
}

data_event_clean=function(nm.event, dat.surv.raw, daymax=30){
    dat.surv.severe=data_surv_clean(dat.surv.raw, "patient_num", "days_since_admission", daymax, "severe")
    dat.surv.deceased=data_surv_clean(dat.surv.raw, "patient_num", "days_since_admission", daymax, "deceased")
    colnames(dat.surv.severe)=c("patient_num", "x_severe", "delta_severe")
    colnames(dat.surv.deceased)=c("patient_num", "x_deceased", "delta_deceased")
    dat.surv=suppressMessages(left_join(dat.surv.severe, dat.surv.deceased, by="patient_num"))
    dat.surv$x_severedeceased=pmin(dat.surv$x_severe,dat.surv$x_deceased)
    dat.surv$delta_severedeceased=ifelse((dat.surv$delta_severe+dat.surv$delta_deceased)!=0,1,0)
    calendar_at_admission=dat.surv.raw[dat.surv.raw$days_since_admission==0, c("patient_num", "calendar_date")]
    dat.surv=left_join(calendar_at_admission, dat.surv, by="patient_num")
    colnames(dat.surv)[2]="calendar_date_at_admission"
    dat.surv
}

data_analysis_clean=function(code.dict, nm.event, dat.surv.raw, dat.x.raw, dat.dem.raw, nm.patient_num, nm.days_since_admission, nm.value, patient.keep){
    code.dict=apply(code.dict, 2, as.character)
    combine.set=c("48065-7","48066-5")
    combine.nm=paste(combine.set, collapse=":")
    code.dict=data.frame(rbind(code.dict, c(combine.nm, "D-dimer")))
    dat.surv=data_surv_clean(dat.surv.raw, nm.patient_num, nm.days_since_admission, daymax=30, nm.event)
    dat.lab=data_lab_clean(dat.x.raw, code.dict, days_since_admission=0)
    dat.dem=data_dem_clean(dat.dem.raw, nm.patient_num, nm.gender="sex", nm.age="age_group", nm.race="race")
    dat.analysis=suppressMessages(left_join(dat.surv, dat.dem, by=nm.patient_num))
    dat.analysis=suppressMessages(left_join(dat.analysis, dat.lab, by=nm.patient_num))
    dat.analysis$age_group_new=dat.analysis$age_group
    dat.analysis$age_group_new[dat.analysis$age_group%in%c("00to02","03to05","06to11","12to17","18to25")]="00to25"
    dat.analysis$age_group_new <- factor(dat.analysis$age_group_new, levels = c("00to25","26to49","50to69","70to79","80plus"))
    dat.analysis$race_new=dat.analysis$race
    dat.analysis$race_new[dat.analysis$race%in%c("American_indian", "Hawaiian_pacific_islander", "Hispanic_latino", "Other")]="Hispanic and Other"
    dat.analysis$race_new <- factor(dat.analysis$race_new, levels = c("White", "Black", "Asian", "Hispanic and Other"))
    dat.analysis=dat.analysis[dat.analysis$patient_num%in%patient.keep,]
    dat.analysis
}

tab_compare_lab=function(myday, phase2.ClinicalCourse, phase2.PatientObservations, phase1.Labs){
  data(code.dict, package="FourCePhase2.1Data")
    code.dict=apply(code.dict, 2, as.character)
    code.dict.new=data.frame(code.dict)
    colnames(code.dict.new)[1]="concept_code"
    nm.lab=code.dict.new[,2]
    nm.lab=gsub(" ", "_", nm.lab)
    nm.lab=gsub("\\(", "", nm.lab)
    nm.lab=gsub("\\)", "", nm.lab)
    nm.lab=gsub("-", "_", nm.lab)
    code.dict.new[,2]=nm.lab
    dat=phase2.PatientObservations
    dat$concept_code=as.character(dat$concept_code)
    dat=suppressMessages(left_join(dat, code.dict.new, by="concept_code"))
    nm.lab.add=unique(dat[which(is.na(dat$labname)==1&dat$concept_type=="LAB-LOINC"),"concept_code"])
    
    patient_severe=phase2.ClinicalCourse[which(phase2.ClinicalCourse$severe==1),"patient_num"]
    nm.lab=sort(unique(dat$labname))
    res.p2=NULL
    for(nm in nm.lab){
        tmp_all=dat[which(dat$days_since_admission==myday & dat$labname==nm),]
        tmp_severe=dat[which(dat$days_since_admission==myday & dat$labname==nm & dat$patient_num%in%patient_severe),]
        loinc=unique(tmp_all[,"concept_code"])[1]
        if(dim(tmp_all)[1]!=0){
            #n_all=dim(tmp_all)[1]
            n_all=length(unique(tmp_all$patient_num))
            tmp_all2=tmp_all[,c("patient_num", "value")]
            tmp_all2 = data.frame(data.table(tmp_all2)[, lapply(.SD, mean), by=patient_num])
            mean_all=mean(tmp_all2[,"value"],na.rm=T)
            stdev_all=sd(tmp_all2[,"value"],na.rm=T)
            mean_log_all=mean(log(tmp_all2[,"value"]+0.5), na.rm=T)
            stdev_log_all=sd(log(tmp_all2[,"value"]+0.5),na.rm=T)
            
            n_severe=length(unique(tmp_severe$patient_num))
            tmp_severe2=tmp_severe[,c("patient_num", "value")]
            tmp_severe2=data.frame(data.table(tmp_severe2)[,lapply(.SD, mean), by=patient_num])
            mean_severe=mean(tmp_severe2[,"value"],na.rm=T)
            stdev_severe=sd(tmp_severe2[,"value"],na.rm=T)
            mean_log_severe=mean(log(tmp_severe2[,"value"]+0.5), na.rm=T)
            stdev_log_severe=sd(log(tmp_severe2[,"value"]+0.5),na.rm=T)
            
            res.p2=rbind(res.p2, cbind(nm, data.frame(loinc=loinc, n_all, mean_all, stdev_all, mean_log_all, stdev_log_all,
                                                      n_severe, mean_severe, stdev_severe, mean_log_severe, stdev_log_severe)))
        }
    }
    colnames(res.p2)[-c(1:2)]=paste0("p2.", colnames(res.p2)[-c(1:2)])
    colnames(res.p2)[1]="labname"
    
    res.p1=NULL
    nm.lab2=sort(unique(phase1.Labs$loinc))
    for(nm in nm.lab2){
        tmp=phase1.Labs[which(phase1.Labs$days_since_admission==myday & phase1.Labs$loinc==nm), ]
        if(dim(tmp)[1]!=0){
            n_all=tmp[, "num_patients_all"]
            mean_all=tmp[,"mean_value_all"]
            stdev_all=tmp[,"stdev_value_all"]
            mean_log_all=tmp[,"mean_log_value_all"]
            stdev_log_all=tmp[,"stdev_log_value_all"]
            n_severe=tmp[, "num_patients_ever_severe"]
            mean_severe=tmp[,"mean_value_ever_severe"]
            stdev_severe=tmp[,"stdev_value_ever_severe"]
            mean_log_severe=tmp[,"mean_log_value_ever_severe"]
            stdev_log_severe=tmp[,"stdev_log_value_ever_severe"]
            
            res.p1=rbind(res.p1, cbind(nm, data.frame(n_all,mean_all, stdev_all, mean_log_all, stdev_log_all,
                                                      n_severe,mean_severe, stdev_severe, mean_log_severe, stdev_log_severe)))
        }
    }
    nm.lab=res.p1[,1]
    nm.lab=gsub(" ", "_", nm.lab)
    nm.lab=gsub("\\(", "", nm.lab)
    nm.lab=gsub("\\)", "", nm.lab)
    res.p1[,1]=nm.lab
    colnames(res.p1)=paste0("p1.", colnames(res.p1))
    colnames(res.p1)[1]="loinc"
    res=suppressMessages(left_join(res.p1, res.p2, by="loinc"))
    res=res[,c("labname","loinc", 
               "p1.n_all","p2.n_all",
               "p1.mean_all", "p2.mean_all",
               "p1.stdev_all", "p2.stdev_all",
               "p1.mean_log_all",  "p2.mean_log_all",
               "p1.stdev_log_all", "p2.stdev_log_all",
               "p1.n_severe","p2.n_severe",
               "p1.mean_severe", "p2.mean_severe",
               "p1.stdev_severe", "p2.stdev_severe",
               "p1.mean_log_severe",  "p2.mean_log_severe",
               "p1.stdev_log_severe", "p2.stdev_log_severe"
    )]
    list(res=res, nm.lab.add=nm.lab.add)
}


tab_compare_med=function(phase2.PatientObservations, phase2.ClinicalCourse, phase1.Medications){
    dat=phase2.PatientObservations[phase2.PatientObservations$concept_type=="MED-CLASS",]
    dat$concept_code=as.character(dat$concept_code)
    nm.med.add=dat$concept_code[dat$concept_code%in%phase1.Medications$med_class!=1]
    patient_severe=phase2.ClinicalCourse[which(phase2.ClinicalCourse$severe==1),"patient_num"]
    nm.med=sort(unique(dat$concept_code))
    res.p2=NULL
    for(nm in nm.med){
        n_all_before1=length(unique(dat[which(dat$days_since_admission<= (-16)   & dat$days_since_admission>= -364 & dat$concept_code==nm),"patient_num"]))
        n_severe_before1=length(unique(dat[which(dat$days_since_admission<= (-16)   & dat$days_since_admission>= -364 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))
        
        n_all_before2=length(unique(dat[which(dat$days_since_admission<= (-14)   & dat$days_since_admission>= -366 & dat$concept_code==nm),"patient_num"]))
        n_severe_before2=length(unique(dat[which(dat$days_since_admission<= (-14)   & dat$days_since_admission>= -366 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))
        
        
        n_all_since1=length(unique(dat[which(dat$days_since_admission>0 & dat$concept_code==nm),"patient_num"]))
        n_severe_since1=length(unique(dat[which(dat$days_since_admission>0 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))
        
        n_all_since2=length(unique(dat[which(dat$days_since_admission>=0 & dat$concept_code==nm),"patient_num"]))
        n_severe_since2=length(unique(dat[which(dat$days_since_admission>=0 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))
        
        
        res.p2=rbind(res.p2, data.frame(medclass=nm,n_all_before1,n_all_before2, n_all_since1, n_all_since2, n_severe_before1, n_severe_before2, n_severe_since1, n_severe_since2))
    }
    colnames(res.p2)[-1]=paste0("p2.", colnames(res.p2)[-1])
    res.p1=NULL
    for(nm in nm.med){
        tmp=phase1.Medications[which(phase1.Medications$med_class==nm), ]
        res.p1=rbind(res.p1, data.frame(medclass=nm, 
                                        n_all_before=tmp["num_patients_all_before_admission"],
                                        n_all_since=tmp["num_patients_all_since_admission"],
                                        n_severe_before=tmp["num_patients_ever_severe_before_admission"],
                                        n_severe_since=tmp["num_patients_ever_severe_since_admission"]))
    }
    
    colnames(res.p1)=c("medclass", "p1.n_all_before", "p1.n_all_since", "p1.n_severe_before", "p1.n_severe_since")
    res=suppressMessages(left_join(res.p1, res.p2, by="medclass"))
    res=res[,c("medclass", 
               "p1.n_all_before","p2.n_all_before1","p2.n_all_before2",
               "p1.n_all_since","p2.n_all_since1","p2.n_all_since2",
               "p1.n_severe_before","p2.n_severe_before1","p2.n_severe_before2",
               "p1.n_severe_since","p2.n_severe_since1","p2.n_severe_since2"
    )]
    list(res=res, nm.med.add=nm.med.add)
}

tab_compare_diag=function(phase2.PatientObservations, phase2.ClinicalCourse, phase1.Diagnoses){
    dat=phase2.PatientObservations[phase2.PatientObservations$concept_type%in%c("DIAG-ICD10", "DIAG-ICD9"),]
    dat$concept_code=as.character(dat$concept_code)
    patient_severe=phase2.ClinicalCourse[which(phase2.ClinicalCourse$severe==1),"patient_num"]
    nm.diag=sort(unique(dat$concept_code))
    
    tmp.all.before1=dat[which(dat$days_since_admission<= (-16) & dat$days_since_admission>=-364),c("patient_num", "concept_code")]
    tmp.all.before1=tmp.all.before1[duplicated(tmp.all.before1)!=1,]
    n_all_before1=table(tmp.all.before1[,"concept_code"])
    n_all_before1=data.frame(n_all_before1)
    n_all_before1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_before1, by="Var1"))
    
    tmp.all.before2=dat[which(dat$days_since_admission<= (-14) & dat$days_since_admission>=-366),c("patient_num", "concept_code")]
    tmp.all.before2=tmp.all.before2[duplicated(tmp.all.before2)!=1,]
    n_all_before2=table(tmp.all.before2[,"concept_code"])
    n_all_before2=data.frame(n_all_before2)
    n_all_before2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_before2, by="Var1"))
    
    
    tmp.all.since1=dat[dat$days_since_admission>0,c("patient_num", "concept_code")]
    tmp.all.since1=tmp.all.since1[duplicated(tmp.all.since1)!=1,]
    n_all_since1=table(tmp.all.since1[,"concept_code"])
    n_all_since1=data.frame(n_all_since1)
    n_all_since1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_since1, by="Var1"))
    
    tmp.all.since2=dat[dat$days_since_admission>=0,c("patient_num", "concept_code")]
    tmp.all.since2=tmp.all.since2[duplicated(tmp.all.since2)!=1,]
    n_all_since2=table(tmp.all.since2[,"concept_code"])
    n_all_since2=data.frame(n_all_since2)
    n_all_since2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_since2, by="Var1"))
    
    tmp.severe.before1=dat[which(dat$days_since_admission<= (-16) & dat$days_since_admission>=-364 & dat$patient_num%in%patient_severe),c("patient_num", "concept_code")]
    tmp.severe.before1=tmp.severe.before1[duplicated(tmp.severe.before1)!=1,]
    n_severe_before1=table(tmp.severe.before1[,"concept_code"])
    n_severe_before1=data.frame(n_severe_before1)
    n_severe_before1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_before1, by="Var1"))
    
    tmp.severe.before2=dat[which(dat$days_since_admission<= (-14) & dat$days_since_admission>=-366 & dat$patient_num%in%patient_severe),c("patient_num", "concept_code")]
    tmp.severe.before2=tmp.severe.before2[duplicated(tmp.severe.before2)!=1,]
    n_severe_before2=table(tmp.severe.before2[,"concept_code"])
    n_severe_before2=data.frame(n_severe_before2)
    n_severe_before2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_before2, by="Var1"))
    
    
    tmp.severe.since1=dat[dat$days_since_admission>0 & dat$patient_num%in%patient_severe,c("patient_num", "concept_code")]
    tmp.severe.since1=tmp.severe.since1[duplicated(tmp.severe.since1)!=1,]
    n_severe_since1=table(tmp.severe.since1[,"concept_code"])
    n_severe_since1=data.frame(n_severe_since1)
    n_severe_since1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_since1, by="Var1"))
    
    tmp.severe.since2=dat[dat$days_since_admission>=0& dat$patient_num%in%patient_severe,c("patient_num", "concept_code")]
    tmp.severe.since2=tmp.severe.since2[duplicated(tmp.severe.since2)!=1,]
    n_severe_since2=table(tmp.severe.since2[,"concept_code"])
    n_severe_since2=data.frame(n_severe_since2)
    n_severe_since2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_since2, by="Var1"))
    
    
    res.p2=cbind(n_all_before1,n_all_before2[,2], n_all_since1[,2], n_all_since2[,2],n_severe_before1[,2],n_severe_before2[,2], n_severe_since1[,2], n_severe_since2[,2])
    colnames(res.p2)=c("diag-icd","p2.n_all_before1","p2.n_all_before2","p2.n_all_since1","p2.n_all_since2","p2.n_severe_before1","p2.n_severe_before2","p2.n_severe_since1","p2.n_severe_since2")
    res.p2[is.na(res.p2)]=0
    
    res.p1=phase1.Diagnoses[, -c(1,3)]
    colnames(res.p1)=c("diag-icd", "p1.n_all_before", "p1.n_all_since", "p1.n_severe_before", "p1.n_severe_since")
    res=suppressMessages(left_join(res.p1, res.p2, by="diag-icd"))
    res=res[,c("diag-icd", 
               "p1.n_all_before","p2.n_all_before1","p2.n_all_before2",
               "p1.n_all_since","p2.n_all_since1","p2.n_all_since2",
               "p1.n_severe_before","p2.n_severe_before1","p2.n_severe_before2",
               "p1.n_severe_since","p2.n_severe_since1","p2.n_severe_since2"
    )]
    res
}

tab_compare_dem=function(phase2.PatientSummary, phase2.PatientObservations, phase1.Demographics){
    dat.dem.raw=phase2.PatientSummary
    dat.dem=phase1.Demographics
    dat.dem.raw[,c("sex", "age_group", "race")]=apply(phase2.PatientSummary[,c("sex", "age_group", "race")],2, as.character)
    dat.dem[,c("sex", "age_group", "race")]=apply(phase1.Demographics[,c("sex", "age_group", "race")],2, as.character)
    
    tmp.all=dat.dem.raw[,c("patient_num", "sex", "age_group", "race")]
    tmp.severe=dat.dem.raw[which(dat.dem.raw$severe==1),c("patient_num", "sex", "age_group", "race")]
    
    mysettings=dat.dem[,c(2:4)]
    res.p2=NULL
    for(ii in 1:dim(mysettings)[1]){
        sex.i=mysettings[ii,"sex"]
        age_group.i=mysettings[ii,"age_group"]
        race.i=mysettings[ii,"race"]
        if(sex.i=="all"){sex.i=unique(dat.dem.raw[,"sex"])}
        if(age_group.i=="all"){age_group.i=unique(dat.dem.raw[,"age_group"])}
        if(race.i=="all"){race.i=unique(dat.dem.raw[,"race"])}
        
        n_all=length(unique(tmp.all[which(tmp.all[,"sex"]%in%sex.i &
                                              tmp.all[,"age_group"]%in%age_group.i &
                                              tmp.all[,"race"]%in%race.i),"patient_num"]))
        
        n_severe=length(unique(tmp.all[which(tmp.severe[,"sex"]%in%sex.i &
                                                 tmp.severe[,"age_group"]%in%age_group.i &
                                                 tmp.severe[,"race"]%in%race.i),"patient_num"]))   
        res.p2=rbind(res.p2,c(n_all, n_severe))
    }
    res=cbind(dat.dem[,-1], res.p2)
    colnames(res)[-(1:3)]=c("p1.n_all","p1.n_severe",  "p2.n_all", "p2.n_severe")
    res=res[,c("sex","age_group","race", "p1.n_all", "p2.n_all", "p1.n_severe", "p2.n_severe")]
    res
}

tab_compare_cc=function(phase2.ClinicalCourse, phase1.ClinicalCourse){
    res.p2=NULL
    patient_ever_severe=unique(phase2.ClinicalCourse[phase2.ClinicalCourse$severe==1,"patient_num"])
    for(myday in phase1.ClinicalCourse[,"days_since_admission"]){
        num_patients_all_still_in_hospital=length(unique(phase2.ClinicalCourse[which(phase2.ClinicalCourse[,"days_since_admission"]==myday & phase2.ClinicalCourse[,"in_hospital"]==1),"patient_num"]))
        num_patients_ever_severe_still_in_hospital=length(unique(phase2.ClinicalCourse[which(phase2.ClinicalCourse[,"days_since_admission"]==myday & phase2.ClinicalCourse[,"in_hospital"]==1 & phase2.ClinicalCourse[,"patient_num"]%in%patient_ever_severe==1),"patient_num"]))
        res.p2=rbind(res.p2,c(num_patients_all_still_in_hospital,num_patients_ever_severe_still_in_hospital))
    }
    res=cbind(phase1.ClinicalCourse[,-1], res.p2)
    colnames(res)[-1]=c("p1.num_patients_all_still_in_hospital","p1.num_patients_ever_severe_still_in_hospital",
                        "p2.num_patients_all_still_in_hospital","p2.num_patients_ever_severe_still_in_hospital")
    res=res[,c("days_since_admission",
               "p1.num_patients_all_still_in_hospital","p2.num_patients_all_still_in_hospital",
               "p1.num_patients_ever_severe_still_in_hospital","p2.num_patients_ever_severe_still_in_hospital")]
    res
}

tab_compare_dc=function(phase2.ClinicalCourse, phase1.DailyCounts){
    phase1.DailyCounts[,"calendar_date"]=as.character(phase1.DailyCounts[,"calendar_date"])
    res.p2=NULL
    for(mydate in phase1.DailyCounts[,"calendar_date"]){
        print(mydate)
        cumulative_patients_all=length(unique(phase2.ClinicalCourse[which(as.Date(as.character(phase2.ClinicalCourse[,"calendar_date"]),"%Y-%m-%d")<=as.Date(mydate, "%Y-%m-%d")),"patient_num"]))
        cumulative_patients_severe=length(unique(phase2.ClinicalCourse[which(as.Date(as.character(phase2.ClinicalCourse[,"calendar_date"]),"%Y-%m-%d")<=as.Date(mydate, "%Y-%m-%d") & phase2.ClinicalCourse[,"severe"]==1),"patient_num"]))
        cumulative_patients_dead=length(unique(phase2.ClinicalCourse[which(as.Date(as.character(phase2.ClinicalCourse[,"calendar_date"]),"%Y-%m-%d")<=as.Date(mydate, "%Y-%m-%d") & phase2.ClinicalCourse[,"deceased"]==1),"patient_num"]))
        num_patients_in_hospital_on_this_date=length(unique(phase2.ClinicalCourse[which(as.Date(as.character(phase2.ClinicalCourse[,"calendar_date"]),"%Y-%m-%d")==as.Date(mydate, "%Y-%m-%d") & phase2.ClinicalCourse[,"in_hospital"]==1),"patient_num"]))
        num_patients_in_hospital_and_severe_on_this_date=length(unique(phase2.ClinicalCourse[which(as.Date(as.character(phase2.ClinicalCourse[,"calendar_date"]),"%Y-%m-%d")==as.Date(mydate, "%Y-%m-%d") & phase2.ClinicalCourse[,"in_hospital"]==1 & phase2.ClinicalCourse[,"severe"]==1),"patient_num"]))
        res.p2=rbind(res.p2,c(cumulative_patients_all,cumulative_patients_severe,cumulative_patients_dead, num_patients_in_hospital_on_this_date,num_patients_in_hospital_and_severe_on_this_date))
    }
    res=cbind(dat.dc[,-1], res.p2)
    colnames(res)[-1]=c("p1.cumulative_patients_all","p1.cumulative_patients_severe",  "p1.cumulative_patients_dead", "p1.num_patients_in_hospital_on_this_date","p1.num_patients_in_hospital_and_severe_on_this_date", 
                        "p2.cumulative_patients_all","p2.cumulative_patients_severe",  "p2.cumulative_patients_dead", "p2.num_patients_in_hospital_on_this_date","p2.num_patients_in_hospital_and_severe_on_this_date")
    res=res[,c("calendar_date","p1.cumulative_patients_all","p2.cumulative_patients_all",
               "p1.cumulative_patients_severe","p2.cumulative_patients_severe",
               "p1.cumulative_patients_dead", "p2.cumulative_patients_dead", 
               "p1.num_patients_in_hospital_on_this_date","p2.num_patients_in_hospital_on_this_date",
               "p1.num_patients_in_hospital_and_severe_on_this_date","p2.num_patients_in_hospital_and_severe_on_this_date")]
    res
}

qc_site=function(phase1.DailyCounts, phase1.ClinicalCourse, phase1.Demographics,phase1.Diagnoses, phase1.Labs, phase1.Medications, site.nm){
  data(icd.list, package="FourCePhase2.1Data")
  data(lab.range, package="FourCePhase2.1Data")
    qc.dm=err_report_demographics_site(phase1.Demographics, site.nm)
    qc.cc=err_report_clinicalcourse_site(phase1.ClinicalCourse, site.nm)
    qc.dc=err_report_dailycounts_site(phase1.DailyCounts, site.nm)
    qc.crossover=err_report_crossover_site(phase1.ClinicalCourse, phase1.Demographics, phase1.DailyCounts, site.nm)
    qc.icd=err_report_diagnosis_site(phase1.Diagnoses, phase1.ClinicalCourse, phase1.Demographics, phase1.DailyCounts, icd.list, site.nm)
    qc.med=err_report_med_site(phase1.ClinicalCourse, phase1.Demographics, phase1.DailyCounts, phase1.Medications, site.nm)
    qc.lab=err_report_lab_site(phase1.ClinicalCourse, phase1.Demographics, phase1.DailyCounts, phase1.Labs, site.nm)
    qc.lab.unit=err_report_lab_unit_site(phase1.Labs, lab.range, site.nm)
    list(qc.dm=qc.dm, qc.cc=qc.cc, qc.dc=qc.dc, qc.crossover=qc.crossover, qc.icd=qc.icd, qc.med=qc.med, qc.lab=qc.lab, qc.lab.unit=qc.lab.unit)
}
err_report_demographics_site=function(dat.Demographics, site.nm){
    err.label=
        c("missing (sex,age,race)=all",
          "N_all<N_ever_severe",
          "negative N (not -999 or -99)",
          "sum of different sex groups not equal to sex='all')",
          "sum of different age groups not equal to age_group='all')",
          "sum of different race groups not equal to race='all')"
        )
    
    err=NULL
    dat.site=dat.Demographics
    colnames(dat.site)=tolower(colnames(dat.site))
    
    nm.check=c("sex", "age_group", "race")
    dat.check=dat.site[,nm.check]
    err=c(err,0%in%apply(dat.check,1, function(x) sum(x!="all"))!=1)
    
    id.nomiss=which(dat.site[,"num_patients_all"]>0 & dat.site[,"num_patients_ever_severe"]>0)
    err=c(err, any(dat.site[id.nomiss,"num_patients_all"]<dat.site[id.nomiss,"num_patients_ever_severe"]))
    
    dat.check=dat.site[,c("num_patients_all", "num_patients_ever_severe")]
    err=c(err, any(dat.check[dat.check<0]%in%c(-999,-99)!=1))
    
    dat.site$num_patients_all[dat.site$num_patients_all<0]=NA
    dat.site$num_patients_ever_severe[dat.site$num_patients_ever_severe<0]=NA
    
    tmp0=dat.site[which(dat.site$sex=="all" & dat.site$age_group=="all" & dat.site$race=="all"),]
    tmp1=dat.site[which(dat.site$sex!="all" & dat.site$age_group=="all" & dat.site$race=="all"),]
    tmp2=dat.site[which(dat.site$sex=="all" & dat.site$age_group!="all" & dat.site$race=="all"),]
    tmp3=dat.site[which(dat.site$sex=="all" & dat.site$age_group=="all" & dat.site$race!="all"),]
    
    for(iit in 1:3){
      if(iit==1){itmp=tmp1}
      if(iit==2){itmp=tmp2}
      if(iit==3){itmp=tmp3}
      num_patients_all=itmp[,"num_patients_all"]
      num_patients_ever_severe=itmp[,"num_patients_ever_severe"]
      ierr.all=ifelse(is.na(sum(num_patients_all)), FALSE, colSums(tmp0[,-c(1:4)])[1]!=sum(num_patients_all))
      ierr.ever_severe=ifelse(is.na(sum(num_patients_ever_severe)), FALSE, colSums(tmp0[,-c(1:4)])[2]!=sum(num_patients_ever_severe))
    err=c(err, any(c(ierr.all, ierr.ever_severe)))
    }
    report=data.frame(site.nm, label=err.label, err)
    
    err.report=report[report[,"err"]==T,c("site.nm", "label")]
    list(err.report=err.report, err.label=err.label)
}

err_report_clinicalcourse_site=function(dat.ClinicalCourse, site.nm){
    err.label=c(
        "no days0",
        "N_all at day 0 is not the largest",
        "N_all<N_ever_severe",
        "negative N (not -999 or -99)")
    
    err=NULL
    dat.site=dat.ClinicalCourse
    colnames(dat.site)=tolower(colnames(dat.site))
    
    err=c(err,0%in%dat.site[,"days_since_admission"]!=1)
    err=c(err,0%in%dat.site[which(dat.site[,"num_patients_all_still_in_hospital"]==max(dat.site[,"num_patients_all_still_in_hospital"])),"days_since_admission"]!=1)
    id.nomiss=which(dat.site[,"num_patients_all_still_in_hospital"]>0 & dat.site[,"num_patients_ever_severe_still_in_hospital"]>0)
    id3=which(dat.site[id.nomiss,"num_patients_all_still_in_hospital"]<dat.site[id.nomiss,"num_patients_ever_severe_still_in_hospital"])
    err=c(err,length(id3)>1)
    
    dat.check=dat.site[,c("num_patients_all_still_in_hospital", "num_patients_ever_severe_still_in_hospital")]
    id4=which(dat.check[dat.check<0]%in%c(-999,-99)!=1)
    err=c(err,length(id4)>1)
    report=data.frame(site.nm, label=err.label, err)
    
    err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]
    list(err.report=err.report, err.label=err.label)
}

err_report_dailycounts_site=function(dat.DailyCounts,site.nm){
    err.label=
        c("cumulative_patients_all is not largest in last date",
          "cumulative_patients_severe is not largest in last date",
          "cumulative_patients_dead is not largest in last date"
        )
    err=NULL
    dat.site=dat.DailyCounts
    colnames(dat.site)=tolower(colnames(dat.site))
    dat.site=dat.site[order(as.Date(as.character(dat.site$calendar_date),format='%Y-%m-%d')),]
    
    err=dim(dat.site)[1]%in%which(dat.site[,"cumulative_patients_all"]==max(dat.site[,"cumulative_patients_all"]))!=1
    err=c(err, dim(dat.site)[1]%in%which(dat.site[,"cumulative_patients_severe"]==max(dat.site[,"cumulative_patients_severe"]))!=1)
    err=c(err, dim(dat.site)[1]%in%which(dat.site[,"cumulative_patients_dead"]==max(dat.site[,"cumulative_patients_dead"]))!=1)
    
    report=data.frame(site.nm, label=err.label, err)
    
    err.report=report[report[,"err"]==T,c("site.nm", "label")]
    list(err.report=err.report, err.label=err.label)
}


err_report_crossover_site=function(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, site.nm){
    
    err.label=c(
        "missing ClinicalCourse or Demographics or DailyCounts",
        "N_all in the Demographics and DailyCounts not match",
        "N_all in the ClincalCourse and DailyCounts not match",
        "N_ever_severe in Demographics and DailyCounts not match",
        "N_ever_severe in ClinicalCourse and DailyCounts not match"
    )
    
    
    exist.cc=is.null(dat.ClinicalCourse)!=1
    exist.dm=is.null(dat.Demographics)!=1
    exist.dc=is.null(dat.DailyCounts)!=1
    
    if(exist.cc*exist.dm*exist.dc==0){err1=T; err2=F; err3=F; err4=F; err5=F}else{
        err1=F
        dat.site.cc=dat.ClinicalCourse
        colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
        dat.site.dm=dat.Demographics
        colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
        dat.site.dc=dat.DailyCounts
        colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
        
        
        n.dm=dat.site.dm[which(apply(dat.site.dm[,c("sex", "age_group", "race")],1, function(x) all(x=="all"))),c("num_patients_all")]
        n.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
        n.dc=max(dat.site.dc[,"cumulative_patients_all"])
        
        if(all(c(length(n.dm), length(n.dc))>0)){err2=n.dm!=n.dc}else{err2=FALSE}
        if(all(c(length(n.cc), length(n.dc))>0)){err3=n.cc!=n.dc}else{err3=FALSE}
        
        n.dm=dat.site.dm[which(apply(dat.site.dm[,c("sex", "age_group", "race")],1, function(x) all(x=="all"))),c("num_patients_ever_severe")]
        n.cc=max(dat.site.cc[,"num_patients_ever_severe_still_in_hospital"])
        n.dc=max(dat.site.dc[,"cumulative_patients_severe"])
        
        if(all(c(length(n.dm), length(n.dc))>0)){err4=n.dm!=n.dc}else{err4=FALSE}
        if(all(c(length(n.cc), length(n.dc))>0)){err5=n.cc!=n.dc}else{err5=FALSE}
    }
    err=c(err1, err2, err3, err4, err5)
    report=data.frame(site.nm, label=err.label, err)
    
    err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]
    list(err.report=err.report, err.label=err.label)
}

err_report_diagnosis_site=function(dat.Diagnoses, dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, icd.list, site.nm){
    icd.list0=icd.list
    err.label1="N_all_before > N_all at day0"
    err.label2="N_all_since > N_all at day0"
    err.label3= "N_all_before < N_ever_severe_before"
    err.label4= "N_all_since < N_ever_severe_since"
    err.label5= "negative N (not -999 or -99)"
    err.label6= "ICD not belong to dictionary"
    err.label7= "N_ever_severe<N_ever_severe_before with diagnosis"
    err.label8= "N_ever_severe<N_ever_severe_since with diagnosis"
    
    err.label=c(err.label1, err.label2, err.label3, err.label4, err.label5, err.label6, err.label7, err.label8)
    
    dat.site.cc=dat.ClinicalCourse
    colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
    dat.site.dm=dat.Demographics
    colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
    dat.site.dc=dat.DailyCounts
    colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
    dat.site=dat.Diagnoses
    colnames(dat.site)=tolower(colnames(dat.site))
    
    n.all.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
    n.all.dm=tryCatch(dat.site.dm[dat.site.dm$sex=="all"& dat.site.dm$race=="all" & dat.site.dm$age_group=="all", "num_patients_all"], error=function(e) NA)
    n.all.dc=max(dat.site.dc[, "cumulative_patients_all"])
    n.all=max(n.all.cc, n.all.dm, n.all.dc, na.rm=T)
    n.icd=max(dat.site[,"num_patients_all_before_admission"])
    err1=n.icd>n.all
    
    n.icd=max(dat.site[,"num_patients_all_since_admission"])
    err2=n.icd>n.all
    
    id.nomiss=which(dat.site[,"num_patients_all_before_admission"]>0 & dat.site[,"num_patients_ever_severe_before_admission"]>0)
    err3=any(dat.site[id.nomiss,"num_patients_all_before_admission"]<dat.site[id.nomiss,"num_patients_ever_severe_before_admission"])
    
    id.nomiss=which(dat.site[,"num_patients_all_since_admission"]>0 & dat.site[,"num_patients_ever_severe_since_admission"]>0)
    err4=any(dat.site[id.nomiss,"num_patients_all_since_admission"]<dat.site[id.nomiss,"num_patients_ever_since_before_admission"])
    
    dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid", "icd_code_3chars", "icd_version"))]
    err5=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)
    
    icd.list=unique(as.character(dat.site[,"icd_code_3chars"]))
    err6=paste(icd.list[icd.list%in%icd.list0!=1],collapse=";")
    
    err7=any(dat.site[id.nomiss,"num_patients_ever_severe_before_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))
    note7=dat.site[id.nomiss[which(dat.site[id.nomiss,"num_patients_ever_severe_before_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))],c("icd_code_3chars","num_patients_ever_severe_since_admission")]
    err8=any(dat.site[id.nomiss,"num_patients_ever_severe_since_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))
    note8=dat.site[id.nomiss[which(dat.site[id.nomiss,"num_patients_ever_severe_since_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))],c("icd_code_3chars","num_patients_ever_severe_since_admission")]
    
    err=c(err1, err2, err3, err4, err5, err6, err7, err8)
    err.label[6]=paste0("ICD not in dictionary:", err6)
    if(dim(note7)[1]!=0){
        err.label[7]=paste0(paste(note7, collapse=";"), "")}
    
    if(dim(note8)[1]!=0){
        err.label[8]=paste0(err.label[8],":",max(dat.site.dc[, "cumulative_patients_severe"])," vs. ", note8[2], "(", note8[1], ")")}
    report=data.frame(site.nm, label=err.label, err)
    
    err.report=report[as.character(report[,"err"])%in%c(FALSE,"")!=1,c("site.nm", "label")]
    list(err.report=err.report, err.label=err.label)
}

err_report_lab_site=function(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, dat.Labs, site.nm){
    err.label=c(
        "N_all > N_all at day0",
        "N_all < N_ever_severe",
        "negative N (not -999 or -99)",
        "day 0+ not included",
        "Inf or -Inf",
        "N_ever_severe (in DailyCount)<N_ever_severe (in Labs) for lab")
    
    dat.site.cc=dat.ClinicalCourse
    colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
    dat.site.dm=dat.Demographics
    colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
    dat.site.dc=dat.DailyCounts
    colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
    dat.site=dat.Labs
    colnames(dat.site)=tolower(colnames(dat.site))
    
    n.all.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
    n.all.dm=tryCatch(dat.site.dm[dat.site.dm$sex=="all"& dat.site.dm$race=="all" & dat.site.dm$age_group=="all", "num_patients_all"], error=function(e) NA)
    n.all.dc=max(dat.site.dc[, "cumulative_patients_all"])
    n.all=max(n.all.cc, n.all.dm, n.all.dc, na.rm=T)
    
    n.lab=max(dat.site[dat.site$days_since_admission==0,"num_patients_all"])
    err1=n.lab>n.all
    
    id.nomiss=which(dat.site[,"num_patients_all"]>0 & dat.site[,"num_patients_ever_severe"]>0)
    err2=any(dat.site[id.nomiss,"num_patients_all"]<dat.site[id.nomiss,"num_patients_ever_severe"])
    
    dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid", "loinc", "units", "days_since_admission", colnames(dat.site)[grepl("log",colnames(dat.site))]))]
    err3=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)
    
    err4=sum(dat.site[,"days_since_admission"]>0)<1
    
    err5=any(dat.site%in%c(Inf, -Inf))
    err6=any(dat.site[id.nomiss,"num_patients_ever_severe"]>max(dat.site.dc[, "cumulative_patients_severe"]))
    if(err6==T){
        label6=paste(err.label[6],paste(as.character(dat.site[id.nomiss[which(dat.site[id.nomiss,"num_patients_ever_severe"]>max(dat.site.dc[, "cumulative_patients_severe"]))],"loinc"]),collapse = ";"),sep=" ")
        err.label[6]=label6
    }
    
    err=c(err1, err2, err3, err4, err5, err6)
    report=data.frame(site.nm, label=err.label, err)
    
    err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]
    list(err.report=err.report, err.label=err.label)
}

err_report_med_site=function(dat.ClinicalCourse, dat.Demographics, dat.DailyCounts, dat.Medications, site.nm){
    err.label1="N_all_before > N_all at day0"
    err.label2="N_all_since > N_all at day0"
    err.label3= "N_all_before < N_ever_severe_before"
    err.label4= "N_all_since < N_ever_severe_since"
    err.label5= "negative N (not -999 or -99)"
    err.label6= "N_ever_severe<N_ever_severe_before with med"
    err.label7= "N_ever_severe<N_ever_severe_since with med"
    err.label8= "no data"
    
    err.label=c(err.label1, err.label2, err.label3, err.label4, err.label5, err.label6, err.label7, err.label8)
    if(is.null(dat.Medications)==1){
        err.report=data.frame(site.nm=site.nm, label="no input data file for medication")}else{
            dat.site.cc=dat.ClinicalCourse
            colnames(dat.site.cc)=tolower(colnames(dat.site.cc))
            dat.site.dm=dat.Demographics
            colnames(dat.site.dm)=tolower(colnames(dat.site.dm))
            dat.site.dc=dat.DailyCounts
            colnames(dat.site.dc)=tolower(colnames(dat.site.dc))
            dat.site=dat.Medications
            colnames(dat.site)=tolower(colnames(dat.site))
            
            if(dim(dat.site)[1]!=0){
                err8=FALSE
                n.all.cc=dat.site.cc[dat.site.cc$days_since_admission==0,"num_patients_all_still_in_hospital"]
                n.all.dm=tryCatch(dat.site.dm[dat.site.dm$sex=="all"& dat.site.dm$race=="all" & dat.site.dm$age_group=="all", "num_patients_all"], error=function(e) NA)
                n.all.dc=max(dat.site.dc[, "cumulative_patients_all"])
                n.all=max(n.all.cc, n.all.dm, n.all.dc, na.rm=T)
                n.med=max(dat.site[,"num_patients_all_before_admission"])
                err1=n.med>n.all
                
                n.med=max(dat.site[,"num_patients_all_since_admission"])
                err2=n.med>n.all
                
                id.nomiss=which(dat.site[,"num_patients_all_before_admission"]>0 & dat.site[,"num_patients_ever_severe_before_admission"]>0)
                err3=any(dat.site[id.nomiss,"num_patients_all_before_admission"]<dat.site[id.nomiss,"num_patients_ever_severe_before_admission"])
                
                id.nomiss=which(dat.site[,"num_patients_all_since_admission"]>0 & dat.site[,"num_patients_ever_severe_since_admission"]>0)
                err4=any(dat.site[id.nomiss,"num_patients_all_since_admission"]<dat.site[id.nomiss,"num_patients_ever_since_before_admission"])
                
                dat.check=dat.site[,setdiff(colnames(dat.site), c("siteid", "med_class"))]
                err5=any(unique(dat.check[dat.check<0])%in%c(-99, -999)!=1)
                
                err6=any(dat.site[id.nomiss,"num_patients_ever_severe_before_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))
                err7=any(dat.site[id.nomiss,"num_patients_ever_severe_since_admission"]>max(dat.site.dc[, "cumulative_patients_severe"]))
                
                err=c(err1, err2, err3, err4, err5, err6, err7, err8)}else{
                    err1=err2=err3=err4=err5=err6=err7=FALSE; err8=TRUE
                    err=c(err1, err2, err3, err4, err5, err6, err7, err8)
                }
            report=data.frame(site.nm, label=err.label, err)
            
            err.report=report[report[,"err"]==TRUE,c("site.nm", "label")]}
    list(err.report=err.report, err.label=err.label)
}


err_report_lab_unit_site=function(dat.Labs, lab.range, site.nm){
    dat=dat.Labs
    colnames(dat)=tolower(colnames(dat))
    
    dat$siteid=toupper(dat$siteid)
    nm.day="days_since_admission"
    comb.lab=dat
    comb.lab$loinc=trimws(comb.lab$loinc, which = c("both", "left", "right"))
    nm.lab.all = unique(comb.lab$loinc)
    nm.lab.all.new=gsub("-",".",nm.lab.all)
    
    lab.range.nm=colnames(lab.range)
    lab.range.nm=gsub("loinc|_LB|_UB", "", lab.range.nm)
    nm.lab.all.new=nm.lab.all.new[nm.lab.all.new%in%lab.range.nm]
    nm.lab.all=nm.lab.all.new
    comb.lab=comb.lab[which(comb.lab$days_since_admission%in%c(0:30)),]
    err.report=NULL
    for(nm.lab in nm.lab.all){
        tmp=comb.lab[comb.lab$loinc%in%nm.lab,c("days_since_admission","mean_log_value_all")]
        tmp$mean_log_value_all[tmp$mean_log_value_all%in%c(-99,-999, -Inf, Inf)]=NA
        tmp=tmp[which(is.na(tmp$mean_log_value_all)!=1),]
        if(dim(tmp)[1]>=5){
            tmp.range=lab.range[,c(1,which(grepl(gsub("-",".",nm.lab),colnames(lab.range))==1))]
            colnames(tmp.range)[2:3]=c("LB", "UB")
            tmp=suppressMessages(left_join(tmp, tmp.range, by="days_since_admission"))
            err.tmp=1*(sum(tmp$mean_log_value_all<tmp$LB)>(0.9*dim(tmp)[1])|sum(tmp$mean_log_value_all>tmp$UB)>(0.9*dim(tmp)[1]))
            err.tmp=c(site.nm, err.tmp,paste0("lab unit issue for ", nm.lab))
            err.report=rbind(err.report, err.tmp)}
    }
    if(is.null(err.report)!=1){
        err.report=data.frame(err.report)}else{err.report=data.frame(matrix(NA,1,3))}
    colnames(err.report)=c(site.nm, "err","label")
    err.report=err.report[err.report[,"err"]==1,c(site.nm, "label")]
    list(err.report=err.report)
}





