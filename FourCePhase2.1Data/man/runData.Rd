\name{runData}
\alias{runData}
\title{Data pivot}
\description{
Create data pivot for Phase2.1 analysis}
\usage{
runData(phase2.ClinicalCourse, phase2.PatientObservations, phase2.PatientSummary, data.type, output.dir)

}
\arguments{
  \item{phase2.ClinicalCourse}{Phase2.1 data for ClinicalCourse}
  \item{phase2.PatientObservations}{Phase2.1 data for PatientObservations}
  \item{phase2.PatientSummary}{Phase2.1 data for PatientSummary}
  \item{data.type}{The type of pivot data; can be chosen from "Labs_Longitudinal","Medications_Longitudinal","Diagnoses_Longitudina","Covariates_Baseline","EventTime"}
  \item{output.dir}{directory of the output}
}

\author{Chuan Hong}

\examples{
devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", upgrade=FALSE)
library(FourCePhase2.1Data)

setwd("/4ceData/RDevelopment")
input.dir = "Input/" 
output.dir = "Output/" 

siteid="SITE001" ##replace with your siteid
phase1.Labs=read.csv(paste0(input.dir, "Labs-", siteid,".csv"))
phase1.Medications=read.csv(paste0(input.dir, "Medications-", siteid,".csv"))
phase1.Diagnoses=read.csv(paste0(input.dir, "Diagnoses-", siteid,".csv"))
phase1.Demographics=read.csv(paste0(input.dir, "Demographics-", siteid,".csv"))
phase1.DailyCounts=read.csv(paste0(input.dir, "DailyCounts-", siteid,".csv"))
phase1.ClinicalCourse=read.csv(paste0(input.dir, "ClinicalCourse-", siteid,".csv"))

phase2.ClinicalCourse=read.csv(paste0(input.dir, "Phase2LocalPatientClinicalCourse.csv"))
phase2.PatientObservations=read.csv(paste0(input.dir, "Phase2LocalPatientObservations.csv"))
phase2.PatientSummary=read.csv(paste0(input.dir, "Phase2LocalPatientSummary.csv"))

runData(phase2.ClinicalCourse, phase2.PatientObservations, phase2.PatientSummary, data.type="Labs_Longitudinal", output.dir)
}

\keyword{quality control}
