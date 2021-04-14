rm(list=ls())
devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", upgrade=FALSE)
library(FourCePhase2.1Data)
currSiteId = "MGB"
dir.input="/Users/chuanhong/Documents/Input"
runQC_nodocker(currSiteId, dir.input)

