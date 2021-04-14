rm(list=ls())
devtools::install_github("https://github.com/covidclinical/Phase2.1SurvivalRPackage", subdir="FourCePhase2.1Survival", upgrade=FALSE, ref="ChuanHong-testing")
library(FourCePhase2.1Data)
currSiteId = FourCePhase2.1Data::getSiteId()
dir.input="/Users/chuanhong/Documents/Input"
runQC_nodocker(currSiteId, dir.output)

