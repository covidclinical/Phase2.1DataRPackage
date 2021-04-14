rm(list=ls())
devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", upgrade=FALSE)
currSiteId = FourCePhase2.1Data::getSiteId()
FourCePhase2.1Data::runQC(currSiteId)

