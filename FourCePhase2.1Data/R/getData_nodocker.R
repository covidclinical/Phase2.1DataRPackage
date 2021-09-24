#' Import LocaLPatientClinicalCourse.csv
#' 
#' Uses [get4ceFile()] - see that file for additional customization.
#' 
#' @param siteid Name of your site.
#' @param dir.input Input directory
#' @param ... (Optional) Additional arguments passed to [get4ceFile()].
#
#' @seealso [get4ceFile()]
#' @export
getLocalPatientClinicalCourse_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile("LocalPatientClinicalCourse.csv", dir.input, ...)
}


#' Import LocaLPatientObservations.csv
#' @inherit getLocalPatientClinicalCourse_nodocker
#' @export
getLocalPatientObservations_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile("LocalPatientObservations.csv", dir.input, ...)
}

#' Import LocaLPatientSummary.csv
#' @inherit getLocalPatientClinicalCourse_nodocker
#' @export
getLocalPatientSummary_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile("LocalPatientSummary.csv", dir.input, ...)
}

#' Import LocaLPatientSummary.csv
#' @inherit getLocalPatientClinicalCourse_nodocker
#' @export
getLocalPatientMapping_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile("LocalPatientMapping.csv", dir.input, ...)
}

#' Import Labs-yoursite.csv
#' 
#' Uses [get4ceFile()] - see that file for additional customization.
#' 
#' @param siteid Name of your site.
#' @param dir.input Input directory
#' @param ... (Optional) Additional arguments passed to [get4ceFile()].
#
#' @seealso [get4ceFile()]
#' @export
getLabs_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile(paste0("Labs-", siteid,".csv"), dir.input, tolower = FALSE, ...)
}


#' Import Medications-yoursite.csv
#' @inherit getLabs_nodocker
#' @export
getMedications_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile(paste0("Medications-", siteid,".csv"), dir.input, tolower = FALSE, ...)
}

#' Import Diagnoses-yoursite.csv
#' @inherit getLabs_nodocker
#' @export
getDiagnoses_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile(paste0("Diagnoses-", siteid,".csv"), dir.input, tolower = FALSE, ...)
}

#' Import Demographics-yoursite.csv
#' @inherit getLabs_nodocker
#' @export
getDemographics_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile(paste0("Demographics-", siteid,".csv"), dir.input, tolower = FALSE, ...)
}

#' Import DailyCounts-yoursite.csv
#' @inherit getLabs_nodocker
#' @export
getDailyCounts_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile(paste0("DailyCounts-", siteid,".csv"), dir.input, tolower = FALSE, ...)
}

#' Import ClinicalCourse-yoursite.csv
#' @inherit getLabs_nodocker
#' @export
getClinicalCourse_nodocker <- function(siteid, dir.input, ...) {
  get4ceFile(paste0("ClinicalCourse-", siteid,".csv"), dir.input, tolower = FALSE, ...)
}

