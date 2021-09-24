
#' Import a 4CE CSV file
#' 
#' Files are loaded from the input data directory, optionally using [data.table::fread()].
#' 
#' @param filename Name of the file, such as LocalPatientSummary.csv
#' @param dir.input Input directory, which is via [getInputDataDirectoryName()]
#'   if using docker.
#' @param fread if TRUE, use data.table::fread() for faster import. Otherwise
#'   use read.csv(). This will also check the 4ce.fread option to make it work
#'   without passing a function argument.
#' @param tolower Lowercase column names? (default: yes)
#' @param ... (Optional) Additional arguments passed through to fread() or
#'   read.csv()
#'
#' @seealso [data.table::fread()], [utils::read.csv()]
#' 
#' @export
#' @importFrom utils read.csv
get4ceFile <-
  function(filename,
           dir.input = getInputDataDirectoryName(),
           fread = getOption("4ce.fread", FALSE),
           tolower = TRUE,
           ...) {
  
  filepath = file.path(dir.input, filename)
  
  if (fread) {
    df = data.table::fread(filepath, data.table = FALSE, ...)
  } else {
    df = read.csv(filepath, ...)
  }
  
  if (tolower) {
    colnames(df) = tolower(colnames(df))
  }
  
  return(df)
}

#' Import LocalPatientClinicalCourse.csv
#' 
#' Uses [get4ceFile()] - see that file for additional customization.
#' 
#' @param siteid (unused)
#' @param ... (Optional) Additional arguments passed to [get4ceFile()].
#'   
#' @seealso [get4ceFile()]
#' @export
getLocalPatientClinicalCourse <- function(siteid = NULL, ...) {
  get4ceFile("LocalPatientClinicalCourse.csv", ...)
}

#' Import LocalPatientObservations.csv
#' @inherit getLocalPatientClinicalCourse
#' @export
getLocalPatientObservations <- function(siteid = NULL, ...) {
  get4ceFile("LocalPatientObservations.csv", ...)
}

#' Import LocalPatientSummary.csv
#' @inherit getLocalPatientClinicalCourse
#' @export
getLocalPatientSummary <- function(siteid = NULL, ...) {
  get4ceFile("LocalPatientSummary.csv", ...)
}

#' Import LocalPatientMapping.csv
#' @inherit getLocalPatientClinicalCourse
#' @export
getLocalPatientMapping<- function(siteid = NULL, ...) {
  get4ceFile("LocalPatientMapping.csv", ...)
}

#' Import Labs-yoursite.csv
#' 
#' Uses [get4ceFile()] - see that file for additional customization.
#' 
#' @param siteid Name of your site.
#' @param ... (Optional) Additional arguments passed to [get4ceFile()].
#
#' @seealso [get4ceFile()]
#' @export
getLabs<- function(siteid, ...) {
  get4ceFile(paste0("Labs-", siteid, ".csv"), tolower = FALSE, ...)
}

#' Import Medications-yoursite.csv
#' @inherit getLabs
#' @export
getMedications<- function(siteid, ...) {
  get4ceFile(paste0("Medications-", siteid, ".csv"), tolower = FALSE, ...)
}

#' Import Diagnoses-yoursite.csv
#' @inheritParams getLabs
#' @export
getDiagnoses<- function(siteid, ...) {
  get4ceFile(paste0("Diagnoses-", siteid, ".csv"), tolower = FALSE, ...)
}

#' Import Demographics-yoursite.csv
#' @inheritParams getLabs
#' @export
getDemographics<- function(siteid, ...) {
  get4ceFile(paste0("Demographics-", siteid, ".csv"), tolower = FALSE, ...)
}

#' Import DailyCounts-yoursite.csv
#' @inheritParams getLabs
#' @export
getDailyCounts<- function(siteid, ...) {
  get4ceFile(paste0("DailyCounts-", siteid, ".csv"), tolower = FALSE, ...)
}

#' Import ClinicalCourse-yoursite.csv
#' @inheritParams getLabs
#' @export
getClinicalCourse<- function(siteid, ...) {
  get4ceFile(paste0("ClinicalCourse-", siteid, ".csv"), tolower = FALSE, ...)
}

