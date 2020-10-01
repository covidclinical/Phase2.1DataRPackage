## This file contains functions related to file system locations where the site-specific 
## data will reside.

#' Returns the name of the root of the file system location to be used as workspace.
#'
#' @keywords 4CE
#' @export

get4ceRootDirectoryName <- function() {
    return("/4ceData")
}

#' Returns the name of the directory where site input data is expected to reside.
#'
#' @param fullyQualified Should the return value be a fully-qualified path?  If not, just the name of the directory is returned.
#' @keywords 4CE
#' @export

getInputDataDirectoryName <- function(fullyQualified=TRUE) {

    if (fullyQualified) {
        return(
            file.path(
                get4ceRootDirectoryName(), 
                getInputDataDirectoryName(fullyQualified=FALSE)
            )
        )
    } else {
        ## the relative path is hardcoded
        return("Input")
    }
}

getOutputDataDirectoryName <- function(fullyQualified=TRUE) {

    if (fullyQualified) {
        return(
            file.path(
                get4ceRootDirectoryName(), 
                getInputDataDirectoryName(fullyQualified=FALSE)
            )
        )
    } else {
        ## the relative path is hardcoded
        return("Output")
    }
}
#' Returns the name of the site whose files are in the directory returned by getInputDataDirectoryName().
#' Throws an error if files are inconsistently named, or if files for multiple site ids are present,
#' or if required files are missing.
#'
#' @keywords 4CE
#' @export

getSiteId <- function() {

    ## enumerate files in the input data directory
    fNames = list.files(getInputDataDirectoryName())

    ## we are supposed to find a single match to each of these file name patterns
    fNamePatterns = c(
        "Labs-.+\\.csv",
        "Medications-.+\\.csv",
        "Diagnoses-.+\\.csv",
        "Demographics-.+\\.csv",
        "DailyCounts-.+\\.csv",
        "ClinicalCourse-.+\\.csv"
    )

    siteId = NA

    ## for each file pattern we are supposed to match
    for (currFNamePattern in fNamePatterns) {

        ix = grep(pattern=currFNamePattern, x=fNames)

        ## if no match, stop
        if (length(ix) == 0) {
            stop("No match found for file name pattern: ", currFNamePattern)
        }

        ## if multiple matches, stop
        if (length(ix) > 1) {
            stop("Multiple matches found for file name pattern: ", currFNamePattern, ": ", paste(fNames[ix], collapse=", "))
        }

        ## so we have exactly one match, make sure all files are named for the same site ID
        prefixSuffix = strsplit(x=currFNamePattern, split=".+\\", fixed=TRUE)[[1]]
        currSiteId = 
            gsub(
                x = gsub(
                    x = fNames[ix],
                    pattern=prefixSuffix[1],
                    replacement=""
                ),
                pattern=prefixSuffix[2],
                replacement=""
            )
        
        if (is.na(siteId)) {
            siteId = currSiteId
        }

        if (siteId != currSiteId) {
            stop("Files for multiple sites found in the input data directory: ", siteId, ", ", currSiteId)
        }
    }

    return(siteId)
}
