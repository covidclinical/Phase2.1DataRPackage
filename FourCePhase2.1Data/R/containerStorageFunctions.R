#' Returns the name of the directory in the container that packages can use for 
#' temporary storage (e.g., to persist analysis results between calculation and 
#' upload).
#'
#' @param fullyQualified Should the return value be a fully-qualified path?  If not, just the name of the directory is returned.
#' @keywords 4CE
#' @export

getContainerScratchDirectory <- function() {

    ## for now, use the home directory of the user who is running
    return("~/")
}