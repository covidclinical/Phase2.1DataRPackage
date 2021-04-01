
#' Obtain obfuscation level for the site
#'
#' @keywords 4CE Phase2 Project
#' @export

getObfuscation <- function(siteid){
  data(site.country.obfuscation, package="FourCePhase2.1Data")
  site.country.obfuscation[site.country.obfuscation$SiteID==siteid,"Obfuscation"]
}
  

