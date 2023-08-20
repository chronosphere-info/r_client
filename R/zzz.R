#' Evolving Earth System Variables
#' 
#' The implemented functions allow the query, download, and import of remotely-stored and version-controlled data items. The inherent meta-database maps data files and import code to programming classes and allows access to these items via files deposited in public repositories. The purpose of the project is to increase reproducibility and establish version tracking of results from (paleo)environmental/ecological research.
#'  
#' This is still a Release Candidate version. As is R, this is free software and comes with ABSOLUTELY NO WARRANTY. Nevertheless, notes about found bugs and suggestions are more than welcome. The chronosphere links to data and code deposited on public servers, which can be inspected using the code and data URLs of the items (\code{codeURL}, \code{primaryURL} and \code{secondaryURL} in the result of \code{datasets(master=TRUE)}). By using this package you agree that you take responsibility for the items that you download. Never run \code{fetch()} as a superuser or with administrative privileges! Doing this can open up a security hole to be exploited in case the remote servers get hijacked. 
#'
#' @author Adam T. Kocsis (adam.t.kocsis@gmail.com)
#' @docType package
#' @name chronosphere
"_PACKAGE"

#' @importFrom utils read.csv download.file unzip flush.console sessionInfo
#' @importFrom utils assignInNamespace
#' @importFrom utils browseURL
#' @importFrom tools md5sum
NULL


.onAttach<-function(libpath, pkgname){
	packageStartupMessage(
	"Chronosphere - Evolving Earth System Variables\nImportant: never fetch data as a superuser / with admin. privileges!\n
Note that the package was split for efficient maintenance and development:\n - Plate tectonic calculations -> package 'rgplates'\n - Arrays of raster and vector spatials -> package 'via'")
}
