#' Earth System History Variables
#'
#' The purpose of the 'chronosphere' project is to facilitate spatially explicit analyses of (paleo)environmental/ecological research. The package serves as a gateway to plate tectonic reconstructions, deep time global climate model results as well as fossil occurrence datasets such as the Paleobiology Database \url{https://paleobiodb.org/} and the PaleoReefs Database \url{https://www.paleo-reefs.pal.uni-erlangen.de/}. Environmental data stored on a remote server can be downloaded and imported directly to the R environment. Query functions to the GPlates (\url{https://www.gplates.org/}) desktop application or the GPlates Web Service (\url{https://gws.gplates.org/}) allow users to reconstruct coordinates, static plates, and Spatial objects. A wrapper class \code{\link{RasterArray}} is implemented around the \code{\link[raster:raster]{RasterLayer}} class, allowing the organization of spatially explicit raster data in \code{n}-dimensional arrays. The project is developed under the umbrella of the DFG (Deutsche Forschungsgemeinschaft) Research Unit TERSANE2 (For 2332, TEmperature Related Stressors in ANcient Extinctions).
#' 
#' This is still the Beta version. As is R, this is free software and comes with ABSOLUTELY NO WARRANTY. Nevertheless, notes about found bugs and suggestions are more than welcome. 
#'
#' @author Adam T. Kocsis (adam.t.kocsis@gmail.com) and Nussaibah B. Raja
#' @docType package
#' @name chronosphere
NULL

#' @import raster
#' @import sp
#' @importFrom methods new cbind2 rbind2
#' @importFrom utils read.csv download.file unzip flush.console sessionInfo
#' @importFrom graphics par layout mtext
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom grDevices devAskNewPage
#' @importFrom utils assignInNamespace
#' @importFrom utils browseURL
NULL

