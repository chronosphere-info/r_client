
#' Dimensions of \code{\link{SpatialArray}} or \code{\link{RasterArray}} objects
#' 
#' The function returns the dimensions of the array in which \code{RasterLayer}s or \code{Spatial*} objects are organized.
#' @param x A \code{\link{SpatialArray}} or \code{\link{RasterArray}} class object.
#' @return A \code{numeric} vector.
#' 
#' @examples
#' data(dems)
#' dim(dems)
#' data(clim)
#' dim(clim)
#' @exportMethod dim
setMethod(
	"dim", 
	signature="XArray", 
	function(x){
		proxyDim <- dim(x@index)
		if(is.null(proxyDim)) proxyDim <- length(x@index)
		proxyDim
	} 
)
