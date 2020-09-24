
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


#' Names of one-dimensional \code{\link{RasterArray}}, \code{\link{SpatialStack}} or \code{\link{SpatialArray}} objects.
#' 
#' Get or set the names of one-dimensional \code{\link{RasterArray}} or \code{\link{SpatialArray}} objects 
#' @param x \code{\link{RasterArray}}, \code{\link{SpatialStack}} or \code{\link{SpatialArray}} object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of names or \code{NULL}.
#' 
#' @examples
#' data(dems)
#' names(dems)
#' names(dems)[4] <- "weirdo"
#' # NULL
#' data(clim)
#' names(clim)
#' @rdname names
#' @exportMethod names
setMethod(
	"names",
	signature="XArray",
	function(x){	
		names(x@index)
	}
)

#' @rdname names
#' @exportMethod "names<-"
setReplaceMethod(
	"names",
	signature="XArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(is.null(names(x))) names(x@index) <- rep(NA, length(x@index))
		names(x@index) <- value 
		return(x)
})


#' Number of elements or layers in a \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object
#' 
#' Function to return the length of the array in which \code{RasterLayers} are organized.
#' 
#' The \code{length()} function returns the number elements that should be present based on the array structure itself, and not the total number of values stored in the object (such as the \code{length} method of \code{RasterStack}s). As the object can contain missing values, the number of actual layers can be queried with \code{\link{nlayers}}. 
#' 
#' @param x a \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object.
#' @return A \code{numeric} value. 
#' @examples
#' data(dems)
#' # omit third element
#' dems[3] <- NA
#' # number of elements in the RasterArray
#' length(dems)
#' # remaining number values in the stack 
#' length(dems@stack)
#' # the number of remaining layers in the RasterArray
#' nlayers(dems)
#' 
#' @rdname arraylength
#' @exportMethod length
setMethod(
	"length",
	signature="XArray",
	function(x) length(x@index)
)

#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="XArray",
	function(x) nlayers(x@stack)
)

