
#' Dimensions of \code{\link{SpatialArray}} or \code{\link{RasterArray}} objects
#' 
#' The function returns the dimensions of the array in which \code{RasterLayer}s or \code{Spatial*} objects are organized.
#' @param x A \code{\link{SpatialArray}} or \code{\link{RasterArray}} class object.
#' @return A \code{numeric} vector.
#' 
#' @examples
#' data(dems)
#' dim(dems)
#' data(coasts)
#' dim(coasts)
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
#' data(coasts)
#' names(coasts)
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


#' Names of \code{\link{RasterArray}} or \code{\link{SpatialArray}} Layers in the stack
#' 
#' @param x A \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object.
#' @param ... additional arguments passed to class-specific methods.
#' @return A \code{character} vector of names.
#' @exportMethod layers
#' 
#' @examples
#' # names of layers in the stack
#' data(dems)
#' layers(dems)
#' @rdname layers
setGeneric("layers", function(x,...) standardGeneric("layers"))

#' @rdname layers
setMethod(
	"layers", 
	signature="XArray", 
	function(x){
		# returns the layer names
		names(x@stack)

	} 
)


#####################

#' Column names of two-dimensional \code{\link{RasterArray}} or \code{\link{SpatialArray}}
#' 
#' Get or set the column names of two-dimensional code{\link{RasterArray}} or \code{\link{SpatialArray}} objects 
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}} object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of column names or \code{NULL}.
#' 
#' @examples
#' data(coasts)
#' colnames(coasts)
#' colnames(coasts) <- c("a", "b")
#' @rdname colnames
#' @exportMethod colnames
setMethod(
	"colnames",
	signature="XArray",
	function(x) colnames(x@index)
)

#' @rdname colnames
#' @exportMethod "colnames<-"
setReplaceMethod(
	"colnames",
	signature="XArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(length(dim(x))!=2) stop("The proxy is not a 2D matrix.")
		colnames(x@index) <- value 
		return(x)
})



#' Row names of two-dimensional \code{\link{RasterArray}} or \code{\link{SpatialArray}} objects
#' 
#' Get or set the row names of two-dimensional \code{\link{RasterArray}} or \code{\link{SpatialArray}}objects  
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}} object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of row names or \code{NULL}.
#' 
#' @examples
#' data(coasts)
#' rownames(coasts)
#' rownames(coasts) <- paste(rownames(coasts), "Ma")
#' @rdname rownames
#' @exportMethod rownames
setMethod(
	"rownames",
	signature="XArray",
	function(x) rownames(x@index)
)

#' @rdname rownames
#' @exportMethod "rownames<-"
setReplaceMethod(
	"rownames",
	signature="XArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(length(dim(x))!=2) stop("The proxy is not a 2D matrix.")
		rownames(x@index) <- value 
		return(x)
})


#' Names of multidimensional \code{\link{RasterArray}} or \code{\link{SpatialArray}} objects.
#' 
#' Get or set the dimnames of multidimensional \code{\link{RasterArray}} or \code{\link{SpatialArray}} objects 
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}} object.
#' @param value \code{character} vector.
#' @return A \code{list} of \code{character} vectors or \code{NULL}.
#' 
#' @examples
#' data(dems)
#' dimnames(dems)
#' data(coasts)
#' dimnames(coasts)
#' dimnames(coasts)[[2]] <- c("first", "second")
#' @rdname dimnames
#' @exportMethod dimnames
setMethod(
	"dimnames",
	signature="XArray",
	function(x) dimnames(x@index)
)

#' @rdname dimnames
#' @exportMethod "dimnames<-"
setReplaceMethod(
	"dimnames",
	signature="XArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(is.null(dim(x))) stop("One-dimensional RasterArrays and SpatialArrays have no dimnames.")
		dimnames(x@index) <- value 
		return(x)
})



#' Number of columns and rows of a \code{\link{RasterArray}} or \code{\link{SpatialArray}} 
#' 
#' Unlike the \code{ncol} and \code{nrow} functions of the raster package (\code{\link[raster]{ncell}}), this function returns the number of columns and rows of the \code{RasterArray} container, rather than the dimensions of the contained \code{RasterLayer}s. 
#' 
#' @param x A \code{\link{RasterArray}} or \code{\link{SpatialArray}}  class object.
#' @rdname adimatt
#' @return A \code{numeric} value of the number of columns and rows.
#' @exportMethod ncol
#' @examples
#' data(coasts)
#' ncol(coasts)
#' nrow(coasts)
setMethod(
	"ncol", 
	signature="XArray", 
	function(x){
		ncol(x@index)
	} 
)

#' @rdname adimatt
#' @exportMethod nrow
setMethod(
	"nrow", 
	signature="XArray", 
	function(x){
		nrow(x@index)
	} 
)

