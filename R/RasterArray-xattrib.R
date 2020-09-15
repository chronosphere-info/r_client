#' Number of elements or layers in a RasterArray class object
#' 
#' Function to return the length of the array in which \code{RasterLayers} are organized.
#' 
#' The length() function returns the number elements that should be present based on the array structure itself, and not the total number of values stored in the object (such as the \code{length} method of \code{RasterStack}s). As the object can contain missing values, the number of actual layers can be queried with \code{\link{nlayers}}. 
#' 
#' @param x a \code{RasterArray} class object.
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
	signature="RasterArray",
	function(x) length(x@index)
)

#' Number of cells in a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @rdname ncell
#' @return A \code{numeric} value.
#' @examples
#' data(dems)
#' ncell(dems)
#' @exportMethod ncell
setMethod(
	"ncell",
	signature="RasterArray",
	function(x) ncell(x@stack)
)

#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="RasterArray",
	function(x) nlayers(x@stack)
)


#' Column names of two-dimensional RasterArray or SpArray
#' 
#' Get or set the column names of two-dimensional RasterArray or SpArray objects 
#' @param x \code{RasterArray} or \code{SpArray} object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of column names or \code{NULL}.
#' 
#' @examples
#' data(clim)
#' colnames(clim)
#' colnames(clim) <- c("a", "b")
#' @rdname colnames
#' @exportMethod colnames
setMethod(
	"colnames",
	signature="RasterArray",
	function(x) colnames(x@index)
)

#' @rdname colnames
#' @exportMethod "colnames<-"
setReplaceMethod(
	"colnames",
	signature="RasterArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(length(dim(x))!=2) stop("The RasterArray proxy is not a 2D matrix.")
		colnames(x@index) <- value 
		return(x)
})



#' Row names of two-dimensional RasterArray or SpArray objects
#' 
#' Get or set the row names of two-dimensional RasterArray or SpArray objects  
#' @param x \code{RasterArray} or \code{SpArray} object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of row names or \code{NULL}.
#' 
#' @examples
#' data(clim)
#' rownames(clim)
#' rownames(clim) <- paste("year", rownames(clim))
#' @rdname rownames
#' @exportMethod rownames
setMethod(
	"rownames",
	signature="RasterArray",
	function(x) rownames(x@index)
)

#' @rdname rownames
#' @exportMethod "rownames<-"
setReplaceMethod(
	"rownames",
	signature="RasterArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(length(dim(x))!=2) stop("The RasterArray proxy is not a 2D matrix.")
		rownames(x@index) <- value 
		return(x)
})

#' Names of one-dimensional RasterArray, SpatialStack or SpatialArray objects.
#' 
#' Get or set the names of one-dimensional RasterArray or SpArray objects 
#' @param x \code{RasterArray}, \code{SpatialStack} or \code{SpArray} object.
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
	signature="RasterArray",
	function(x){	
		names(x@index)
	}
)

#' @rdname names
#' @exportMethod "names<-"
setReplaceMethod(
	"names",
	signature="RasterArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(is.null(names(x))) names(x@index) <- rep(NA, length(x@index))
		names(x@index) <- value 
		return(x)
})

#' Names of multidimensional RasterArray or SpArray objects.
#' 
#' Get or set the dimnames of multidimensional RasterArray or SpArray objects 
#' @param x \code{RasterArray} or \code{SpArray} object.
#' @param value \code{character} vector.
#' @return A \code{list} of \code{character} vectors or \code{NULL}.
#' 
#' @examples
#' data(dems)
#' dimnames(dems)
#' data(clim)
#' dimnames(clim)
#' dimnames(clim)[[2]] <- c("first", "second")
#' @rdname dimnames
#' @exportMethod dimnames
setMethod(
	"dimnames",
	signature="RasterArray",
	function(x) dimnames(x@index)
)

#' @rdname dimnames
#' @exportMethod "dimnames<-"
setReplaceMethod(
	"dimnames",
	signature="RasterArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(is.null(dim(x))) stop("One-dimensional RasterArrays have no dimnames.")
		dimnames(x@index) <- value 
		return(x)
})

#' Names of RasterArray's Layers in the stack
#' 
#' @param x A \code{RasterArray} class object.
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
	signature="RasterArray", 
	function(x){
		# returns the layer names
		names(x@stack)

	} 
)

#' The total number of values in a RasterArray object
#' 
#' @param x A \code{RasterArray} class object.
#' @param ... additional arguments passed to class-specific methods.
#' @return A \code{numeric} value.
#' 
#' @exportMethod nvalues
#' @examples 
#' data(dems)
#' nvalues(dems)
#' @rdname nvalues
setGeneric("nvalues", function(x,...) standardGeneric("nvalues"))

#' @rdname nvalues
setMethod(
	"nvalues", 
	signature="RasterArray", 
	function(x){
		# returns the layer names
		length(x@stack)

	} 
)


#' Dimensions of RasterArray objects
#' 
#' The function returns the dimensions of the array in which \code{RasterLayer}s are organized.
#' @param x A \code{RasterArray} class object.
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
	signature="RasterArray", 
	function(x){
		proxyDim <- dim(x@index)
		if(is.null(proxyDim)) proxyDim <- length(x@index)
		proxyDim
	} 
)


#' Dimensions of RasterLayers in a RasterArray object
#' 
#' The funcion will return the dimensions RasterLayers
#' 
#' @param x A \code{RasterArray} class object.
#' @return A \code{numeric} vector with the number of rows and columns in the \code{RasterLayer}s.
#' @param ... additional arguments passed to class-specific methods.
#' 
#' @rdname dimlayer
#' @exportMethod dimlayer
setGeneric("dimlayer", function(x,...) standardGeneric("dimlayer"))

#' @rdname dimlayer
setMethod(
	"dimlayer", 
	signature="RasterArray", 
	function(x){
		# depends on subset-method
		dim(x@stack[[1]])[1:2]

	} 
)

#' Number of columns and rows of a RasterArray
#' 
#' Unlike the \code{ncol} and \code{nrow} functions of the raster package (\code{\link[raster]{ncell}}), this function returns the number of columns and rows of the \code{RasterArray} container, rather than the dimensions of the contained \code{RasterLayer}s. 
#' 
#' @param x A \code{RasterLayer} class object.
#' @rdname adimatt
#' @return A \code{numeric} value of the number of columns and rows.
#' @exportMethod ncol
#' @examples
#' data(clim)
#' ncol(clim)
#' nrow(clim)
setMethod(
	"ncol", 
	signature="RasterArray", 
	function(x){
		ncol(x@index)
	} 
)

#' @rdname adimatt
#' @exportMethod nrow
setMethod(
	"nrow", 
	signature="RasterArray", 
	function(x){
		nrow(x@index)
	} 
)
