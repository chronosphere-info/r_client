
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

