#' The proxy of a RasterArray or SpatialArray object
#' 
#' This function returns an object that symbolizes the structure of layers in the \code{RasterArray} and \code{SpatialArray}.
#'
#' The \code{proxy} method wraps the names of layers in the stack using the \code{index} slot of the \code{RasterArray}.
#'  
#' @param x \code{RasterArray} or \code{SpatialArray}  ocal object.
#' @return A \code{vector}, \code{matrix} or \code{array} of characters representing the \code{RasterArray} and \code{SpatialArray} structure.
#' @param ... additional arguments passed to class-specific methods.
#' @examples
#' data(dems)
#' proxy(dems)
#'
#' data(clim)
#' proxy(clim)
#' @exportMethod proxy
#' @rdname proxy
setGeneric("proxy", function(x,...) standardGeneric("proxy"))

#' @rdname proxy
setMethod(
	"proxy",
	signature="XArray",
	function(x){
		ind <- x@index
		
		# only NAs are present
		if(any(!is.na(ind))){
			if(!is.null(names(x@stack))) ind[]<- names(x@stack)[ind]
		}
		
		return(ind)
	}

)

