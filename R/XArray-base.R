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




#' Transpose a \code{\link{RasterArray}} or \code{\link{SpatialArray}} object
#' 
#' @examples
#' data(dems)
#' t(dems)
#' data(clim)
#' t(clim)
#' @param x A \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object. 
#' @return A \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object.
#' @rdname t-methods
#' 
#' @exportMethod t
"t"

#' @rdname t-methods
setMethod(
	"t", 
	"XArray", 
	function(x){
		if(length(dim(x))>2) stop("The RasterArray or SpatialArray has too many dimensions. ")

		# transpose index
		tIndex<- t(x@index)
		vIndex <- as.numeric(tIndex)

		# ordering
		vIndna <- vIndex[!is.na(vIndex)]

		# reorder the stack
		x@stack <- x@stack[[vIndna]]

		# refill the index
		tIndex[!is.na(tIndex)] <- 1:nlayers(x@stack)

		# copy names
		if(!is.null(colnames(x@index))) rownames(tIndex) <- colnames(x@index)
		if(!is.null(rownames(x@index))) colnames(tIndex) <- rownames(x@index)
		if(!is.null(names(x@index)))  colnames(tIndex) <- names(x@index)

		# replace the index
		x@index <- tIndex



		return(x)

	}
)
