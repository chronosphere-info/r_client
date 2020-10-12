#' @method as.data.frame SpatialArray
#' @rdname as.data.frame-methods
#' @export 
as.data.frame.SpatialArray <- function(x, row.names=NULL, optional=FALSE,...){
	df <- as.data.frame(proxy(x))
	if(ncol(df)==1) colnames(df) <- "X0"
	if(!is.null(row.names)){
		rownames(df)<- row.names
	}
	if(optional){
		rownames(df) <- NULL
		colnames(df) <- NULL
	}
	return(df)
}


#' Convert Spatial* objects to SpatialArrays
#' 
#' The function converts \code{Spatial*}objects to \code{\link{SpatialArray}}-class objects.
#' @param from Object to be converted. 
#' 
#' @examples
#' # data(coasts)
#' # recent <- coasts[1]
#' # # convert Spatial* to as.SpatialArray
#' # sa <- as.SpatialArray(recent)
#' @rdname asSpatialArray
#' @return A \code{SpatialArray} class object.
#' @exportMethod as.SpatialArray
setGeneric("as.SpatialArray", function(from) standardGeneric("as.SpatialArray"))


#' @rdname asRasterArray
setMethod(as.SpatialArray, signature=c("SpatialPoints"), definition=function(from){
	SpatialArray(SpatialStack(from), index=1)
})

#' @rdname asRasterArray
setMethod(as.SpatialArray, signature=c("SpatialPointsDataFrame"), definition=function(from){
	SpatialArray(SpatialStack(from), index=1)
})

#' @rdname asRasterArray
setMethod(as.SpatialArray, signature=c("SpatialLines"), definition=function(from){
	SpatialArray(SpatialStack(from), index=1)
})

#' @rdname asRasterArray
setMethod(as.SpatialArray, signature=c("SpatialLinesDataFrame"), definition=function(from){
	SpatialArray(SpatialStack(from), index=1)
})

#' @rdname asRasterArray
setMethod(as.SpatialArray, signature=c("SpatialPolygons"), definition=function(from){
	SpatialArray(SpatialStack(from), index=1)
})

#' @rdname asRasterArray
setMethod(as.SpatialArray, signature=c("SpatialPolygonsDataFrame"), definition=function(from){
	SpatialArray(SpatialStack(from), index=1)
})



#' Coerce a \code{\link{SpatialArray}} class object to a list
#' 
#' The function will return the items of the \code{\link{SpatialArray}} as a list (conserving the names of the elements in the stack).
#' @param x A \\code{\link{SpatialArray}} class object.
#' @return A \code{list} of \code{RasterLayers}.
#' @param ... arguments passed to or from methods.
#' 
#' @exportMethod as.list
setMethod("as.list","SpatialArray", function(x,...){
	x@stack@Spatials
})
