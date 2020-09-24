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
#' # data(dems)
#' # recent <- dems[1]
#' # # convert RasterLayer to RasterArray
#' # ra <- as.RasterArray(recent)
#' @rdname asSpatialArray
#' @return A \code{aSpatialArray} class object.
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



