#' @rdname spTransform
setMethod(
	"spTransform", 
	c("SpatialArray", "ANY"),
	function(x, CRSobj, ...){
		# only if
		if(! requireNamespace("rgdal", quietly=TRUE)) stop("This method requires the 'rgdal' package to run.")
		
		# fall back to SpatialStack method.
		newStack<-spTransform(x@stack, CRSobj)

		# recreate the strucuture
		x <- SpatialArray(stack=newStack, index=x@index)
		
		# final value
		return(x)
	}
)
