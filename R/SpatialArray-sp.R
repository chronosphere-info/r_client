#' @rdname spTransform
setMethod(
	"spTransform", 
	c("SpatialArray", "ANY"),
	function(x, CRSobj, ...){
		# only if
		if(! requireNamespace("rgdal", quietly=TRUE)) stop("This method requires the 'rgdal' package to run.")
		
		# fall back to SpatialStack method.
		x@stack<-spTransform(x@stack, CRSobj)
		
		# final value
		return(x)
	}
)
