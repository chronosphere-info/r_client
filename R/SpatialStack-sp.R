#' Function to transform the coordinate reference system of an entire SpatialStack or SpatialArray
#' 
#' Joint reprojection of entire sets of vector data.
#'
#' The function requires the rgdal package to run.
#' 
#' @param x The \code{\link{SpatialStack}} or \code{\link{SpatialArray}} object.
#' @param CRSobj A \code{\link[sp]{CRS}} class or \code{character} object defining a coordinate reference system.
#' @param ... Additional arguments.
#' @rdname spTransform
#' @examples
#' # load example data
#' data(coasts)
#' mollCoast <- spTransform(coasts, "+proj=moll")
#' @return A \code{\link{SpatialStack}} or \code{\link{SpatialArray}} object.
#' @exportMethod spTransform
setMethod(
	"spTransform", 
	c("SpatialStack", "ANY"),
	function(x, CRSobj, ...){
		# only if
		if(! requireNamespace("rgdal", quietly=TRUE)) stop("This method requires the 'rgdal' package to run.")
		
		# iterate separately for every part
		x@Spatials<-lapply(x@Spatials, function(y){
			sp::spTransform(y, CRSobj)
		})

		# update the CRS of the entire stack
		if(is.character(CRSobj)){
			x@proj4string <- sp::CRS(CRSobj)
		}else{
			x@proj4string <- CRSobj
		}

		# final value
		return(x)
	}
)
