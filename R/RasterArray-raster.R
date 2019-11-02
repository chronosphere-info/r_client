# Functions that are practically inherited from Raster*

#' @exportMethod xres
setMethod(
	"xres",
	signature="RasterArray",
	function(x){
		xres(x@stack)
	} 
)


#	setMethod(
#		"filename",
#		signature="RasterArray",
#		function(x){
#			filename(x@stack)
#		} 
#	)

#' @exportMethod yres
setMethod(
	"yres",
	signature="RasterArray",
	function(x){
		yres(x@stack)
	} 
)

#' @exportMethod res
setMethod(
	"res",
	signature="RasterArray",
	function(x){
		res(x@stack)
	} 
)

#' @exportMethod minValue
setMethod(
	"minValue",
	signature="RasterArray",
	function(x, vec=FALSE){
		vals <- minValue(x@stack)
		if(!vec){
			ret <- x@index
			ret[]<- vals[ret]
			ret
		}else{
			vals
		}
	} 
)

#' @exportMethod maxValue
setMethod(
	"maxValue",
	signature="RasterArray",
	function(x, vec=FALSE){
		vals <- maxValue(x@stack)
		if(!vec){
			ret <- x@index
			ret[]<- vals[ret]
			ret
		}else{
			vals
		}
	} 
)



#' @exportMethod resample
setMethod(
	"resample",
	signature=c("RasterArray", "ANY"),
	function(x,y,...){
		x@stack <- stack(resample(x@stack, y,...))
		return(x)
	}
)

#' @exportMethod crop
setMethod(
	"crop",
	signature=c("RasterArray"),
	function(x,y,...){
		x@stack <- stack(crop(x@stack,y,...))
		return(x)
	}
)


#' @exportMethod aggregate
setMethod(
	"aggregate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- stack(aggregate(x@stack,...))
		return(x)
	}
)

#' @exportMethod disaggregate
setMethod(
	"disaggregate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- stack(disaggregate(x@stack,...))
		return(x)
	}
)


#' Extraction of values from multiple RasterLayers in a RasterArray object
#' 
#' The function takes a set of time-dependent coordinates and extracts the value they point to from associted RasterLayers in a RasterArray.
#' 
#' @param y (\code{matrix} or \code{data.frame}). The data table containing the coordinates and (optionally) 
#' 	the indices or names of the associated \code{RasterLayer}s in \code{x}.
#' @param x (\code{RasterArray}). A set of \code{RasterLayers} that are associated with entries (one dimension) or the rows of \code{x}.
#' @param by (\code{character} or \code{vector}) The link between \code{x} and \code{y}. If \code{by} is a \code{character}
#' string then it is expected to be column of \code{x} and should contain the \code{names} or the indices of the
#' associated \code{RasterLayer}s in \code{x}. If it is a \code{vector} its length should match the number of rows in \code{x}
#' and it will be used as if it were a column of \code{x}.
#' @param lng (\code{character}) A column of \code{x} that includes the paleolongitudes.
#' @param lat (\code{character}) A column of \code{x} that includes the paleolatitudes.
#' @param force (\code{character}) If set to \code{"numeric"} the \code{by} argument or the column it points to will be converted to
#' numeric values, and x will be subsetted with \code{numeric} subscripts of the \code{x} \code{RasterArray}. If set to character, the by column (or vector) will be
#' forced to \code{character} values and will be used as character subscripts.
#' @exportMethod extract
setMethod(
	"extract", 
	signature=c(x="RasterArray", y="data.frame"), 
	definition=function(x, y, by, lng="plng", lat="plat", force=NULL){
	
	# defense
#	if(!is.data.frame(y) & !is.matrix(y)) stop("The argument y has to be a data.frame or matrix.")
	
	dimX <- dim(x)
	if(length(dimX)>2) stop("x has to be a one or two-dimensional RasterArray.")

	# recursive case
	if(length(dimX)==2){
		vals <- NULL
		for(k in 1:(dimX[2])){
			temp <- extract(x[,k], y=y, by=by, lng=lng, lat=lat, force=force)
			vals <- cbind(vals, temp)
		}
		# rename the column names
		colnames(vals)<- colnames(x)

	# base case
	}else{

		# column that contains which map the coordinate belongs to
		if(is.character(by)){
			if(!by%in%colnames(y)) stop("The argument by has to be a column of y. ")
			interactor <- y[, by]
		# separate vector
		}else{
			if(is.vector(by) & (length(by)==nrow(y))){
				interactor <- by
			}else{
				stop("Invalid by argument.")
			}
		}
		
		# force subscript type
		if(!is.null(force)){
			if(force=="numeric"){
				interactor <- as.numeric(as.character(interactor))
			}
			if(force=="character"){
				interactor <- as.character(interactor)
			}
		}
	
		# iterate by
		doFor <- sort(unique(interactor))
	
		# coordinates
		coords <- y[, c(lng, lat)]
	
		# storage
		vals <- rep(NA, nrow(y))
	
		for(i in 1:length(doFor)){
			bThis<- doFor[i]==interactor
	
			# select the appropriate RasterLayer
			thisLayer <- x[doFor[i]]
	
			# extract values from raster
			vals[bThis] <- raster::extract(thisLayer, coords[bThis, ])
		}
		names(vals) <- rownames(y)
	}
	return(vals)

})


#' @exportMethod cellStats
setMethod("cellStats", signature="RasterArray", 
	definition=function(x, stat){
		statVect <- raster::cellStats(x@stack, stat=stat)
		endObj<- x@index
		endObj[!is.na(endObj)] <- statVect
		return(endObj)
	}
)

#' @exportMethod summary
setMethod("summary", 
	signature="RasterArray",
	definition=function(object){
		allSumm <- raster::summary(object@stack)
		endRes <- extendDim(proxy(object), allSumm)
		endRes
	}
)

#' Project a RasterArray object
#'
#' The uppercase first letter is temporary until the a proper S4 generic of the function is implemented in the \code{raster} package.
#' 
#' @param from A \code{RasterArray} object to project.
#' 
#' @param ... Arguments passed to the projectRaster function.
#' 
#' @export
ProjectRaster <- function(from, ...){
	from@stack <- raster::stack(raster::projectRaster(from=from@stack, ...))
	return(from)
}
