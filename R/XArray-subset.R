
#' Subset a \code{\link{RasterArray}} or \code{\link{SpatialArray}} object
#' 
#' Extract subsets of \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object similarly to a regular array. 
#' 
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \\code{\link{RasterArray}} or \code{\link{SpatialArray}} wrapper be dropped and the element be reduced to a single \code{RasterLayer}/ \code{Spatial*} object?
#' @param oneDim \code{logical} In case of multidimensional \code{\link{RasterArray}}s or \code{\link{SpatialArray}}s, setting \code{oneDim} to \code{TRUE} allows the application of one dimensional subscripts.  
#' @return A \code{RasterLayer}, \code{RasterArray}, \code{Spatial*} or \code{\link{SpatialArray}} class object.
# combined
#' @exportMethod subset
#' @examples
#' data(dems)
#' # first 4
#' subset(dems, i=1:4)
#' # missing at the end
#' subset(dems, i=1:12)
#' # character subscript
#' subset(dems, i=c("5", "25"))
#' # logical subscript
#' subs <- rep(TRUE, length(dems))
#' subs[1] <- FALSE # remove first
#' subset(dems, i= subs)
#' # no drop
#' subset(dems, i=1, drop=FALSE)
setMethod(
	"subset", 
	signature(x="XArray"), 
	function(x, i,j, ...,oneDim=FALSE, drop=TRUE){
			# fetch the index
			indDim <- dim(x@index)

			# one dim case
			if(is.null(indDim) | length(indDim)==1){
				originIndex <-x@index[i]
			}

			# two dim case
			if(length(indDim)>=2){
				# multidimensional subscript
				if(!oneDim){
					originIndex <-x@index[i,j,...]
				# one dimensional subscript
				}else{
					originIndex<-x@index[i, drop=TRUE]
				}
			}
		
			# constrain one dimension
			fetchIndex <- originIndex
			dim(fetchIndex) <- NULL

			# drop to a single RasterLayer
			if(length(fetchIndex)==1 & drop==TRUE){
				# if it is NA
				if(is.na(fetchIndex)){
					x<-NA
				}else{
					x<- x@stack[[fetchIndex]]
				}
				

			# keep using RasterArray
			}else{
				# separate the NAs
				bNA <- is.na(fetchIndex)
				if(any(bNA)){
					validFetch <- fetchIndex[!bNA]
				}else{
					validFetch <- fetchIndex
				}

				# get the relevant layers
				x@stack <- x@stack[[validFetch, drop=FALSE]]

				# rewrite the index 
				x@index<- originIndex

				# if the proxy was numeric, it should be reset
				x@index[!bNA] <- 1:nlayers(x@stack)

				return(x)
			}		
	}
)


#' Indexing to extract subsets of a code{\link{RasterArray}} or \code{\link{SpatialArray}} object
#'
#' Single bracket \code{'['} refers to indices and names within the \code{\link{RasterArray}} . Use double brackets to extract layers based on their names (in the stack).
#' 
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}}  object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \code{\link{RasterArray}} or \code{\link{SpatialArray}}  wrapper be dropped and the element be reduced to a single \code{RasterLayer} or \code{Spatial*}?
#' @return A \code{RasterLayer}, \code{RasterArray}, \code{Spatial*} or \code{\link{SpatialArray}} class object.
#' @examples
#' data(dems)
#' # numeric subsetting
#' firstThree <- dems[1:3]
#' # character subsetting
#' second <- dems["10"]
#' # logical subsetting
#' subscript <- rep(FALSE, length(dems))
#' subscript[2] <- TRUE
#' second2 <- dems[subscript]
#' 
#' @exportMethod [
setMethod(
	"[",
	signature(x="XArray", i="ANY", j="ANY"),
	definition=function(x,i,j,..., drop=TRUE){
		sysCall <- sys.call(which=-1)

		oneDim<-FALSE
		if(length(sysCall)==3){
			oneDim <- TRUE
		}
		subset(x,i,j,..., oneDim=oneDim, drop=drop)
		
	}
)
