# Functions to combine items that result in a RasterArray

###############################################################################
# combine()


#' @rdname combine
setMethod(
	"combine",
	"RasterLayer",

	#c.RasterLayer<- 
	function(x, ...){
		listArg <- list(...)
		finRA <- x
		# store the system call
		callSymb <- sys.call(which=-1)
	
		# run loop only if it is more than 1
		if(length(listArg)!=0){
			for(i in 1:length(listArg)){
				elem <- listArg[[i]]
				# name of the first will be taken care of by c2
				finRA<-c2(finRA, elem)
				# try to overwrite the name - necessary for multiple combinations
				if(class(elem)=="RasterLayer"){
					if(is.symbol(callSymb[[i+2]])){
						names(finRA)[length(finRA)] <- deparse(callSymb[[i+2]])
					}else{
						names(finRA)[length(finRA)] <- NA
					}
				}
			}
		}
	
		return(finRA)
	}
)


################################################################
# Internals for c- methods


setMethod("c2", signature=c("RasterLayer", "RasterLayer"), 
	definition=function(x, y){

		ind <- 1:2
		
		# get the names alright:
		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[2]])){
			names(ind)[1] <- deparse(callSymb[[2]])
		}

		if(is.symbol(callSymb[[3]])){
			names(ind)[2] <- deparse(callSymb[[3]])
		}

		# take the two layers and make a stack
		endObj <- RasterArray(raster::stack(x, y), index=ind)

		return(endObj)
	}
)


# adding NAs to the layer
setMethod("c2", signature=c("RasterLayer", "logical"), 
	definition=function(x, y){
		if(!any(!is.na(y))) "Invalid argument."

		# add y NAs to the end
		ind <- c(1, rep(NA, length(y)))

		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[2]])){
			names(ind)[1] <- deparse(callSymb[[2]])
		}
		endObj <- RasterArray(raster::stack(x), index=ind)
		return(endObj)

	}
)

# raster::mask() depends on similar structure. 
setMethod("c2", c("RasterLayer", "list"),
	function(x,y){
		ind <- c(list(x), y)

		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[2]])){
			names(ind)[1] <- deparse(callSymb[[2]])
		}

		if(is.symbol(callSymb[[3]])){
			names(ind)[2] <- deparse(callSymb[[3]])
		}

		return(ind)

	}
)


# if somebody wants to do this, than that really wants a list instead
setMethod("c2", c("RasterArray", "list"),
	function(x,y){
		ind <- c(as.list(x), y)

#		callSymb <- sys.call(which=-3)
#		if(is.symbol(callSymb[[2]])){
#			names(ind)[1] <- deparse(callSymb[[2]])
#		}
#
#		if(is.symbol(callSymb[[3]])){
#			names(ind)[2] <- deparse(callSymb[[3]])
#		}

		return(ind)

	}
)


# adding NAs to the arrays
setMethod("c2", signature=c("RasterArray", "logical"), 
	definition=function(x, y){
		if(!any(!is.na(y))) "Invalid argument."
		ind<- c(x@index, rep(NA,length(y)))

		# copy the name if it there is one
		if(!is.null(names(y))) names(ind)[(length(ind)-length(y)+1):length(ind)] <- names(y)

		# replace index with new
		x@index <- ind

		# return corrected object
		return(x)
		
	}
)



# adding multiple RasterLayers
setMethod("c2", signature=c("RasterArray", "RasterLayer"), 
	definition=function(x, y){
		# The new index
		ind <- c(x@index, nlayers(x)+1)
		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[3]])){
			names(ind)[length(ind)] <- deparse(callSymb[[3]])
		}

		endObj <- RasterArray(raster::stack(x@stack, y), index=ind)
		return(endObj)

	}
)


# simples method
setMethod("c2", signature=c("RasterLayer", "RasterArray"), 
	definition=function(x, y){
		# new index 
		ind <- c(1, y@index+1)

		# use the name of object
		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[2]])){
			names(ind)[1] <- deparse(callSymb[[2]])
		}

		endObj <- RasterArray(raster::stack(x, y@stack), index=ind)
		return(endObj)


	}
)



setMethod("c2", signature=c("RasterArray", "RasterArray"), 
	definition=function(x, y){
		# shift indices of the second argument
		indexPlus<- y@index+nlayers(x)

		# combine the indices
		ind <- c(x@index, indexPlus)

		# the final object
		endObj <- RasterArray(raster::stack(x@stack, y@stack), index=ind)

		return(endObj)

		
	}
)

###############################################################################

#' Combine Raster and Vector Spatial data to \code{\link{RasterArray}} or \code{\link{SpatialArray}} objects by rows or columns
#' 
#' The function takes a sequence of \code{RasterLayer} or \code{RasterArray} class objects and combines them to two dimensional \code{RasterArrays},
#' Alternatively, the function can be used to combine vector \code{Spatial*} data to \code{SpatialArray}s.
#' Named objects will be forced together based on \code{names}, \code{colnames} or \code{rownames} attributes, via insertion of \code{NAs}.
#' 
#' @examples
#' data(dems)
#' # create matrices out of vectors
#' colb <- cbind(dems, dems)
#' rowb <- rbind(dems, dems)
#' # automatic name matching
#' dems2 <- dems[c(1:4, 6:10)]
#' matched <- suppressWarnings(cbind(dems, dems2))
#' @export cbind.RasterArray
#' @exportS3Method cbind RasterArray
#' @return A \code{RasterArray} or \code{SpatialArray} class object.
#' @rdname bind-methods
#cbind.RasterArray<-function(..., deparse.level=1){
cbind.RasterArray<-function(...){
		listArg <- list(...)
		finRA <- listArg[[1]]
		for(i in 2:length(listArg)){
			finRA<-cbind2(finRA, listArg[[i]])
		}
		return(finRA)
	}

###############################################################################
# rbind()


#' @export rbind.RasterArray
#' @exportS3Method rbind RasterArray
#' @rdname bind-methods
#' @param ... RasterLayer or RasterArray class objects to be combined.
#rbind.RasterArray<-function(..., deparse.level=1){
rbind.RasterArray<-function(...){
	listArg <- list(...)
	finRA <- listArg[[1]]
	for(i in 2:length(listArg)){
		finRA<-rbind2(finRA, listArg[[i]])
	}
	return(finRA)
}

