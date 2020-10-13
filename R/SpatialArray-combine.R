#' @export rbind.SpatialArray
#' @exportS3Method rbind SpatialArray
#' @rdname bind-methods
cbind.SpatialArray<-function(...){
	listArg <- list(...)
	finRA <- listArg[[1]]
	for(i in 2:length(listArg)){
		finRA<-cbind2(finRA, listArg[[i]])
	}
	return(finRA)
}

#' @export rbind.SpatialArray
#' @exportS3Method rbind SpatialArray
#' @rdname bind-methods
rbind.SpatialArray<-function(...){
	listArg <- list(...)
	finRA <- listArg[[1]]
	for(i in 2:length(listArg)){
		finRA<-rbind2(finRA, listArg[[i]])
	}
	return(finRA)
}




#' @rdname combine
setMethod(
	"combine",
	"VectorSpatialClasses",

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
				if(inherits(elem, "Spatial")){
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



# c2 submethods

setMethod("c2", signature=c("VectorSpatialClasses", "VectorSpatialClasses"), 
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
		endObj <- SpatialArray(stack(x, y), index=ind)

		return(endObj)
	}
)


# adding NAs to the layer
setMethod("c2", signature=c("VectorSpatialClasses", "logical"), 
	definition=function(x, y){
		if(!any(!is.na(y))) "Invalid argument."

		# add y NAs to the end
		ind <- c(1, rep(NA, length(y)))

		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[2]])){
			names(ind)[1] <- deparse(callSymb[[2]])
		}
		endObj <- SpatialArray(stack(x), index=ind)
		return(endObj)

	}
)


# depends on similar structure. 
setMethod("c2", c("VectorSpatialClasses", "list"),
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


# adding NAs to the arrays
setMethod("c2", signature=c("SpatialArray", "logical"), 
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
setMethod("c2", signature=c("SpatialArray", "VectorSpatialClasses"), 
	definition=function(x, y){
		# The new index
		ind <- c(x@index, nlayers(x)+1L)
		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[3]])){
			names(ind)[length(ind)] <- deparse(callSymb[[3]])
		}

		endObj <- SpatialArray(stack(x@stack, y), index=ind)
		return(endObj)

	}
)


# simples method
setMethod("c2", signature=c("VectorSpatialClasses", "SpatialArray"), 
	definition=function(x, y){
		# new index 
		ind <- c(1L, y@index+1L)

		# use the name of object
		callSymb <- sys.call(which=-3)
		if(is.symbol(callSymb[[2]])){
			names(ind)[1] <- deparse(callSymb[[2]])
		}

		endObj <- SpatialArray(stack(x, y@stack), index=ind)
		return(endObj)


	}
)



setMethod("c2", signature=c("SpatialArray", "SpatialArray"), 
	definition=function(x, y){
		# shift indices of the second argument
		indexPlus<- y@index+nlayers(x)

		# combine the indices
		ind <- c(x@index, indexPlus)

		# the final object
		endObj <- SpatialArray(stack(x@stack, y@stack), index=ind)

		return(endObj)

		
	}
)
