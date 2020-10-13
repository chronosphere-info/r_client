
#' Apply-type iterator for RasterArrays and SpatialArrays
#' 
#' The function implements the \code{\link[base]{apply}}-type iterators for the RasterArray class. Output values are constrained to RasterArrays, whenever possible. 
#' Not yet implemented for multidimensional MARGINs.
#' @examples
#' # Null dimensional margin
#' data(coasts)
#' # apply function to every element manually
#' # memory taken by every layer
#' apply(coasts, MARGIN=NULL, object.size)
#' # double of itself
#' data(dems)
#' a<- cbind(dems, dems)
#' same <- apply(a, 1, sum)
#' @return Depending on the on the output of \code{FUN}, a \code{list}, a \code{vector} or \code{RasterArray} or \code{SpatialArray} object.
#' @param X an array, including matrices and RasterArrays.
#' @param MARGIN a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns, \code{c(1, 2)} indicates rows and columns. Where \code{X} has named dimnames, it can be a character vector selecting dimension names. For \code{RasterArrays} only single dimension margins are implemented. If it is \code{NULL} then the function is applied to every item.
#' @param FUN  the function to be applied: see ‘Details’ of \code{\link[base]{apply}}. 
#' @param ... optional arguments passed to \code{FUN}.
#' @rdname apply-methods
#' @exportMethod apply
setGeneric("apply", def=base::apply)


# class-specific methods are the next-level, solving the problem of simplifiy, and doing MARGIN-based dispatch


# apply methods
# MARGIN=NULL method applicator
ArrayApplyNULL <- function(X, FUN,...){
	# original dimensionality is kept
	indeX <- X@index
	FUN  <- match.fun(FUN)

	# get the dimension of X
	if(is.null(dim(X))){
		xdim <- length(X)
	}else{
		xdim <- dim(X)
	}

	# prototype the execution
	tempVal <-indeX[!is.na(indeX)]
	proto <- suppressWarnings(FUN(X@stack[[tempVal[1]]], ...))

	# iterate result for every item
	run <- sapply(indeX, function(x){
		if(!is.na(x)){
			# run function on actual elements
			obj <- X@stack[[x]]
			result <- FUN(obj,...)
		}else{
			# try to run the function with NAs
			obj <- x
			result <- NA
			if(is.numeric(proto) | is.character(proto)  | is.logical(proto)){
				result <- rep(result, length(proto))
				dim(result) <- dim(proto)
			}
		#	try(result <- FUN(obj,...), silent=TRUE)
		}
		return(result)
		
	})

	# somewhat redundant check of correct type
	if(is.numeric(run) | is.character(run)  | is.logical(run)){
		# simplest case - dimension match
		if(length(run)==length(indeX)){
			# copy all attributes
			attributes(run) <- attributes(X@index)
			endRes <- run

		}else{
			# if it is not a hyperprism
			if(!is.array(run) & !is.matrix(run)){
				stop("Output cannot be formatted to an array.")
			}else{
				# get the dimension of the final product
				if(is.null(dim(proto))){
					protoDim <- length(proto)
				}else{
					protoDim <- dim(proto)
				}

				newDim <- c(xdim, protoDim)
				# transpose the output
				newRun <- t(run)

				# copy the dimensions - by a miracle this conserves everything
				dim(newRun) <- newDim

				# dimnames of the source and the output
				if(length(xdim)==1){
					xNames <- list(names(X))
				}else{
					xNames <- dimnames(X)
				}
				if(length(protoDim)==1){
					protoNames <- list(names(proto))
				}else{
					protoNames <- dimnames(proto)
				}
				

				if(!is.null(xNames) | !is.null(protoNames)){
					tempList <- list()
					for(i in 1:length(newDim)) tempList[[i]] <- rep(NA, newDim[i])

					dimnames(newRun) <- tempList

					if(!is.null(xNames)){
						for(i in 1:length(xdim)) dimnames(newRun)[[i]] <- xNames[[i]]
					}
					if(!is.null(protoNames)){
						for(i in 1:(length(newDim)-length(xdim))) dimnames(newRun)[[length(xdim)+i]] <- protoNames[[i]]
					}
				}

				endRes <- newRun 
			}

		}
	}else{
		# list output - potential RasterArray
		if(is.list(run)){
			whichNA <- is.na(run)

			# determine what the class of output is
			classes  <- unique(unlist(lapply(run[!whichNA], class)))

			# single - go on
			if(length(classes)==1){
				if(classes=="RasterLayer" | inherits(run[[which(!whichNA)[1]]], "Spatial")){ 

					if(classes=="RasterLayer"){
						# copy the inded
						newStack <- stack(run[!whichNA])
					}else{
						newStack <- SpatialStack(run[!whichNA])
					}
					
					# values in the index
					vals <- 1:nlayers(newStack)

					# use the original index for 
					indeX[whichNA] <- NA
					indeX[!whichNA] <- vals

					# RasterArray conversion
					if(classes=="RasterLayer"){
						# final object ot be export
						endRes <- RasterArray(stack=newStack, index=indeX)
					}

					if(inherits(run[[which(!whichNA)[1]]], "Spatial")){
						# final object ot be export
						endRes <- SpatialArray(stack=newStack, index=indeX)
					}
				}else{
					stop("Output class of FUN is not supported. ")
				}

			# or break	
			}else{
				stop("Multiple output classes. Submit functions that have same output for every element.")

			}


		}

	}


	return(endRes)


}


# this method is to be called, when the MARGIN is non-null - original apply-type iteration
ArrayApplyReduce <- function(X, MARGIN, FUN,...){
	if(length(dim(X))==1) stop("The 'apply()'' function is not applicable to vector-like RasterArray or SpatialArrays s.\nUse 'sapply()'' instead.")
	if(length(MARGIN)>1) stop("Not yet implemented for multidimensinal margins.")

	# use the proxy instead
	proxX <- proxy(X)

	ret <-apply(proxX, MARGIN, function(y){
		# get the columns/rows associated with this subset
		thislayers <- X[[y]]
		# if all are empty, force numeric
		if(sum(is.na(thislayers))==length(thislayers)) thislayers <- as.numeric(thislayers)

		# and plug it into the supplied function
		FUN(thislayers)
	})

	# draft of the apply output structure
	struct <- apply(X@index, MARGIN, function(y) 1)

	# if this output is a rasterlayer make an exception!
	if(is.list(ret)){
		classOfItems <- unlist(lapply(ret, function(y){
				if(class(y)=="RasterLayer" | inherits(y, "Spatial")){
					return(FALSE)
				}else{
					if(is.numeric(y) | is.character(y) | is.logical(y)){
						if(is.na(y)){
							return(NA)
						}else{
							return(TRUE)
						}
					}else{
						return(TRUE)
					}
				}
			} ))
		if(sum(unique(classOfItems), na.rm=TRUE)==0){
			bNA <- is.na(classOfItems)

			# transform this to a rasterArray
			newStack <- stack(ret[!bNA])
			# index
			struct[] <- NA
			struct[!bNA] <- 1:sum(!bNA)
			names(struct) <- names(ret)

			if(class(newStack)=="RasterStack"){
				ret <- RasterArray(newStack, struct)
			}else{
				ret <- SpatialArray(newStack, struct)
			}

		}
	}

	return(ret)
}
