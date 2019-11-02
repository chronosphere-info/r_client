# Functions to subset and replace items in a RasterArray

# combined
#' @exportMethod subset
setMethod(
	"subset", 
	signature(x="RasterArray"), 
	function(x, i,j, ..., drop=TRUE, filename=""){
			# fetch the index
			indDim <- dim(x@index)

			# one dim case
			if(is.null(indDim) | length(indDim)==1){
				originIndex <-x@index[i]
			}

			# two dim case
			if(length(indDim)>=2){
				originIndex <-x@index[i,j,...]
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
				x@index[!bNA] <- 1:raster::nlayers(x@stack)

				return(x)
			}		
	}
)

#' @exportMethod [
setMethod(
	"[",
	signature(x="RasterArray", i="ANY", j="ANY"),
	definition=function(x,i,j,..., drop=TRUE){
		subset(x,i,j,..., drop=drop)
	}
)


#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="RasterArray", value="logical"),
	definition=function(x,i,j,..., value){
		# fetch the index
		indDim <- dim(x@index)

		if(sum(is.na(value))!=length(value)) stop("Invalid replacement type.")
		if(is.null(indDim) | length(indDim)==1){
			if(length(i)!=length(value) & length(value)!=1) stop("Invalid replacement length.")
			theIndex <- x@index[i]
			x@index[i] <- NA

		}
		
		# multi- dim case
		if(length(indDim)>=2){
			theIndex <- x@index[i,j,...]
			x@index[i,j,...] <- NA
		}

		# ensure flat index

		# rebuild the stack
		origInd<- 1:nlayers(x@stack)
		keepOrig <- origInd[!origInd%in%theIndex]
		
		# omit unwanted layers
		x@stack <- x@stack[[keepOrig]]
		
		# constrain order again
		x@index<- defragment(x@index)

		return(x)
		
	}
)



#' Replace RasterLayers in a RasterArray object
#' @param x \code{RasterArray} object.
#' @param value A \code{RasterLayer} or \code{RasterArray} object.
#' 
#' @examples
#' # an example
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="RasterArray", value="RasterLayer"),
	definition=function(x,i,j,..., value){
		# fetch the index
		indDim <- dim(x@index)
		
		# one dim case
		if(is.null(indDim) | length(indDim)==1){
			# pointer to the stack to be replaced
			theIndex <- x@index[i]

			# separte the index based on NA
			bIndNA <- is.na(theIndex)
			
			# if at least one layer stack is not there
			if(any(bIndNA)){
				# usef for the addition
				newI <- i[bIndNA]

				# prepare the new index vector
				tempIndex <- x@index
				tempIndex[newI]<- -1
				tempIndex <- defragment(tempIndex)

				# where are the old layers in the new stack
				oldInNew <- tempIndex[!is.na(x@index)]
				# the index of the new layers
				totallyNew <- tempIndex[newI]
				# add to the rest
				newInd <- c(oldInNew, totallyNew)

				# use this to reorder
				tempInd2 <- rep(NA, length(newInd))
				tempInd2[newInd]<- 1:length(newInd)
				
				# add the new layer to the stack
				newStack <- stack(x@stack, value[[rep(1, length(totallyNew))]])

				# the reorderd stack
				x@stack <- newStack[[tempInd2]]
				x@index <- tempIndex
			}
			if(any(!bIndNA)){
				# restart the process, now with the new index vector
				theIndex <- x@index[i]
				replaceIndex <- theIndex[!bIndNA]
			
				# simply replace the layers in the stack...
				allVals <- 1:nlayers(x@stack)
				origVals <- allVals[!allVals%in%replaceIndex]
			
				# create a reorderer vector
				newInd <-c(origVals, replaceIndex)
				tempInd2 <- rep(NA, length(newInd))
				tempInd2[newInd] <-  1:length(tempInd2)
				
				# put the additional elements to the stack
				newStack <- stack(x@stack[[origVals]], value[[rep(1, length(replaceIndex))]])
				
				# reorder to correct
				x@stack <- newStack[[tempInd2]]
			}
		}
		# multi- dim case
		if(length(indDim)>=2){
			theIndex <- x@index[i,j,...]
			
			# separte the index based on NA
			bIndNA <- is.na(theIndex)
			
			if(any(bIndNA)){
				fullInd <- 1:length(x@index)
				dim(fullInd) <- dim(x@index)
				newIJ <- fullInd[i,j,...]
				newIJ<- newIJ[bIndNA]

				# prepare the index vector
				tempIndex <- x@index
				tempIndex[newIJ]<- -1
				tempIndex <- defragment(tempIndex)

				# where are the old layers in the new stack
				oldInNew <- tempIndex[!is.na(x@index)]
				# add the index at the end
				totallyNew <- tempIndex[newIJ]
				newInd <- c(oldInNew, totallyNew)

				# use this to reorder
				tempInd2 <- rep(NA, length(newInd))
				tempInd2[newInd]<- 1:length(newInd)
				
				# add the new layer to the stack
				newStack <- stack(x@stack, value[[rep(1, length(totallyNew))]])

				# the reorderd stack
				x@stack <- newStack[[tempInd2]]
				x@index <- tempIndex
			}
			if(any(!bIndNA)){
				# restart the process, now with the new index vector
				theIndex <- x@index[i,j,...]
				replaceIndex <- theIndex[!bIndNA]
			
				# simply replace the layers in the stack...
				allVals <- 1:nlayers(x@stack)
				origVals <- allVals[!allVals%in%replaceIndex]
			
				# create a reorderer vector
				newInd <-c(origVals, replaceIndex)
				tempInd2 <- rep(NA, length(newInd))
				tempInd2[newInd] <-  1:length(tempInd2)
				
				# put the additional elements to the stack
				newStack <- stack(x@stack[[origVals]], value[[rep(1, length(replaceIndex))]])
				
				# reorder to correct
				x@stack <- newStack[[tempInd2]]
			
			}

		}

		return(x)
	}
)


#' @exportMethod "[["
setMethod(
	"[[", 
	signature(x="RasterArray"),
	function(x,i,drop=TRUE){
		x@stack[[i, drop=drop]]
	}
)

#' Replace RasterLayers in a RasterArray object
#' @param x \code{RasterArray} object.
#' @param value \code{character} vector.
#' 
#' @examples
#' # an example
#' @exportMethod "[[<-"
setReplaceMethod(
	"[[", 
	signature(x="RasterArray"),
	function(x,i, value){
		x@stack[[i]] <- value
		return(x)
	}
)
