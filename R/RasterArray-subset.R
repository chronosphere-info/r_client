# Functions to subset and replace items in a RasterArray

#' Subset a RasterArray object
#' 
#' Extract subsets of RasterArray class object similarly to a regular array. 
#' 
#' @param x \code{RasterArray} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \code{RasterArray} wrapper be dropped and the element be reduced to a single \code{RasterLayer}?
#' @param oneDim \code{logical} In case of multidimensional \code{RasterArray}s, setting \code{oneDim} to \code{TRUE} allows the application of one dimensional subscripts.  
#' 
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
	signature(x="RasterArray"), 
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
				x@index[!bNA] <- 1:raster::nlayers(x@stack)

				return(x)
			}		
	}
)

#' Indexing to extract subsets of a RasterArray object
#'
#' Single bracket \code{'['} refers to indices and names within the \code{RasterArray}. Use double brackets to extract layers based on their names (in the stack).
#' 
#' @param x \code{RasterArray} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \code{RasterArray} wrapper be dropped and the element be reduced to a single \code{RasterLayer}?
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
	signature(x="RasterArray", i="ANY", j="ANY"),
	definition=function(x,i,j,..., drop=TRUE){
		sysCall <- sys.call(which=-1)

		oneDim<-FALSE
		if(length(sysCall)==3){
			oneDim <- TRUE
		}
		subset(x,i,j,..., oneDim=oneDim, drop=drop)
		
	}
)

#' Replace layers of a RasterArray object
#' 
#' Single bracket \code{'['} refers to indices and names within the \code{RasterArray}. Use double brackets to replace layers based on their names (in the stack).
#'
#' @param x \code{RasterArray} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param value A \code{RasterLayer} or \code{RasterArray} object.
#' @examples
#' data(dems)
#' # replace third element with missing value
#' dems[3] <- NA
#' # duplicate first element and make it the second too
#' dems[2] <-dems[1]
#' 
#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="RasterArray", value="logical"),
	definition=function(x,i,j,..., value){
		# fetch the index
		indDim <- dim(x@index)

		sysCall <- sys.call(which=-1)

		if(sum(is.na(value))!=length(value)) stop("Invalid replacement type.")
		if(is.null(indDim) | length(indDim)==1){
			if(length(i)!=length(value) & length(value)!=1) stop("Invalid replacement length.")
			theIndex <- x@index[i]
			x@index[i] <- NA

		}
		
		# multi- dim case
		if(length(indDim)>=2){
			# one dimensinoal 
			if(length(sysCall)==4){
				theIndex <- x@index[i]
				x@index[i] <- NA
			}else{
				theIndex <- x@index[i,j,...]
				x@index[i,j,...] <- NA
			}
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

#' @rdname replacementSingle
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


#' Indexing to extract RasterLayers of a RasterArray object
#'
#' Double bracket \code{'[['} refers to layers' name in the \code{RasterStack} of the \code{RasterArray}. Use single brackets to extract elements based on their position in the \code{RasterArray}.
#' 
#' @return The function returns either a single \code{RasterLayer}, a \code{RasterStack} or a list of \code{RasterLayers}. 
#' @param x \code{RasterArray} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param drop \code{logical} should the \code{RasterStack} be dropped and the element be reduced to a single \code{RasterLayer}?
#' @exportMethod "[["
#' @examples
#' data(dems)
#' # finds a layer
#' dems[["dem_30"]]
#' # returns a stack
#' dems[[c("dem_0", "dem_15")]]
#' # replaces a layervalues, but not the attributes of the layer
#' dem2 <- dems
#' dem2[["dem_0"]] <- dem2[["dem_5"]]
#' # compare every value in the 0 and 5 ma maps, they are all the same
#' mean(values(dem2[["dem_0"]]==dem2[["dem_5"]]))
setMethod(
	"[[", 
	signature(x="RasterArray"),
	function(x,i,drop=TRUE){
		# where are NAs in the subscrtip
		bNA <- is.na(i)
		if(sum(bNA)==length(i)) return(i)

		# logical method
		if(is.logical(i)){
			if(length(i)!=length(x)) stop("Invalid subscript length.")
			
			# stack subscript
			usedInd <- i
			usedInd[bNA] <- FALSE
			
			#select appropriate layers
			newStack<- x@stack[[which(usedInd), drop=FALSE]]

			# index subscript
			newIndex <- x@index[i]
			newIndex[!is.na(newIndex)] <- 1:sum(!is.na(newIndex))
		}

		# either character or numeric
		if(is.character(i) | is.numeric(i)){
			#select appropriate layers
			newStack<- x@stack[[i[!bNA], drop=FALSE]]

			# reindex
			newIndex <- rep(NA, length(i))
			newIndex[!bNA] <- 1:nlayers(newStack)
		}

		final <- RasterArray(index=newIndex, stack=newStack)
		
		if(drop){
			if(length(final)==1){
				final <- final@stack[[1]]
			}
		}

		return(final)
	}
)

#' Replace RasterLayers in a RasterArray object
#'
#' Double bracket \code{'[['} refers to layers' name in the \code{RasterStack} of the \code{RasterArray}. Use single brackets to replace elements based on their position in the \code{RasterArray}.
#' 
#' @param x \code{RasterArray} object.
#' @param i subscript of layers to replace.
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
