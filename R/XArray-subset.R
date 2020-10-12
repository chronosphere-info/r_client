
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
#' data(coasts)
#' subset(coasts, i=2, j=1:2)
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
#' data(coasts)
#' present<- coasts["0", ]
#' allMargin <- coasts[, "margin"]
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


#' Indexing to extract \code{RasterLayer}s of a \code{\link{RasterArray}} or \code{Spatial*} of a \code{\link{SpatialArray}} object
#'
#' Double bracket \code{'[['} refers to layers' name in the \code{RasterStack} of the \code{RasterArray} or the \code{SpatialStack} of the \code{SpatialArray}. Use single brackets to extract elements based on their position in the \code{\link{RasterArray}} or \code{\link{SpatialArray}}
#' 
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param drop \code{logical} should the \code{RasterStack} be dropped and the element be reduced to a single \code{RasterLayer}?
#' @return A \code{RasterLayer} or \code{RasterArray} class object.
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
	signature(x="XArray"),
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

		# depending on type of object
		if(class(newStack)=="RasterStack"){
			final <- RasterArray(index=newIndex, stack=newStack)
		}

		if(class(newStack)=="SpatialStack"){
			final <- SpatialArray(index=newIndex, stack=newStack)
		}

		if(drop){
			if(length(final)==1){
				final <- final@stack[[1]]
			}
		}

		return(final)
	}
)



#' Replace layers of a \code{\link{RasterArray}} or \code{\link{SpatialArray}} object
#' 
#' Single bracket \code{'['} refers to indices and names within the \\code{\link{RasterArray}} or \code{\link{SpatialArray}}. Use double brackets to replace layers based on their names (in the stack).
#' \code{RasterLayers} and \code{\link{RasterArray}} entries can be used to replace values in \code{\link{RasterArray}}s. \code{Spatial*} objects and \code{\link{SpatialArray}}s can be used with \code{\link{SpatialArray}}s.
#'
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param value A same class object as \code{x}.
#' @return None.
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
	signature(x="XArray", value="logical"),
	definition=function(x,i,j,..., value){
		# fetch the index
		indDim <- dim(x@index)

		# this should only apply to NA
		if(any(!is.na(value))) stop("Replacement with TRUE/FALSE is not valid.")

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



# Generalized layer replacement function for XArray. Method dispatch written explicitly as RasterArray[ <- RasterLayer and SpatialArray [<- Spatial*
XArrayReplaceLayer <- function(x,i,j,value,...){
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
			
			if(inherits(value, "Raster")){
				# add the new layer to the stack
				newStack <- stack(x@stack, value[[rep(1, length(totallyNew))]])
			}else{
				# create a new stack
				newStack <- SpatialStack(Spatials=c(x@stack@Spatials, rep(list(value), length(totallyNew))))
			}

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
			
			# stacking is dependent on the kind of Array
			if(inherits(value, "Raster")){
				# put the additional elements to the stack
				newStack <- stack(x@stack[[origVals]], value[[rep(1, length(replaceIndex))]])
			}else{
				# create a new stack
				newStack <- SpatialStack(Spatials=c(x@stack[[origVals]]@Spatials, rep(list(value), length(replaceIndex))))
			}
			
			# reorder to correct
			x@stack <- newStack[[tempInd2]]
		
		}
	}

	return(x)

}


#' Replace \code{RasterLayer}s in a \code{\link{RasterArray}} object and \code{Spatial*} objects in a \code{\link{SpatialArray}} object.
#'
#' Double bracket \code{'[['} refers to layers' name in the \code{RasterStack} of the \code{RasterArray} and the \code{\link{SpatialStack}} of the \code{\link{SpatialArray}}. Use single brackets to replace elements based on their position in the \code{RasterArray}/\code{\link{SpatialArray}}.
#' 
#' @param x \code{\link{RasterArray}} or \code{\link{SpatialArray}} object.
#' @param i subscript of layers to replace.
#' @param value \code{character} vector.
#' @return None.
#' 
#' @rdname doubleBracketReplace
#' @exportMethod "[[<-"
setReplaceMethod(
	"[[", 
	signature(x="XArray"),
	function(x,i, value){
		x@stack[[i]] <- value
		return(x)
	}
)

