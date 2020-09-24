
#' Replace layers of a RasterArray object
#' 
#' Single bracket \code{'['} refers to indices and names within the \code{RasterArray}. Use double brackets to replace layers based on their names (in the stack).
#'
#' @param x \code{RasterArray} object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param value A \code{RasterLayer} or \code{RasterArray} object.
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


#' Replace RasterLayers in a RasterArray object
#'
#' Double bracket \code{'[['} refers to layers' name in the \code{RasterStack} of the \code{RasterArray}. Use single brackets to replace elements based on their position in the \code{RasterArray}.
#' 
#' @param x \code{RasterArray} object.
#' @param i subscript of layers to replace.
#' @param value \code{character} vector.
#' @return None.
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
