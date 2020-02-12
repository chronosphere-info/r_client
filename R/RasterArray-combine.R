# Functions to combine items that result in a RasterArray

###############################################################################
# combine()


#' Combine RasterLayers and one-dimensional RasterArrays
#'
#' Methods sequences that start with an NA do not yet work. 
#' @rdname combine
#' @param x \code{RasterLayer} or \code{RasterArray} object to combine.
#' @return A \code{RasterArray} class object.
#' @param ... additional objects to combine. 
#' @examples
#' data(dems)
#' a <- combine(dems[1], dems[2])
#' @export 
setGeneric("combine", function(x,...) standardGeneric("combine"))


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


#' @rdname combine
#' @export 
setMethod(
	"combine",
	"RasterArray",

	#c.RasterArray<-
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

# pairwise generic
setGeneric("c2", function(x,y,...) standardGeneric("c2"))

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

#' Combine RasterLayers or RasterArrays by rows or columns
#' 
#' The function takes a sequence of \code{RasterLayer} or \code{RasterArray} class objects and combines them to two dimensional \code{RasterArrays}...
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
#' @return A \code{RasterArray} class object.
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

setMethod("cbind2", c("RasterArray","RasterArray"),
function(x,y, deparse.level=1){
#	deparse.level<-1
	# x's names
	if(is.null(dim(x@index))){
		xnames <- names(x)
	}else{
		xnames <- rownames(x)
	}

	# y's names
	if(is.null(dim(y@index))){
		ynames <- names(y)
	}else{
		ynames <- rownames(y)
	}
	
	# Raster properties doesn't match then these should not be in the same array!!!
	origColsX <- colnames(x)
	origColsY <- colnames(y)


	# this needs soxme work
	if(deparse.level!=0){
		# get call (symbol)
		callSymb <- sys.call(which=deparse.level)
		# make a dummy to avoid errors...
		if(length(callSymb)<3) callSymb <- rep(NA, 3)

		if(is.null(origColsX)){
			xSymb <- callSymb[[2]]
			if(is.symbol(xSymb)){
				colsX <-deparse(xSymb)
			}else{
				colsX<-NA
			}
			
		}else{
			colsX <- origColsX
		}
		if(is.null(origColsY)){
			ySymb <- callSymb[[3]]
			if(is.symbol(ySymb)){
				colsY <- deparse(ySymb)
			}else{
				colsY<- NA
			}
			
		}else{
			colsY <- origColsY
		}

		# the names of the arguments
		theNames <- c(colsX, colsY)

		# delete colnames if they are only NAs
		if(!any(!is.na(theNames))) theNames <- NULL

	}else{
		theNames <- NULL
	}

	# in case the names do not match
	needForce <- FALSE
	if(length(xnames)!=length(ynames)){
		needForce <- TRUE
	}else{
		if(sum(xnames == ynames)!=length(xnames)){
			needForce <- TRUE
		}
	}

	if(needForce){	
		
		# create a uniform order
		allNames <- unique(c(xnames, ynames))
		# try to sort numerically first
		suppressWarnings(tempor <- as.numeric(allNames))
		if(any(is.na(tempor))){
			jointNames <- sort(allNames)
		}else{
			jointNames <- as.character(sort(tempor))
		}
		
		if(length(dim(x))==1){
			newX <- x[jointNames]
			names(newX@index) <- jointNames
		}else{
			newX<-x
			newX@index <- newbounds(x@index, rows=jointNames)
		}
		
		if(length(dim(y))==1){
			newY <- y[jointNames]
			names(newY@index) <- jointNames
		}else{
			newY<-y
			newY@index <- newbounds(y@index, rows=jointNames)
		}
		x<-newX
		y<-newY
		warning("The arguments have different rownames, rows are forced to match.", call.=FALSE)
		
	}

	# create a new stack
	newstack<- stack(x@stack, y@stack)

	# the index
	# stacks of ycontinue after the stacks of x
		offset <- nlayers(x)
		y@index<-y@index+offset
	
	# create index object
		newindex <- cbind(x@index, y@index)
		

	# and the column names
	colnames(newindex) <- theNames

	
	# reconstruct the RasterArray
	newRA<- RasterArray(stack=newstack, index=newindex)

	return(newRA)

})

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


setMethod("rbind2", c("RasterArray","RasterArray"),
function(x,y, deparse.level=1){
#	deparse.level<-1
	# x's names
	if(is.null(dim(x@index))){
		xnames <- names(x)
	}else{
		xnames <- colnames(x)
	}

	# y's names
	if(is.null(dim(y@index))){
		ynames <- names(y)
	}else{
		ynames <- colnames(y)
	}
	
	# Raster properties doesn't match then these should not be in the same array!!!
	origRowsX <- rownames(x)
	origRowsY <- rownames(y)

	# this needs soxme work
	if(deparse.level!=0){
		# get call (symbol)
		callSymb <- sys.call(which=deparse.level)
		# make a dummy to avoid errors...
		if(length(callSymb)<3) callSymb <- rep(NA, 3)

		if(is.null(origRowsX)){
			xSymb <- callSymb[[2]]
			if(is.symbol(xSymb)){
				rowsX <-deparse(xSymb)
			}else{
				rowsX<-NA
			}
		}else{
			rowsX <- origRowsX
		}

		if(is.null(origRowsY)){
			ySymb <- callSymb[[3]]
			if(is.symbol(ySymb)){
				rowsY <-deparse(ySymb)
			}else{
				rowsY<-NA
			}
		}else{
			rowsY <- origRowsY
		}

		# the names of the arguments
		theNames <- c(rowsX, rowsY)

		# delete colnames if they are only NAs
		if(!any(!is.na(theNames))) theNames <- NULL

	}else{
		theNames <- NULL
	}

	# in case the names do not match
	needForce <- FALSE
	if(length(xnames)!=length(ynames)){
		needForce <- TRUE
	}else{
		if(sum(xnames == ynames)!=length(xnames)){
			needForce <- TRUE
		}
	}

	if(needForce){	
		
		# create a uniform order
		allNames <- unique(c(xnames, ynames))
		# try to sort numerically first
		suppressWarnings(tempor <- as.numeric(allNames))
		if(any(is.na(tempor))){
			jointNames <- sort(allNames)
		}else{
			jointNames <- as.character(sort(tempor))
		}
		
		if(length(dim(x))==1){
			newX <- x[jointNames]
			names(newX@index) <- jointNames
		}else{
			newX<-x
			newX@index <- newbounds(x@index, cols=jointNames)
		}
		
		if(length(dim(y))==1){
			newY <- y[jointNames]
			names(newY@index) <- jointNames
		}else{
			newY<-y
			newY@index <- newbounds(y@index, cols=jointNames)
		}
		x<-newX
		y<-newY
		warning("The arguments have different colnames, columns are forced to match.", call.=FALSE)
		
	}

	# create a new stack
	newstack<- stack(x@stack, y@stack)

	# the index
	# stacks of ycontinue after the stacks of x
		offset <- nlayers(x)
		y@index<-y@index+offset
	
	# create index object
		newindex <- rbind(x@index, y@index)

	# reorder the stack!
		stackIndex<- as.numeric(newindex)
		stackIndex<-stackIndex[!is.na(stackIndex)]
		newstack <- newstack[[stackIndex]]
		newindex[!is.na(newindex)] <- 1:raster::nlayers(newstack)

	# and the column names
	rownames(newindex) <- theNames

	
	# reconstruct the RasterArray
	newRA<- RasterArray(stack=newstack, index=newindex)

	return(newRA)

})
