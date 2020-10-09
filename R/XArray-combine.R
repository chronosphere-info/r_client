#' Combine RasterLayers and one-dimensional RasterArrays and Spatial* objects with one-dimensinoal SpatialArrays
#'
#' Methods sequences that start with an NA do not yet work. 
#' @rdname combine
#' @param x \code{RasterLayer} or \code{RasterArray} objects or Spatial* and \code{\link{SpatialArray}} objects to combine.
#' @return A \code{\link{RasterArray}} or \code{\link{SpatialArray}}class object.
#' @param ... additional objects to combine. 
#' @examples
#' data(dems)
#' a <- combine(dems[1], dems[2])
#' @export 
setGeneric("combine", function(x,...) standardGeneric("combine"))


#' @rdname combine
#' @export 
setMethod(
	"combine",
	"XArray",
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
				if(class(elem)=="RasterLayer" | inherits(elem, "Spatial")){
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


# pairwise generic
setGeneric("c2", function(x,y,...) standardGeneric("c2"))



# general cbind2 method 
setMethod("cbind2", c("XArray","XArray"),
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

	
	# reconstruct the XArray
	if(class(newstack)=="RasterStack"){
		newRA<- RasterArray(stack=newstack, index=newindex)
	}else{
		newRA<- SpatialArray(stack=newstack, index=newindex)
	}

	return(newRA)

})



# general rbind2 method
setMethod("rbind2", c("XArray","XArray"),
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
		newindex[!is.na(newindex)] <- 1:nlayers(newstack)

	# and the column names
	rownames(newindex) <- theNames

	
	# reconstruct the RasterArray
	if(class(newstack)=="RasterStack"){
		newRA <- RasterArray(stack=newstack, index=newindex)
	}
	if(class(newstack)=="SpatialStack"){
		newRA <- SpatialArray(stack=newstack, index=newindex)
	}

	return(newRA)

})
