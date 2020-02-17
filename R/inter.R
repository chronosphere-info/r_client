#' Match the dates of a time-dependent variable with a predefined vector
#' 
#' The function takes a variable \code{x} (e.g. a vector or a \code{\link[chronosphere:RasterArray-class]{RasterArray}} object), and reorders it to best match the dates provided in a vector \code{y}.
#' 
#' @param x Object to be reordered to match \code{y}.
#' 
#' @param y (\code{numeric}) The vector of dates (numeric values) to order to.
#' 
#' @param index (\code{logical}) If this argument is \code{TRUE}, only the indices will be returned that refer to the new order, rather than the reordered \code{x} variable.
#' 
#' @param time \code{numeric}. Single value referring to that dimension of \code{x} where the time-coding names are (\code{time=1} is the default for RasterArrays in \code{chronosphere}).
#' @param ... Additional arguments passed to class-specific methods.
#' @rdname matchtime
#' @return An object of the class as \code{x} or a \code{numeric} vector.
#' @examples
#' # original vector
#' orig <- 1:10
#' # target values
#' targ <- c(5.1,4.2, 3.4, 2.7, 2.3)
#' # how do the two series match the best?
#' matchtime(orig, targ)
#' @exportMethod matchtime
setGeneric("matchtime", function(x,y,...) standardGeneric("matchtime"))

#' @rdname matchtime
setMethod(
	"matchtime", 
	signature="numeric",
	function(x, y, index=FALSE, ...){

	newIndex <- rep(NA, length(y))

	for(i in 1:length(y)){
		absDiff <- abs(y[i]-x)
		# which is closest
		newIndex[i] <- which(min(absDiff)==absDiff)[1]
	}

	if(!index){
		return(x[newIndex])
	}else{
		return(newIndex)
	}
})

#' @rdname matchtime
setMethod(
	"matchtime", 
	signature="character",
	function(x, y, index=FALSE, ...){
		a<-as.numeric(x)
		newIndex<- matchtime(x=a,y=y,index=TRUE)

	if(!index){
		return(x[newIndex])
	}else{
		return(newIndex)
	}
})

#' @rdname matchtime
setMethod(
	"matchtime", 
	signature="RasterArray",
	function(x, y, index=FALSE,time=1, ...){
		# get the age identifiers
		# in the first names
		dimensions<- dim(x)
		if(length(dimensions)==1){
			age<-names(x)
		}else{
			age<-dimnames(x)[[time]]
		}

		newIndex<- matchtime(x=age,y=y, index=TRUE)

	if(index){
		return(newIndex)
	}else{
		# non-standard evalution to mathc the number of dimensions
		dl <- length(dimensions)
		if(dl==1){
			return(x[newIndex])
		}else{
			# make the subscript so it matches the dimensions
				# empty for all dimensions
				empty<-rep("", dl)
				# where is the time dimension - add subscript
				empty[time] <- paste("c(", paste(newIndex, collapse=","), ")", sep="")
				# construct the dimensions matching subscript stirng
				subscript <- paste(empty,collapse=",")
				# the rest of the call
				theexp <- paste("x[", subscript, "]", sep="")
			
			# execute call
			return(eval(parse(text=theexp)))
		}

	}
})

