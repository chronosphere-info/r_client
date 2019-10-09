#' Match the dates of a time-dependent variable with a predefined vector
#' 
#' The function takes a variable \code{x} (e.g. a vector or a RasterArray object), and reorders it to best match the dates provided in a vector \code{y}.
#' 
#' @param x Object to be reordered to match \code{y}.
#' 
#' @param y (\code{numeric}) The vector of dates (numeric values) to order to.
#' 
#' @param index (\code{logical}) If this argument is \code{TRUE}, only the indices will be returned that refer to the new order, rather than the reordered \code{x} variable.
#' 
#' @param time \code{numeric}. Single value referring to that dimension of \code{x} where the time-coding names are (\code{time=1} is the default for RasterArrays in \code{chronosphere}).
#' @rdname matchtime
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


#' Extraction of values from a multiple RasterLayers
#' 
#' The function takes a set of time-dependent coordinates and extracts the value they point to from associted RasterLayers in a RasterArray.
#' 
#' @param x (\code{matrix} or \code{data.frame}). The data table containing the coordinates and (optionally) 
#' 	the indices or names of the associated \code{RasterLayer}s in \code{y}.
#' @param y (\code{RasterArray}). A set of \code{RasterLayers} that are associated with the rows of \code{x}.
#' @param by (\code{character} or \code{vector}) The link between \code{x} and \code{y}. If \code{by} is a \code{character}
#' string then it is expected to be column of \code{x} and should contain the \code{names} or the indices of the
#' associated \code{RasterLayer}s in \code{y}. If it is a \code{vector} its length should match the number of rows in \code{x}
#' and it will be used as if it were a column of \code{x}.
#' @param lng (\code{character}) A column of \code{x} that includes the paleolongitudes.
#' @param lat (\code{character}) A column of \code{x} that includes the paleolatitudes.
#' @param force (\code{character}) If set to \code{"numeric"} the \code{by} argument or the column it points to will be converted to
#' numeric values, and y will be subsetted with \code{numeric} subscripts of the \code{y} \code{RasterArray}. If set to character, the by column (or vector) will be
#' forced to \code{character} values and will be used as character subscripts.
#' 
#' @export
multiextract <- function(x, y, by, lng="plng", lat="plat", force=NULL){
	
	# defense
	if(!is.data.frame(x) & !is.matrix(x)) stop("The argument x has to be a data.frame or matrix.")
	if(class(y)!="RasterArray") stop("The argument y has to be a one dimensional RasterArray.")

	# column that contains which map the coordinate belongs to
	if(is.character(by)){
		if(!by%in%colnames(x)) stop("The argument by has to be a column of x. ")
		interactor <- x[, by]
	# separate vector
	}else{
		if(is.vector(by) & (length(by)==nrow(x))){
			interactor <- by
		}else{
			stop("Invalid by argument.")
		}
	}
	
	# force subscript type
	if(!is.null(force)){
		if(force=="numeric"){
			interactor <- as.numeric(as.character(interactor))
		}
		if(force=="character"){
			interactor <- as.character(interactor)
		}
	}

	# iterate by
	doFor <- sort(unique(interactor))

	# coordinates
	coords <- x[, c(lng, lat)]

	# storage
	vals <- rep(NA, nrow(x))

	for(i in 1:length(doFor)){
		bThis<- doFor[i]==interactor

		# select the appropriate RasterLayer
		thisLayer <- y[doFor[i]]

		# extract values from raster
		vals[bThis] <- raster::extract(thisLayer, coords[bThis, ])
	}
	names(vals) <- rownames(x)
	return(vals)

}

