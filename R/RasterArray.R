#' @import raster

# class declaration
setClassUnion("arrayORmatrixORvector", c("vector", "matrix", "array"))

#' Virtual Array of RasterLayers
#' 
#' Array template for RasterLayers
#' 
#' The class implements structures to organize RasterLayers that have the same dimensions. Subsetting rules were defined using the proxy object in the \code{@index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: RasterStack, the actual data.
#' proxy: A proxy object that represents the organization of the layers. 
#' 
#' Currently the following methods are implemented - documentation will come later:
#' 
#'   show: type in the name to the console.
#'
#'   subset: needed for [
#'
#'   "[": behaves just like a matrix, or array. drop=TRUE keeps RasterArray, even if there is only one layer. x[i] doesn't yet work for multidimensional containers. 
#'
#'   "[[": just like RasterStack
#'
#'   "colnames": names of the second dim.
#'
#'   "rownames":names of the first dim.
#'
#'   "dim": dimensions of the array. Unlike the method of RasterStack, this doesn't include the layer dimensions.
#'
#'   "dimlayer": dimensions of the RasterLayers.
#'
#'   "dimnames": Names in all dimensions. I am thinking about naming the dimensions in some way (e.g. time, depth, var), so we can do subsetting with them. 
#'
#'   "nrow": Number of rows in the array. Unlike nrow for the RasterLayers.
#'
#'   "ncol": Number of columns in the array. Unlike ncol for the RasterLayers.
#'   
#'   "maxValue": outputs maximum value in RasterLayers - keeping the structure of proxy.
#'   
#'   "minValue": outputs minimum value in RasterLayers - keeping the structure of proxy.
#'   
#'   "res", "yres", "xres" are the same as for RasterStack.
#'   
#' 
#' There are only two contructor processes defined, these should get us through the initial developmental phases.
#' There is no validity testing yet, that is still to be implemented.
#' The plot method will be directed with the proxy object. 
#' 
#' @param stack A \code{RasterStack} class object.
#' @param proxy A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @examples
#' # data import
#'	library(raster) - maybe we should attach this automatically
#'	library(chronosphere)
#'   data(clim)
#'   st <-stack(clim)
#' 
#' # class definition
#'   # 1d vector of rasters
#'   ind <- 1:3
#'   names(ind) <- names(st)
#'   
#'   first <- RasterArray(stack=st, index=ind)
#'   
#'   # 2d matrix of rasters
#'   ind2 <- matrix(1:6, ncol=2, nrow=3)
#'   rownames(ind2) <- names(st)
#'   colnames(ind2) <- c("orig", "plus")
#'   stPlus <- st+1
#'   
#'   second <- RasterArray(stack(st, stPlus), index=ind2)
#'   
#'   # 3d array of rasters
#'   ind3 <- array(1:prod(3,2,4), dim=c(3,2,4), dimnames=list(rownames(ind2), colnames(ind2), letters[1:4]))
#'   
#'   stA<- second@stack
#'   stB<- second@stack+100
#'   stC<- second@stack+200
#'   stD<- second@stack+300
#'   
#'   third <- RasterArray(stack(stA, stB,stC,stD), index=ind3)
#' 
#' # example subsetting
#' # one dimensional RasterArray
#'   # direct application, logical subsetting
#'   subset(first, c(TRUE, FALSE, TRUE))
#'   
#'   # access by index - dropping the structure
#'   first[3] 
#' 
#'   # acces by name - dropping strucutre
#'   first["X0"]
#'   first["X4"]
#'   
#'   # access by index - no drop
#'   first[3, drop=FALSE]
#'   # access by name, dropping
#'   first["X0", drop=FALSE]
#'   first["X4", drop=FALSE]
#' 
#' 
#' # two dimensional RasterArray
#'   second[1] # doesn't work - need fix
#'   second[,2] # second column
#'   
#'   # single element, 
#'   second["X0","orig"]
#'   second["X0","plus"]
#'   
#'   # single element, without dropping
#'   second["X0","orig", drop=FALSE]
#'   second["X0","plus", drop=FALSE]
#'   
#'   # list type access?
#'   second[[1]]
#' 
#' # three dimensional RasterArray
#'   third[1] # doesn't work - need fix!
#'   
#'   # first plane
#'   third[,,1]
#'   
#'   # second plane
#'   third[,,2]
#'   
#'   # first row-plane
#'   third[1,,]
#'   
#'   third[, 2,]
#' @exportClass RasterArray
RasterArray <- setClass("RasterArray", slots=list(index="arrayORmatrixORvector", stack="RasterStack"))

# constructor generic
# constructor note: should force the name of the layers in the stack!

# build from existing stack with existing index, or dimensions
#' @export RasterArray
setMethod("initialize",signature="RasterArray",
	definition=function(.Object,stack, index=NULL, dim=NULL){

		if(is.null(dim)){ 
			.Object@index <- index
			.Object@stack <- stack
			
		}else{
			if(nlayers(x)==prod(index)){
				.Object@stack<- stack
				index <- array(1:nlayers(x), dim=dim)
				.Object@index<- index
			}

		}
		return(.Object)
	}
)


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

#' @exportMethod [[
setMethod(
	"[[", 
	signature(x="RasterArray"),
	function(x,i,drop=TRUE){
		x@stack[[i, drop=drop]]
	}
)


setMethod(
	"show",
	signature="RasterArray", 
	function (object) 
	{
	    cat("class         :", class(object), "\n")
	    if (rotated(object)) {
	        cat("rotated     : TRUE\n")
	    }
	    mnr <- 15
	#   if (filename(object) != "") {
	#       cat("filename    :", filename(object), "\n")
	#   }


	    nl <- nlayers(object)
	    if (nl == 0) {
	        cat("nlayers     :", nl, "\n")
	    } else {
	   		cat("RasterLayer properties: \n")
	       cat("- dimensions  : ", paste(dimlayer(object), collapse=", "), 
	            "  (nrow, ncol)\n", 
	        sep = "")

	        cat("- resolution  : ", xres(object), ", ", yres(object), 
	            "  (x, y)\n", sep = "")
	        cat("- extent      : ", object@stack@extent@xmin, ", ", object@stack@extent@xmax, 
	            ", ", object@stack@extent@ymin, ", ", object@stack@extent@ymax, 
	            "  (xmin, xmax, ymin, ymax)\n", sep = "")
	        cat("- coord. ref. :", projection(object@stack, TRUE), "\n")
  			cat("Array properties: \n")
  			adim <- dim(object)
  			allName <- names(object)
		   
	        if(length(adim)==1){
		        cat("- dimensions   : ", paste(adim, collapse=", "), 
		            "  (vector)\n", 
		            sep = "")
		      
		    }else{
		    	allName<- dimnames(object)
		    	if(length(allName)==2){
			    	cat("- dimensions  : ", paste(adim, collapse=", "), 
			            "  (nrow, ncol)\n", 
			            sep = "")
			    }else{
			    	cat("- dimensions  : ", paste(adim, collapse=", "), 
			            "  (nrow, ncol, ...)\n", 
			            sep = "")
			    }
		#    	for(i in 1:length(allName)){
		#			if(i==1) cat("- rownames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#			if(i==2) cat("- colnames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#			if(i>2) cat(paste("- Dim", i, " names", sep=""), "  : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#    	}
				

		    	  
		    }
		    cat("- num. layers    : ", nlayers(object), "\n", 
		        sep = "")
		    cat("- proxy:\n ")
		    print(proxy(object))
		   
	    }
	    cat("\n")
	}
)




#' @exportMethod length
setMethod(
	"length",
	signature="RasterArray",
	function(x) length(x@index)
)

#' @exportMethod ncell
setMethod(
	"ncell",
	signature="RasterArray",
	function(x) ncell(x@stack)
)

#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="RasterArray",
	function(x) nlayers(x@stack)
)

#' @exportMethod colnames
setMethod(
	"colnames",
	signature="RasterArray",
	function(x) colnames(x@index)
)

#' @exportMethod rownames
setMethod(
	"rownames",
	signature="RasterArray",
	function(x) rownames(x@index)
)

#' @exportMethod names
setMethod(
	"names",
	signature="RasterArray",
	function(x){	
		names(x@index)
	}
)

#' @exportMethod dimnames
setMethod(
	"dimnames",
	signature="RasterArray",
	function(x) dimnames(x@index)
)

#' @exportMethod layers
setGeneric("layers", function(x,...) standardGeneric("layers"))


setMethod(
	"layers", 
	signature="RasterArray", 
	function(x){
		# returns the layer names
		names(x@stack)

	} 
)


#' @exportMethod nvalues
setGeneric("nvalues", function(x,...) standardGeneric("nvalues"))


setMethod(
	"nvalues", 
	signature="RasterArray", 
	function(x){
		# returns the layer names
		length(x@stack)

	} 
)



#' @exportMethod dimlayer
setGeneric("dimlayer", function(x,...) standardGeneric("dimlayer"))

#' @exportMethod dim
setMethod(
	"dim", 
	signature="RasterArray", 
	function(x){
		proxyDim <- dim(x@index)
		if(is.null(proxyDim)) proxyDim <- length(x@index)
		proxyDim
	} 
)


setMethod(
	"dimlayer", 
	signature="RasterArray", 
	function(x){
		# depends on subset-method
		dim(x@stack[[1]])[1:2]

	} 
)


#' @exportMethod ncol
setMethod(
	"ncol", 
	signature="RasterArray", 
	function(x){
		ncol(x@index)
	} 
)

#' @exportMethod nrow
setMethod(
	"nrow", 
	signature="RasterArray", 
	function(x){
		nrow(x@index)
	} 
)

#' @exportMethod xres
setMethod(
	"xres",
	signature="RasterArray",
	function(x){
		xres(x@stack)
	} 
)


#	setMethod(
#		"filename",
#		signature="RasterArray",
#		function(x){
#			filename(x@stack)
#		} 
#	)

#' @exportMethod yres
setMethod(
	"yres",
	signature="RasterArray",
	function(x){
		yres(x@stack)
	} 
)

#' @exportMethod res
setMethod(
	"res",
	signature="RasterArray",
	function(x){
		res(x@stack)
	} 
)

#' @exportMethod minValue
setMethod(
	"minValue",
	signature="RasterArray",
	function(x, vec=FALSE){
		vals <- minValue(x@stack)
		if(!vec){
			ret <- x@index
			ret[]<- vals[ret]
			ret
		}else{
			vals
		}
	} 
)

#' @exportMethod maxValue
setMethod(
	"maxValue",
	signature="RasterArray",
	function(x, vec=FALSE){
		vals <- maxValue(x@stack)
		if(!vec){
			ret <- x@index
			ret[]<- vals[ret]
			ret
		}else{
			vals
		}
	} 
)


#' The proxy of a RasterArray or SpArray object
#' 
#' This function extracts the @index slot of a RasterArray or SpArray-class object
#' 
#' @param x (\code{RasterArray} or \code{SpArray}) focal object.
#' @exportMethod proxy
setGeneric("proxy", function(x,...) standardGeneric("proxy"))
setMethod(
	"proxy",
	signature="RasterArray",
	function(x){
		ind <- x@index
		
		# only NAs are present
		if(any(!is.na(ind))){
			if(!is.null(names(x@stack))) ind[]<- names(x@stack)[ind]
		}
		
		return(ind)
	}

)

#' S3-type method for RasterArray allowing View() to work.
#' @export
as.data.frame.RasterArray<- function(from, to="data.frame"){
	df <- as.data.frame(proxy(from))
	if(ncol(df)==1) colnames(df) <- "X0"
	return(df)
}


#' @export cbind.RasterArray
cbind.RasterArray<-function(..., deparse.level=1){
		listArg <- list(...)
		finRA <- listArg[[1]]
		for(i in 2:length(listArg)){
			finRA<-cbind2(finRA, listArg[[i]], deparse.level=deparse.level)
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
	if(deparse.level==1){
		# get call
		theCall<- as.list(sys.call(which=1))
		if(is.null(origColsX)){
			colsX <- as.character(theCall[[2]])
		}else{
			colsX <- origColsX
		}
		if(is.null(origColsY)){
			colsY <- as.character(theCall[[3]])
		}else{
			colsY <- origColsY
		}

		# the names of the arguments
		theNames <- c(colsX, colsY)
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
	if(deparse.level==1) colnames(newindex) <- theNames

	
	# reconstruct the RasterArray
	newRA<- RasterArray(stack=newstack, index=newindex)

	return(newRA)

})


#' @export rbind.RasterArray
rbind.RasterArray<-function(..., deparse.level=1){
		listArg <- list(...)
		finRA <- listArg[[1]]
		for(i in 2:length(listArg)){
			finRA<-rbind2(finRA, listArg[[i]], deparse.level=deparse.level)
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
	if(deparse.level==1){
		# get call
		theCall<- as.list(sys.call(which=1))
		if(is.null(origRowsX)){
			rowsX <- as.character(theCall[[2]])
		}else{
			rowsX <- origRowsX
		}
		if(is.null(origRowsY)){
			rowsY <- as.character(theCall[[3]])
		}else{
			rowsY <- origRowsY
		}

		# the names of the arguments
		theNames <- c(rowsX, rowsY)
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
	if(deparse.level==1) rownames(newindex) <- theNames

	
	# reconstruct the RasterArray
	newRA<- RasterArray(stack=newstack, index=newindex)

	return(newRA)

})

####################################################################
# Changing methods

#' @exportMethod resample
setMethod(
	"resample",
	signature=c("RasterArray", "ANY"),
	function(x,y,...){
		x@stack <- stack(resample(x@stack, y,...))
		return(x)
	}
)

#' @exportMethod crop
setMethod(
	"crop",
	signature=c("RasterArray"),
	function(x,y,...){
		x@stack <- stack(crop(x@stack,y,...))
		return(x)
	}
)


#' @exportMethod aggregate
setMethod(
	"aggregate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- stack(aggregate(x@stack,...))
		return(x)
	}
)

#' @exportMethod disaggregate
setMethod(
	"disaggregate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- stack(disaggregate(x@stack,...))
		return(x)
	}
)
