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
#' index: A proxy object that represents the organization of the layers. 
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
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{index} procedurally.
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
				proxy <- array(1:nlayers(x), dim=index)
				.Object@index<- proxy
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
				x<- x@stack[[fetchIndex]]

			# keep using RasterArray
			}else{
				# get the relevant layers
				x@stack <- x@stack[[fetchIndex, drop=FALSE]]

				# rewrite the index 
				x@index<- originIndex
				x@index[] <- 1:length(x@index)
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
		         cat("- names      : ", paste(allName, collapse=", "), "\n", 
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
		    	for(i in 1:length(allName)){
					if(i==1) cat("- rownames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
					if(i==2) cat("- colnames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
					if(i>2) cat(paste("- Dim", i, " names", sep=""), "  : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		    	}

		    	  
		    }
		   
	    }
	    cat("\n")
	}
)




#' @exportMethod length
setMethod(
	"length",
	signature="RasterArray",
	function(x) length(x@stack)
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
	function(x) colnames(x@rownames)
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

# aggregate

# disaggregate