#' @import raster
#' @import sp
#' @importFrom methods new cbind2 rbind2
#' @importFrom utils read.csv download.file unzip flush.console
#' @importFrom graphics par layout
#' @importFrom grDevices devAskNewPage




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
#'   data(demo)
#'   st <-demo@stack
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
			if(raster::nlayers(stack)==prod(index, na.rm=T)){
				.Object@stack<- stack
				index <- array(1:nlayers(stack), dim=dim)
				.Object@index<- index
			}

		}
		return(.Object)
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

#' S3-type method for RasterArray allowing View(), head() and tail() to work.
#' @exportMethod as.data.frame
#' @S3method as.data.frame RasterArray
as.data.frame.RasterArray <- function(x, rownames=NULL, optional=FALSE){
	df <- as.data.frame(proxy(x))
	if(ncol(df)==1) colnames(df) <- "X0"
	if(!is.null(rownames)){
		rownames(df)<- rownames
	}
	if(optional){
		rownames(df) <- NULL
		colnames(df) <- NULL
	}
	return(df)
}

#' @exportMethod as.RasterArray
setGeneric("as.RasterArray", function(from) standardGeneric("as.RasterArray"))

#' @exportMethod coerce
setMethod(as.RasterArray, signature=c("RasterLayer"), definition=function(from){
	RasterArray(raster::stack(from), index=1)
})

setMethod(as.RasterArray, signature=c("RasterStack"), definition=function(from){
	RasterArray(from, index=1:nlayers(from))
})

setMethod(as.RasterArray, signature=c("RasterBrick"), definition=function(from){
	RasterArray(stack(from), index=1:nlayers(from))
})

setAs(from="RasterLayer", to="RasterArray", function(from){
	as.RasterArray(from)
})

setAs(from="RasterStack", to="RasterArray", function(from){
	as.RasterArray(from)
})

setAs(from="RasterBrick", to="RasterArray", function(from){
	as.RasterArray(from)
})


# function to defragment the matrix
defragment <- function(x){
	b <- is.na(x)
	x[!b] <- 1:sum(!b)
	return(x)
}


# this utility function will combine the layer specific information
# 2d matrix (vals2d) layer names are represented as colnames
extendDim <- function(proxy, vals2d, newdim=1){
	# the original dimensions of the proxy
	origDim <- dim(proxy)
	if(is.null(origDim)) origDim <- length(proxy) 

	# the names of the proxy
	origNames <- dimnames(proxy)
	if(is.null(origNames)) origNames <- list(names(proxy))

	# number extended
	nVals <-dim(vals2d)[newdim]
	
	# copy the names properly 
	addNames <- dimnames(vals2d)[[newdim]]

	# where are the non-na values
	naMap <- !is.na(proxy)

	# vector shape of the data
	endObj <- rep(NA, prod(c(origDim,nVals)))
	
	# loop through the new dimension
	for(i in 1:nVals){
		# what contains the new dimension?
		if(newdim==1){
			theseVals<- vals2d[i,]
		}else{
			theseVals<- vals2d[,i]
		}
		# the final object. 
		endObj[(1:length(proxy))+(i-1)*length(proxy)] <- theseVals[proxy]
	}
	# dimensions and names set right
	dim(endObj) <- c(origDim,nVals)
	dimnames(endObj) <- c(origNames, list(addNames))
	
	return(endObj)
}


#' Positions of missing values in a RasterArray object
#' 
#' The function behaves similar to the regular \code{is.na()} function applied to the proxy object of a \code{RasterArray}.
#' 
#' @param x A \code{RasterArray} class object.
#' 
#' @examples
#' data(demo)
#' demo[2] <- NA
#' is.na(demo)
#' 
#' @export
is.na.RasterArray<-function(x){
	is.na(proxy(x))
}



#' Transpose a RasterArray object
#' 
#' @examples
#' data(demo)
#' t(demo)
#' @param x A RasterArray class object. 
#' 
#' @exportMethod t
setMethod(
	"t", 
	"RasterArray", 
	function(x){
		if(length(dim(x))>2) stop("RasterArray is not a matrix. ")

		# transpose index
		tIndex<- t(x@index)
		vIndex <- as.numeric(tIndex)

		# ordering
		vIndna <- vIndex[!is.na(vIndex)]

		# reorder the stack
		x@stack <- x@stack[[vIndna]]

		# refill the index
		tIndex[!is.na(tIndex)] <- 1:raster::nlayers(x@stack)

		# replace the index
		x@index <- tIndex

		return(x)

	}
)

#' @exportMethod as.list
setMethod("as.list","RasterArray", function(x,...){
	as.list(x@stack)
})


#' Apply-type iterator for RasterArrays
#' 
#' The function implements the apply-type iterators for the RasterArray class. Output values are constrained to RasterArrays, whenever possible. 
#' Not yet implemented for multidimensional MARGINs.
#' @examples
#' # not yet
#' a<- cbind(demo, demo)
#' @rdname apply-methods
#' @exportMethod apply
setGeneric("apply", def=base::apply)

#' @rdname apply-methods
setMethod("apply", "RasterArray",
	function(X, MARGIN, FUN,...){
		if(length(dim(X))==1) stop("The 'apply()'' function is not applicable to vector-like RasterArrays.\nUse 'sapply()'' instead.")
		if(length(MARGIN)>1) stop("Not yet implemented for multidimensinal margins.")

		ret <-apply(X@index, MARGIN, function(y){
			# get the columns/rows associated with this subset
			layers <- X[y]
			# and plug it into the supplied function
			FUN(layers)
		})

		# draft of the apply output structure
		struct <- apply(X@index, MARGIN, function(y) 1)

		# if this output is a rasterlayer make an exception!
		if(is.list(ret)){
			classOfItems <- unlist(lapply(ret, function(y) class(y)!="RasterLayer"))
			if(sum(unique(classOfItems))==0){
				bNA <- is.na(classOfItems)

				# transform this to a rasterArray
				newStack <- raster::stack(ret[!bNA])
				# index
				struct[!bNA] <- 1:sum(!bNA)
				names(struct) <- names(ret)

				ret <- RasterArray(newStack, struct)

			}
		}

		return(ret)
	}
)


