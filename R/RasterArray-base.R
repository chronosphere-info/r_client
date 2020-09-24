
# build from existing stack with existing index, or dimensions
#' @export RasterArray
setMethod("initialize",signature="RasterArray",
	definition=function(.Object,stack, index=NULL, dim=NULL){
		# some defense for index
		if(is.null(dim)){ 
			if(class(stack)!="RasterStack") stop("The 'stack' has to be a 'RasterStack' - class object.")
			if(!is.numeric(index)) stop("The 'index' has to be a 'numeric' object.")
			

			# where were supposed to be NAs
			bNA <- is.na(index)

			if(any(index[!bNA]%%1!=0) | any(index[!bNA]<1)) stop("The 'index' has to contain positive integer values.")
			
			# the number of valid entries mismatch the number of layers
			if(sum(!bNA)!=nlayers(stack)) stop("You have to provide as many layers as many valid entries in index.")

			# reorder the stack
			noNAInd <- index[!bNA]
			newStack <- stack[[noNAInd]]

			# force index to be monotonous integer sequence
			newIndex <- index
			newIndex[] <- NA
			newIndex[!bNA] <- 1:nlayers(stack)

			# promote RasterLayer if that is the only Layer
			if(class(newStack)=="RasterLayer") newStack <- raster::stack(newStack)

			# store final object
			.Object@index <- newIndex
			.Object@stack <- newStack
			
		}else{
			if(!is.numeric(dim)) stop("The 'dim' argument has to be a 'numeric' vector.")
			if(raster::nlayers(stack)!=prod(dim, na.rm=TRUE)) warning("The number of layers in the does not equal the product of the 'dim' vector.")
			.Object@stack<- stack
			index <- array(1:nlayers(stack), dim=dim)
			# in case of reuse
			index[duplicated(as.numeric(index))] <- NA
			.Object@index<- index
			
			
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
#' @return A \code{logical} \code{vector}, \code{matrix} or \code{array} matching the structure of the \code{RasterArray}.
#' 
#' @examples
#' data(dems)
#' dems[2] <- NA
#' is.na(dems)
#' 
#' @export
is.na.RasterArray<-function(x){
	is.na(proxy(x))
}


#' Apply-type iterator for RasterArrays
#' 
#' The function implements the \code{\link[base]{apply}}-type iterators for the RasterArray class. Output values are constrained to RasterArrays, whenever possible. 
#' Not yet implemented for multidimensional MARGINs.
#' @examples
#' # double of itself
#' data(dems)
#' a<- cbind(dems, dems)
#' same <- apply(a, 1, sum)
#' @return Depending on the on the output of \code{FUN}, a \code{list}, a \code{vector} or \code{RasterArray} object.
#' @param X an array, including matrices and RasterArrays.
#' @param MARGIN a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns, \code{c(1, 2)} indicates rows and columns. Where \code{X} has named dimnames, it can be a character vector selecting dimension names. For \code{RasterArrays} only single dimension margins are implemented.
#' @param FUN  the function to be applied: see ‘Details’ of \code{\link[base]{apply}}. 
#' @param ... optional arguments passed to \code{FUN}.
#' @rdname apply-methods
#' @exportMethod apply
setGeneric("apply", def=base::apply)


RasterArrayApply <- function(X, MARGIN, FUN,...){
	if(length(dim(X))==1) stop("The 'apply()'' function is not applicable to vector-like RasterArrays.\nUse 'sapply()'' instead.")
	if(length(MARGIN)>1) stop("Not yet implemented for multidimensinal margins.")

	# use the proxy instead
	proxX <- proxy(X)

	ret <-apply(proxX, MARGIN, function(y){
		# get the columns/rows associated with this subset
		thislayers <- X[[y]]
		# if all are empty, force numeric
		if(sum(is.na(thislayers))==length(thislayers)) thislayers <- as.numeric(thislayers)

		# and plug it into the supplied function
		FUN(thislayers)
	})

	# draft of the apply output structure
	struct <- apply(X@index, MARGIN, function(y) 1)

	# if this output is a rasterlayer make an exception!
	if(is.list(ret)){
		classOfItems <- unlist(lapply(ret, function(y){
				if(class(y)=="RasterLayer"){
					return(FALSE)
				}else{
					if(is.numeric(y) | is.character(y) | is.logical(y)){
						if(is.na(y)){
							return(NA)
						}else{
							return(TRUE)
						}
					}else{
						return(TRUE)
					}
				}
			} ))
		if(sum(unique(classOfItems), na.rm=TRUE)==0){
			bNA <- is.na(classOfItems)

			# transform this to a rasterArray
			newStack <- raster::stack(ret[!bNA])
			# index
			struct[] <- NA
			struct[!bNA] <- 1:sum(!bNA)
			names(struct) <- names(ret)

			ret <- RasterArray(newStack, struct)

		}
	}

	return(ret)
}

#' @rdname apply-methods
#' @aliases apply,RasterArray-method
"apply"

if("simplify" %in% names(formals(base::apply))){
	setMethod("apply", "RasterArray",function(X, MARGIN, FUN,..., simplify=TRUE) 
		RasterArrayApply(X, MARGIN, FUN, ...)
	)

}else{
	setMethod("apply", "RasterArray",function(X, MARGIN, FUN,...) 
		RasterArrayApply(X, MARGIN, FUN, ...)
	)

}



