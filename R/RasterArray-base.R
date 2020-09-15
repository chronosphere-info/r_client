#' Virtual Array of RasterLayers
#' 
#' Array template for RasterLayers
#' 
#' The class implements structures to organize RasterLayers that have the same dimensions. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: RasterStack, the actual data.
#' index: A proxy object that represents the organization of the layers. 
#' 
#' 
#' @param stack A \code{RasterStack} class object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A \code{RasterArray} class object.
#' @examples
#' # data import
#'   data(dems)
#'   st <-dems@stack
#'   ind <- 1:nlayers(st)
#'   names(ind) <- letters[1:length(ind)]
#'   ra<- RasterArray(stack=st, index=ind)
#'   
#' @exportClass RasterArray
RasterArray <- setClass("RasterArray", slots=list(index="arrayORmatrixORvector", stack="RasterStack"))

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





#' The proxy of a RasterArray object
#' 
#' This function returns an object that symbolizes the structure of layers in the \code{RasterArray}.
#'
#' The \code{proxy} method wraps the names of layers in the stack using the \code{index} slot of the \code{RasterArray}.
#'  
#' @param x (\code{RasterArray}  focal object.
#' @return A \code{vector}, \code{matrix} or \code{array} of characters representing the \code{RasterArray} structure.
#' @param ... additional arguments passed to class-specific methods.
#' @examples
#' data(dems)
#' proxy(dems)
#'
#' data(clim)
#' proxy(clim)
#' @exportMethod proxy
#' @rdname proxy
setGeneric("proxy", function(x,...) standardGeneric("proxy"))

#' @rdname proxy
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



#' Transpose a RasterArray object
#' 
#' @examples
#' data(dems)
#' t(dems)
#' data(clim)
#' t(clim)
#' @param x A \code{RasterArray} class object. 
#' @return A \code{RasterArray} class object.
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

		# copy names
		if(!is.null(colnames(x@index))) rownames(tIndex) <- colnames(x@index)
		if(!is.null(rownames(x@index))) colnames(tIndex) <- rownames(x@index)
		if(!is.null(names(x@index)))  colnames(tIndex) <- names(x@index)

		# replace the index
		x@index <- tIndex



		return(x)

	}
)


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



