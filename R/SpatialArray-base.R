
# build from existing stack with existing index, or dimensions
#' @export SpatialArray
setMethod("initialize",signature="SpatialArray",
	definition=function(.Object,stack, index=NULL, dim=NULL){
		# some defense for index
		if(is.null(dim)){ 
			if(class(stack)!="SpatialStack" & class(stack)!="character") stop("The 'stack' has to be a 'SpatialStack' - class object.")
			if(!is.numeric(index)) stop("The 'index' has to be a 'numeric' object.")
			
			# add the immmediate loading
			if("character"%in%class(stack)) stack <- SpatialStack(stack)

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
			if(inherits(newStack, "Spatial")) newStack <- SpatialStack(newStack)

			# store final object
			.Object@index <- newIndex
			.Object@stack <- newStack
			
		}else{
			if(!is.numeric(dim)) stop("The 'dim' argument has to be a 'numeric' vector.")
			if(nlayers(stack)!=prod(dim, na.rm=TRUE)) warning("The number of layers in the does not equal the product of the 'dim' vector.")
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
	signature="SpatialArray", 
	function (object) 
	{
	    cat("class          :", class(object), "\n")
	    mnr <- 15
	#   if (filename(object) != "") {
	#       cat("filename    :", filename(object), "\n")
	#   }


	    nl <- nlayers(object)
	    if (nl == 0) {
	        cat("nlayers      :", nl, "\n")
	        cat("Array properties: \n")
	    } else {
	   		cat("Spatial* properties: \n")
	        cat("- types        : ", paste(sort(unique(types(object@stack))), collapse=", "), 
	            "\n", sep = "")

	        cat("- bbox         : ", 
	        	object@stack@bbox["x", "min"], ", ",  
	        	object@stack@bbox["x", "max"], ", ",  
	        	object@stack@bbox["y", "min"], ", ",  
	        	object@stack@bbox["y", "max"], 
	            "  (xmin, xmax, ymin, ymax)\n", sep = "")

	       cat("- coord. ref.  : ", object@stack@proj4string@projargs, "\n")
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
			    	cat("- dimensions   : ", paste(adim, collapse=", "), 
			            "  (nrow, ncol, ...)\n", 
			            sep = "")
			    }
		#    	for(i in 1:length(allName)){
		#			if(i==1) cat("- rownames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#			if(i==2) cat("- colnames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#			if(i>2) cat(paste("- Dim", i, " names", sep=""), "  : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#    	}
				

		    	  
		    }
		}
	    cat("- num. layers  : ", nlayers(object), "\n", 
	        sep = "")
	    cat("- proxy:\n ")
	    print(proxy(object))
	   
    
	    cat("\n")
	}
)


#' @rdname types
setMethod(
	"types",
	signature="SpatialArray",
	function(x){
		ind <- x@index
		prx <- proxy(x)
		type <- prx
		
		# only NAs are present
		if(any(!is.na(ind))){
			if(!is.null(types(x@stack))) type[]<- types(x@stack)[ind]
		}

		return(type)
	}

)


#' Positions of missing values in a \code{\link{SpatialArray}} object
#' 
#' The function behaves similar to the regular \code{is.na()} function applied to the proxy object of a \code{RasterArray}.
#' 
#' @param x A \code{RasterArray} class object.
#' @return A \code{logical} \code{vector}, \code{matrix} or \code{array} matching the structure of the \code{RasterArray}.
#' 
#' @examples
#' data(coasts)
#' coasts[2,1] <- NA
#' is.na(coasts)
#' 
#' @export
is.na.SpatialArray<-function(x){
	is.na(proxy(x))
}



if("simplify" %in% names(formals(base::apply))){
	setMethod("apply", "SpatialArray",function(X, MARGIN=NULL, FUN,..., simplify=TRUE)
		if(is.null(MARGIN)){
			ArrayApplyNULL(X, FUN, ...)
		}else{
			ArrayApplyReduce(X, MARGIN, FUN, ...)
		}
	)

}else{
	setMethod("apply", "SpatialArray",function(X, MARGIN=NULL, FUN,...) 
		if(is.null(MARGIN)){
			ArrayApplyNULL(X, FUN, ...)
		}else{
			ArrayApplyReduce(X, MARGIN, FUN, ...)
		}
	)

}
