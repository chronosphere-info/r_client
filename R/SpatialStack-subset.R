
# Functions to subset and replace items in a SpatialStack
#' Subset a SpatialStack object
#' 
#' Extract subsets of SpatialStack class object similarly to a RasterStack 
#' 
#' @param x \code{SpatialStack} object.
#' @param i subscript ofvector-like subsetting.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \code{SpatialStack} wrapper be dropped and the element be reduced to a single \code{RasterLayer}?
#' @return A \code{Spatial} or \code{SpatialStack} class object.
#' @examples 
#' # stack of the paleomap paleocoastlines
#' data(coasts)
#' spstack <- coasts@stack
#' subset(spstack, "X5Ma_CS_v7")
# combined
##' @exportMethod subset
setMethod(
	"subset",
	"SpatialStack",
	function(x, i, drop=TRUE){
		# do a check for NA
		if(any(is.na(i))) stop("Cannot use NAs in subsetting of SpatialStacks.")

		# inherit from list-type subsetting
		x@Spatials <- x@Spatials[i]

		# get the list names to determine whether the subset is legit
		newNames<- names(x@Spatials)
		if(any(is.na(newNames))) stop("Invalid subset.")

		if(length(x@Spatials)==1 & drop){
			return(x@Spatials[[1]])
		}else{
			
			# recalculate bounding box
			x@bbox <- reBBOX(x@Spatials)

			# return the modified object
			return(x)
		}
	}
)

#' Indexing to extract subsets of a SpatialStack object
#'
#' The single and double bracket subsetting is identical in the case of SpatiaStacks.
#' 
#' @param x \code{SpatialStack} object.
#' @param i subscript of vector-like subsetting.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \code{SpatialStack} wrapper be dropped and the element be reduced to a single \code{Spatial}?
#' @return A \code{SpatialStack} or \code{Spatial} class object.
#' @examples 
#' # stack of the paleomap paleocoastlines
#' data(coasts)
#' spstack <- coasts@stack
#' spstack[1]
##' @exportMethod [
setMethod(
	"[",
	"SpatialStack",
	function(x, i, drop=TRUE){
		subset(x=x, i=i, drop=drop)
	}
)



#' Replace layers of a SpatialStack object
#' 
#' The single and double bracket subsetting is identical in the case of SpatialStacks.
#'
#' @param x \code{SpatialStack} object.
#' @param i subscript of vector-like subsetting.
#' @param j unused.
#' @param ... unused..
#' @param value A single \code{Spatial} object.
#' @return None.
#' @rdname replacementSingle-spatialstack
#' @examples
#' data(coasts)
#' spstack <- coasts@stack[1:2]
#' spstack[1] <- mapedge()
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SpatialStack", i="character", value="VectorSpatialClasses"),
	definition=function(x,i,j=NULL,..., value){
		if(is.character(i)){
			i <- which(names(x@Spatials)==i)
		}
		# invoke numeric method
		x[i] <- value

		# retirm
		return(x)
		
	}
)

#' @rdname replacementSingle-spatialstack
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SpatialStack", i="logical", value="VectorSpatialClasses"),
	definition=function(x,i,j=NULL,..., value){
		if(is.logical(i)){
			i <- which(i)
		}

		# invoke numeric method
		x[i] <- value

		# retirm
		return(x)
		
	}
)

#' @rdname replacementSingle-spatialstack
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SpatialStack", i="numeric", value="VectorSpatialClasses"),
	definition=function(x,i,j=NULL,..., value){
		if(!missing(j)) stop("Multiple dimensions are not allowed for SpatialStacks.")
		# original number of layers
		lays <- nlayers(x)
		# just replace itesm in the list - one by one
		
		# is there just one value?
		if(grep("Spatial", class(value))){
			for(k in 1L:length(i)){
				x@Spatials[[i[k]]] <- value
			}
		}
		# do not allow extension, 
		if(nlayers(x)>lays) stop("Out of bounds replacement is not allowed. ")


		return(x)
		
	}
)


#' @rdname replacementSingle-spatialstack
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SpatialStack", i="character", value="SpatialStack"),
	definition=function(x,i,j=NULL,..., value){
		if(is.character(i)){
			newI <- NULL
			for(k in 1:length(i)){
				newI <- c(newI, which(names(x@Spatials)==i[k]))
			}
		}
		# invoke numeric method
		x[newI] <- value

		# retirm
		return(x)
		
	}
)


#' @rdname replacementSingle-spatialstack
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SpatialStack", i="logical", value="SpatialStack"),
	definition=function(x,i,j=NULL,..., value){
		if(is.logical(i)){
			i <- which(i)
		}

		# invoke numeric method
		x[i] <- value

		# retirm
		return(x)
		
	}
)

#' @rdname replacementSingle-spatialstack
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SpatialStack", i="numeric", value="SpatialStack"),
	definition=function(x,i,j=NULL,..., value){
		if(!missing(j)) stop("Multiple dimensions are not allowed for SpatialStacks.")
		# original number of layers
		lays <- nlayers(x)
		# just replace itesm in the list - one by one
		
		if(length(i)!=nlayers(value)) stop("Length of replacement value is not the same as the length of subscript.")
		# is there just one value?
		
		for(k in 1L:length(i)){
			x@Spatials[[i[k]]] <- value[k]
		}
		
		# do not allow extension, 
		if(nlayers(x)>lays) stop("Out of bounds replacement is not allowed. ")


		return(x)
		
	}
)



# POTENTIALLY NEEDED: value="list"

#################################################################################
# METHODF FOR [[

#' Indexing to extract Spatial items from a  SpatialStack object
#'
#' The single and double bracket subsetting is identical in the case of SpatialStacks.#' 
#' @param x \code{SpatialStack} object.
#' @param i subscript of vector-like subsetting.
#' @param drop \code{logical} should the \code{SpatialStack} be dropped and the element be reduced to a single \code{Spatial} object?
#' @return A \code{Spatial} or \code{SpatialStack} class object.
#' @exportMethod "[["
setMethod(
	"[[",
	"SpatialStack",
	function(x, i, drop=TRUE){
		subset(x=x, i=i, drop=drop)
	}
)



#' @rdname replacementSingle-spatialstack
#' @exportMethod "[[<-"
setReplaceMethod(
	"[[", 
	signature(x="SpatialStack", i="character"),
	definition=function(x,i,..., value){
		if(is.character(i)){
			i <- which(names(x@Spatials)==i)
		}
		# invoke numeric method
		x[i] <- value

		# retirm
		return(x)
		
	}
)

#' @rdname replacementSingle-spatialstack
#' @exportMethod "[[<-"
setReplaceMethod(
	"[[", 
	signature(x="SpatialStack", i="logical"),
	definition=function(x,i,..., value){
		if(is.logical(i)){
			i <- which(i)
		}

		# invoke numeric method
		x[i] <- value

		# retirm
		return(x)
		
	}
)

#' @rdname replacementSingle-spatialstack
#' @exportMethod "[[<-"
setReplaceMethod(
	"[[", 
	signature(x="SpatialStack", i="numeric", value="VectorSpatialClasses"),
	definition=function(x,i,j=NULL,..., value){
		if(!missing(j)) stop("Multiple dimensions are not allowed for SpatialStacks.")
		
		# original number of layers
		lays <- nlayers(x)
		# just replace itesm in the list - one by one
		
		# is there just one value?
		if(grep("Spatial", class(value))){
			for(k in 1L:length(i)){
				x@Spatials[[i[k]]] <- value
			}
		}
		# do not allow extension, 
		if(nlayers(x)>lays) stop("Out of bounds replacement is not allowed. ")


		return(x)
		
	}
)

