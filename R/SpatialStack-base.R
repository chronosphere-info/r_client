#' @export SpatialStack
setMethod("initialize",signature="SpatialStack",
	definition=function(.Object,Spatials, proj4string=NULL, verbose=TRUE){
		# recursive call to for a list of files
		if("character"==class(Spatials)){
			# output
			spatialList <- list()

			
			for(i in 1:length(Spatials)){
				# readOGR is suggests!
				if(! requireNamespace("rgdal", quietly=TRUE)) stop("The 'rgdal' package is required to read Spatials from disk.")
				spatialList[[i]] <- rgdal::readOGR(Spatials[i], verbose=verbose)
			}
			# store the names
			withExt<- unlist(lapply(strsplit(Spatials, "/"), function(x) x[length(x)]))
			names(spatialList) <- unlist(lapply(strsplit(withExt, "\\."), function(x) x[1]))

			# should be used in the future
			Spatials <- spatialList

		}

		# for Spatial* input
		if(inherits(Spatials, "Spatial")) Spatials <- list(Spatials)

		# add the Spatial objects to the final object
		.Object@Spatials <- Spatials

		# check whether all elements inherit from Spatials
		allInherit <- unlist(lapply(Spatials, function(x){
			inherits(x, "Spatial")
		}))

		if(any(!allInherit)) stop("Not all elements of 'Spatials' is a Spatial* object.") 

		# do the new name
		names(.Object@Spatials) <- newname(Spatials)

		# check the CRS of all Spatials
		allProj <- unlist(lapply(Spatials, function(x) x@proj4string))

		allProj[is.na(allProj)] <- "NA"

		# stop if there are multiple
		if(length(unique(allProj))>1) stop("The supplied list of Spatial objects have multiple CRSs.\nUse the 'proj4string' to project all elements in the same CRS.")

		# transform all to a single CRS
		if(!is.null(proj4string)){
			if(unique(allProj)=="NA"){
				warning("The Spatial objects have no CRS value, but it will be replaced by proj4string.")
			}else{
				for(i in 1:length(Spatials)){
	
					.Object@Spatials[[i]] <- sp::spTransform(.Object@Spatials[[i]], proj4string)
				}
			}
			.Object@proj4string <- proj4string
		}else{
			.Object@proj4string <- Spatials[[1]]@proj4string
		}

		# recalculate @bbox
		.Object@bbox <-reBBOX(.Object@Spatials)

		# return finished objects
		return(.Object)
})

# function to recalculate bounding box of SpatialStack
reBBOX <- function(Spatials){
	allX<- lapply(Spatials, function(a){
		a@bbox["x",]
	})

	allY<- lapply(Spatials, function(a){
		a@bbox["y",]
	})

	bbox <-  rbind(range(allX), range(allY))
	colnames(bbox) <- c("min", "max")
	rownames(bbox) <- c("x", "y")

	return(bbox)
}

# function to rename the elemnts of the SpatialStack. 
# By using a data.frame method you can make sure that no two elemnts have the same name. 
newname <- function(x){
	tem <- as.list(seq(1.0, length(x),1))
	names(tem) <- names(x)
	te <- as.data.frame(tem)
	colnames(te)
}




#' Stacking method for the \code{\link{SpatialStack}} objects
#'
#' The function alls a \code{\link{RasterArray}}-like stacking of Spatial* objects.
#' @rdname stack
#' @param x \code{SpatialPoints},\code{SpatialPointsDataFrame},\code{SpatialLines},\code{SpatialLinesDataFrame},\code{SpatialPolygons},\code{SpatialPolygonsDataFrame}, object.
#' @return A \code{\link{RasterArray}} class object.
#' @export 
setMethod(
	"stack",
	"VectorSpatialClasses",
	function(x, ...){
		# list the additional files
		additional <- list(...)

		# just apply constructor to it
		SpatialStack(c(list(x), additional))

	}
)



## input form should be:
#- list of Spatial objects
#- arguments as Spatial objects
#- the c and combine methods should be here

setMethod(
	"show",
	signature="SpatialStack", 
	function (object) 
	{
	    cat("class       :", class(object), "\n")
	   
	    mnr <- 15
	    nl <- nlayers(object)
	    if (nl == 0) {
	        cat("nlayers     :", nl, "\n")
	    } else {
	    	cat("length      : ", nlayers(object), "\n")

	        cat("bbox        : ", 
	        	object@bbox["x", "min"], ", ",  
	        	object@bbox["x", "max"], ", ",  
	        	object@bbox["y", "min"], ", ",  
	        	object@bbox["y", "max"], 
	            "  (xmin, xmax, ymin, ymax)\n", sep = "")

	        cat("coord. ref. : ", object@proj4string@projargs, "\n")
 			cat("names       : ", paste(names(object), collapse=", "))
	    }
	    cat("\n")
	}
)





#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	"SpatialStack",
	function(x){
		length(x@Spatials)
	}
)

#' @rdname names
#' @exportMethod names
# names
setMethod(
	"names", 
	"SpatialStack",
	function(x){
		names(x@Spatials)
	}
)

#' @rdname names
#' @exportMethod "names<-"
setReplaceMethod(
	"names",
	signature="SpatialStack",
	definition=function(x,  value){
		# not defined for matrices or higher
		names(x@Spatials) <- value
		return(x)
})



#' Return types of objects in a SpatialStack  or SpatialArray object
#'
#' Methods sequences that start with an NA do not yet work. 
#' @rdname types
#' @param x \code{SpatialStack} object.
#' @return A \code{character} class object.
#' @export 
setGeneric("types", function(x) standardGeneric("types"))

#' @rdname types
setMethod(
	"types", 
	"SpatialStack",
	function(x){
		all <- unlist(lapply(x@Spatials, class))
		names(all) <- names(x@Spatials)
		return(all)
	}
)


# Spatial object chain
# c and combine methods 



# Functions to subset and replace items in a SpatialStack
#' Subset a SpatialStack object
#' 
#' Extract subsets of SpatialStack class object similarly to a RasterStack 
#' 
#' @param x \code{SpatialStack} object.
#' @param i subscript ofvector-like subsetting.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \code{SpatialStack} wrapper be dropped and the element be reduced to a single \code{RasterLayer}?
##' @return A \code{Spatial} or \code{SpatialStack} class object.
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
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SpatialStack", i="character"),
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
	signature(x="SpatialStack", i="logical"),
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



