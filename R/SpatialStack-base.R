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
	if(length(Spatials)>0){
		allX<- lapply(Spatials, function(a){
			a@bbox["x",]
		})

		allY<- lapply(Spatials, function(a){
			a@bbox["y",]
		})

		bbox <-  rbind(range(allX), range(allY))
	}else{
		bbox <- matrix(NA, ncol=2, nrow=2)
	}
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




#' Stacking method for the vector Spatial* \code{\link{SpatialStack}} objects
#'
#' The function alls a \code{\link{RasterArray}}-like stacking of Spatial* objects and \code{\link{SpatialStack}}s .
#' @rdname stack
#' @examples
#' data(coasts)
#' one <- coasts[1]
#' two <- coasts[2]
#' three <- coasts[3]
#' # create a SpatialStack similar to a RasterStack
#' spStack <- stack(one, two, three)
#' @param x \code{SpatialPoints},\code{SpatialPointsDataFrame},\code{SpatialLines},\code{SpatialLinesDataFrame},\code{SpatialPolygons},\code{SpatialPolygonsDataFrame}, object.
#' @return A \code{\link{RasterArray}} class object.
#' @exportMethod stack
setMethod(
	"stack",
	"VectorSpatialClasses",
	function(x, ...){
		# list the additional files
		additional <- list(...)

		# classes used to do this
		unlist <- lapply(additional, class)

		# initial object
		full <- SpatialStack(list(x))

		if(length(additional)>0){
			# bind everything to it
			for(i in 1:length(additional)){
				# vector object
				if(inherits(additional[[i]], "Spatial")){
					full <- SpatialStack(c(full@Spatials, list(additional[[i]])))
				}else{
					if(unlist[i]=="SpatialStack"){
						full <- SpatialStack(c(full@Spatials, additional[[i]]@Spatials))
					}else{
						stop("Incompatible arguments.")
					}
				}
			}
		}

		return(full)

	}
)

#' @rdname stack
#' @param x \code{SpatialPoints},\code{SpatialPointsDataFrame},\code{SpatialLines},\code{SpatialLinesDataFrame},\code{SpatialPolygons},\code{SpatialPolygonsDataFrame}, object.
#' @param ... Additional Spatial* objects.
#' @return A \code{\link{RasterArray}} class object.
setMethod(
	"stack",
	"SpatialStack",
	function(x, ...){
		# list the additional files
		additional <- list(...)

		# classes used to do this
		unlist <- lapply(additional, class)

		if(length(additional)>0){
			# bind everything to it
			for(i in 1:length(additional)){
				# vector object
				if(inherits(additional[[i]], "Spatial")){
					x <- SpatialStack(c(x@Spatials, list(additional[[i]])))
				}else{
					if(unlist[i]=="SpatialStack"){
						x <- SpatialStack(c(x@Spatials, additional[[i]]@Spatials))
					}else{
						stop("Incompatible arguments.")
					}
				}
			}
		}

		return(x)
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



#' Return types of objects in a \code{\link{SpatialStack}}  or \code{\link{SpatialArray}} object
#'
#' Methods sequences that start with an NA do not yet work. 
#' @rdname types
#' @param x \code{\link{SpatialStack}}  or \code{\link{SpatialArray}} object.
#' @examples 
#' data(coasts)
#' types(coasts)
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

