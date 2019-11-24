# Functions that are practically inherited from Raster*

#' Resolution of a RasterArray object
#' 
#' The methods are inherited from the \code{RasterStack} class, see \code{\link[raster]{resolution}}. Replacement is not allowed.
#' 
#' @param x a \code{RasterArray} class object.
#' 
#' @rdname res
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

#' @rdname res
#' @exportMethod yres
setMethod(
	"yres",
	signature="RasterArray",
	function(x){
		yres(x@stack)
	} 
)

#' @rdname res
#' @exportMethod res
setMethod(
	"res",
	signature="RasterArray",
	function(x){
		res(x@stack)
	} 
)


#' Minimum and maximum values in a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class. Postitions of layers are conserved in the output. (including missing layers)
#' 
#' @param x a \code{RasterArray} class object.
#' @param vec Should the dimensions of the \code{RasterArray} be omitted?
#' @param ... arguments passed to the \code{\link[raster]{cellStats}} function.
#' 
#' @rdname extremeValues
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

#' @rdname extremeValues
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


#' Resample a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param y The y argument of the \code{\link[raster]{resample}} function.
#' @param ... arguments passed to the \code{\link[raster]{resample}} function.
#' 
#' @exportMethod resample
setMethod(
	"resample",
	signature=c("RasterArray", "ANY"),
	function(x,y,...){
		x@stack <- stack(resample(x@stack, y,...))
		return(x)
	}
)

#' Crop a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param y an xtent object, or any object from which an Extent object can be extracted (see Details)
#' @param ... arguments passed to the \code{\link[raster]{crop}} function.
#' 
#' @exportMethod crop
setMethod(
	"crop",
	signature=c("RasterArray"),
	function(x,y,...){
		x@stack <- raster::stack(crop(x@stack,y,...))
		return(x)
	}
)

#' Aggregate raster cells in a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param ... arguments passed to the \code{\link[raster]{aggregate}} function.
#' 
#' @exportMethod aggregate
setMethod(
	"aggregate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- stack(aggregate(x@stack,...))
		return(x)
	}
)

#' Disaggregate raster cells in a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param ... arguments passed to the \code{\link[raster]{disaggregate}} function.
#' 
#' @exportMethod disaggregate
setMethod(
	"disaggregate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- stack(disaggregate(x@stack,...))
		return(x)
	}
)


#' Extraction of values from multiple RasterLayers in a RasterArray object
#' 
#' The function takes a set of time-dependent coordinates and extracts the value they point to from associted RasterLayers in a RasterArray.
#' 
#' @param y (\code{matrix} or \code{data.frame}). The data table containing the coordinates and (optionally) 
#' 	the indices or names of the associated \code{RasterLayer}s in \code{x}.
#' @param x (\code{RasterArray}). A set of \code{RasterLayers} that are associated with entries (one dimension) or the rows of \code{x}.
#' @param by (\code{character} or \code{vector}) The link between \code{x} and \code{y}. If \code{by} is a \code{character}
#' string then it is expected to be column of \code{x} and should contain the \code{names} or the indices of the
#' associated \code{RasterLayer}s in \code{x}. If it is a \code{vector} its length should match the number of rows in \code{x}
#' and it will be used as if it were a column of \code{x}.
#' @param lng (\code{character}) A column of \code{x} that includes the paleolongitudes.
#' @param lat (\code{character}) A column of \code{x} that includes the paleolatitudes.
#' @param force (\code{character}) If set to \code{"numeric"} the \code{by} argument or the column it points to will be converted to
#' numeric values, and x will be subsetted with \code{numeric} subscripts of the \code{x} \code{RasterArray}. If set to character, the by column (or vector) will be
#' forced to \code{character} values and will be used as character subscripts.
#' @exportMethod extract
setMethod(
	"extract", 
	signature=c(x="RasterArray", y="data.frame"), 
	definition=function(x, y, by, lng="plng", lat="plat", force=NULL){
	
	# defense
#	if(!is.data.frame(y) & !is.matrix(y)) stop("The argument y has to be a data.frame or matrix.")
	
	dimX <- dim(x)
	if(length(dimX)>2) stop("x has to be a one or two-dimensional RasterArray.")

	# recursive case
	if(length(dimX)==2){
		vals <- NULL
		for(k in 1:(dimX[2])){
			temp <- extract(x[,k], y=y, by=by, lng=lng, lat=lat, force=force)
			vals <- cbind(vals, temp)
		}
		# rename the column names
		colnames(vals)<- colnames(x)

	# base case
	}else{

		# column that contains which map the coordinate belongs to
		if(is.character(by)){
			if(!by%in%colnames(y)) stop("The argument by has to be a column of y. ")
			interactor <- y[, by]
		# separate vector
		}else{
			if(is.vector(by) & (length(by)==nrow(y))){
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
		coords <- y[, c(lng, lat)]
	
		# storage
		vals <- rep(NA, nrow(y))
	
		for(i in 1:length(doFor)){
			bThis<- doFor[i]==interactor
	
			# select the appropriate RasterLayer
			thisLayer <- x[doFor[i]]
	
			# extract values from raster
			vals[bThis] <- raster::extract(thisLayer, coords[bThis, ])
		}
		names(vals) <- rownames(y)
	}
	return(vals)

})

#' Statistics across cells in a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class. Postitions of layers are conserved in the output. (including missing layers)
#' 
#' @param x a \code{RasterArray} class object.
#' @param stat A function to be applied.
#' @param ... arguments passed to the \code{\link[raster]{cellStats}} function.
#' 
#' @exportMethod cellStats
setMethod("cellStats", signature="RasterArray", 
	definition=function(x, stat,...){
		statVect <- raster::cellStats(x@stack, stat=stat,...)
		endObj<- x@index
		endObj[!is.na(endObj)] <- statVect
		return(endObj)
	}
)

#' Summary
#'
#' Summarizes a \code{RasterArray} class object.
#' 
#' The method is inherited from the \code{RasterStack} class. 
#' 
#' @param object a \code{RasterArray} class object.
#' @exportMethod summary
setMethod("summary", 
	signature="RasterArray",
	definition=function(object){
		allSumm <- raster::summary(object@stack)
		endRes <- extendDim(proxy(object), allSumm)
		endRes
	}
)


#' Project a RasterArray object
#'
#' The method implemets the \code{\link[raster]{projectRaster}} function for \code{RasterArray} class objects.
#' 
#' @param from A \code{Raster*} \code{RasterArray} object to project.
#' @param to \code{Raster*} object with the parameters to which 'from' should be projected
#' @param res single or (vector of) two numerics. To, optionally, set the output resolution if 'to' is missing
#' @param crs character or object of class 'CRS'. PROJ.4 description of the coordinate reference system. In projectRaster this is used to set the output CRS if 'to' is missing, or if 'to' has no valid CRS
#' @param method method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' (bilinear interpolation; the default value), which is appropriate for continuous variables.
#' @param alignOnly \code{logical}. Use to or other parameters only to align the output (i.e. same origin and resolution), but use the projected extent from from
#' @param over \code{logical}. If TRUE wrapping around the date-line is turned off. This can be desirable for global data (to avoid mapping the same areas twice) but it is not desireable in other cases
#' @param filename \code{character} output filname. Not applicable for RasterArray class objects.
#' @param ... additional arguments as for \code{\link[raster]{writeRaster}}.
#' @rdname projectRaster
#' @exportMethod projectRaster
setGeneric("projectRaster", def=raster::projectRaster)

#' @rdname projectRaster
setMethod("projectRaster", "RasterArray", 
	function(from, to, res, crs, method="bilinear", 
             alignOnly=FALSE, over=FALSE){
		from@stack <- raster::stack(raster::projectRaster(from=from@stack, to=to, res=res, 
			crs=crs, method=method, alignOnly=alignOnly, over=over))
		return(from)
	}
)




#' Methods to mask RasterArray objects, or to mask with them
#' 
#' Additional functions to \code{\link[raster]{mask}} generic function involving the \code{\link{RasterArray}} class. The following methods are implemented:
#' 
#' 
#' RasterArray masked with RasterLayer: every RasterLayer in the stack masked.
#' 
#' 
#' RasterArray masked with another RasterArray: one-to-one match between RasterLayers.
#' 
#' 
#' RasterArray masked with RasterStack: one-to-one match between RasterLayers.
#' 
#' 
#' RasterArray masked with Spatial: all layers masked with an Sp object
#' 
#' 
#' RasterArray masked with Spatial: all layers masked with an Sp object
#' 
#' 
#' RasterLayer masked with RasterArray: layer is masked out iteratively with every member of RasterArray.
#' 
#'
#' @param x Raster* object
#' @param mask Raster* object or a Spatial* object
#' @param filename character. Optional output filename (only if x is a RasterLayer and RasterStackBrick)
#' @param inverse logical. If \code{TRUE}, areas on mask that are _not_ the \code{maskvalue} are masked
#' @param maskvalue numeric. The value in \code{mask} that indicates the cells of \code{x} that should become \code{updatevalue (default = NA)}
#' @param updatevalue numeric. The value that cells of \code{x} should become if they are not covered by \code{mask} (and not \code{NA})
#' @param updateNA logical. If \code{TRUE}, \code{NA} values outside the masked area are also updated to the \code{updatevalue} (only relevant if the \code{updatevalue} is not \code{NA}.
#' @param ... additional arguments as in \code{\link[raster]{writeRaster}}. 
#' 
#' @examples
#' data(demo)
#' 
#' # land
#' lands <- demo
#' for(i in 1:length(lands)){
#'   values(lands[i])[values(lands[i])<0] <- NA
#'   values(lands[i])[!is.na(values(lands[i]))] <- 1
#' }
#' 
#' # land topographies
#' landTopo<- mask(demo, lands)
#' 
#' @rdname mask-RasterArray-methods
#' @exportMethod mask
setMethod(
	"mask", 
	c("RasterArray", "RasterLayer"),
	definition=function(x, mask, inverse=FALSE, maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...){
		x@stack <- raster::stack(
			raster::mask(x@stack, mask=mask, inverse=inverse, maskvalue=maskvalue, 
				updatevalue=updatevalue, updateNA=updateNA, ...)
		)
		return(x)

	}
)


#' @rdname mask-RasterArray-methods
setMethod(
	"mask", 
	c("RasterArray", "RasterArray"),
	definition=function(x, mask, inverse=FALSE, maskvalue=NA, updatevalue=NA,  updateNA=FALSE, ...){
		if(!identical(dim(x), dim(mask))) stop("The dimensions of x and mask has to be the same.")

		oneMiss <- !(is.na(x@index) | is.na(mask@index))
		# subset the two stacks accordingly
			xStack <- x@stack[[x@index[oneMiss]]]
			maskStack <- mask@stack[[mask@index[oneMiss]]]

		# then execute masking with the stack
		newStack <- raster::stack(
			raster::mask(xStack, mask=maskStack, inverse=inverse, maskvalue=maskvalue, 
				updatevalue=updatevalue, updateNA=updateNA, ...)
		)

		# then create the final object
			# copy
			newInd <- x@index
			# insert NAs
			newInd[!oneMiss] <- NA
			newInd[oneMiss] <- 1:raster::nlayers(newStack)

		# class constructor
		RasterArray(index=newInd, stack=newStack)
	}
)

#' @rdname mask-RasterArray-methods
setMethod(
	"mask", 
	c("RasterArray", "Spatial"),
	definition=function(x, mask, inverse=FALSE, updatevalue=NA,  updateNA=FALSE, ...){
		x@stack <- raster::stack(
			raster::mask(x@stack, mask=mask, inverse=inverse, 
				updatevalue=updatevalue, updateNA=updateNA, ...)
		)
		return(x)
	}
)
#' @rdname mask-RasterArray-methods
setMethod(
	"mask", 
	c("RasterArray", "RasterStackBrick"),
	definition=function(x, mask, inverse=FALSE, maskvalue=NA, updatevalue=NA,  updateNA=FALSE, ...){
		if(!identical(nlayers(x), raster::nlayers(mask))) stop("x (RasterArray) and mask (RasterStackBrick) should have the same number of layers.")
		x@stack <- raster::stack(
			raster::mask(x@stack, mask=mask, inverse=inverse, maskvalue=maskvalue, 
				updatevalue=updatevalue, updateNA=updateNA, ...)
		)
		return(x)
	}
)

#' @rdname mask-RasterArray-methods
setMethod(
	"mask", 
	c("RasterLayer", "RasterArray"),
	definition=function(x, mask, filename="", inverse=FALSE, maskvalue=NA, updatevalue=NA,  updateNA=FALSE, ...){
		# copy object
		y <- mask

		# do the masking
		y@stack <- raster::stack(
			raster::mask(x, mask=mask@stack, inverse=inverse, maskvalue=maskvalue, filename=filename, 
				updatevalue=updatevalue, updateNA=updateNA, ...)
		)
		# name the new layers
		names(y@stack) <- paste("masked",names(mask@stack), sep="_")

		return(y)
	}
)


#' @rdname mask-RasterArray-methods
setMethod(
	"mask", 
	c("RasterStackBrick", "RasterArray"),
	definition=function(x, mask, filename="", inverse=FALSE, maskvalue=NA, updatevalue=NA,  updateNA=FALSE, ...){
		if(!identical(raster::nlayers(x), nlayers(mask))) stop("x (RasterStackBrick) and mask (RasterArray) should have the same number of layers.")
		# copy object
		y <- mask

		y@stack <- raster::stack(
			raster::mask(x, mask=mask@stack, inverse=inverse, maskvalue=maskvalue, filename=filename,
				updatevalue=updatevalue, updateNA=updateNA, ...)
		)
		return(y)
	}
)

#' Calculate method for the RasterArray object
#' 
#' Calculate values for a new RasterLayer/RasterArray object from another RasterArray object, using a formula. 
#' 
#' The method is an extension of the \code{\link[raster]{calc}} function. The strucuture expressed as the RasterArray's dimensions allows the calculations to be iterated for different margins of the array, similarly to the \code{apply} function, controlled by the \code{margin} argument.
#' 
#' @param x A RasterArray class object.
#' 
#' @param fun function to be applied.
#' 
#' @param margin The \code{MARGIN} parameter of the \code{apply} function.
#' 
#' @param na.rm Remove NA values, if supported by 'fun' (only relevant when summarizing a multilayer Raster object into a RasterLayer)
#' 
#' @param forcefun logical. Force calc to not use fun with apply; for use with ambiguous functions and for debugging (see Details)
#' 
#' @param forceapply logical. Force calc to use fun with apply; for use with ambiguous functions and for debugging (see Details)
#' 
#' @examples
#' data(demo)
#' 
#' d2 <- cbind(demo, demo)
#' double <- calc(d2, margin=1, fun=sum)
#' 
#' @exportMethod calc
setMethod(
	"calc", 
	c(x="RasterArray",fun="function"), 
	function(x, fun, margin=NULL, na.rm=NULL, forcefun=FALSE, forceapply=FALSE){
		if(!is.null(margin)) if(length(margin)>1){
			warning("Multidimensional margin is not yet implemented.\nThe 'margin' argument is forced to NULL.")
			margin <- NULL
		}

		# completely inherit from rasterstack
		if(is.null(margin) | length(dim(x))==1){
			if(is.null(na.rm)){
				ret <- raster::calc(x=x@stack, fun=fun,  forcefun=forcefun, forceapply=forceapply)
			}else{
				ret <- raster::calc(x=x@stack, fun=fun,  na.rm=na.rm, forcefun=forcefun, forceapply=forceapply)
			}
		# margin passed to underlying apply
		}else{
			if(is.null(na.rm)){
				retList <- apply(proxy(x), margin, function(y){
					# get relevant layers in the stack
					raster::calc(x=x@stack[[y]], fun=fun,  forcefun=forcefun, forceapply=forceapply)
				})

				# ANOTHER APPLY LOOP CAN BE USED TO DEFINE TARGET OBJECT WITH HIGHER DIMENSIONS.
				# FIRST, EXPERIMENT WITH 3d RASTERARRAYS.  THEN DEVELOP APPLY. 

			}else{
				retList <- apply(proxy(x), margin, function(y){
					# get relevant layers in the stack
					raster::calc(x=x@stack[[y]], fun=fun,  na.rm=na.rm, forcefun=forcefun, forceapply=forceapply)
				})
			}

			# create a new RasterArray from the list of Layers
			newStack <- raster::stack(retList)
			ind <- 1:raster::nlayers(newStack)
			names(ind) <- names(retList)
			ret <- RasterArray(newStack, ind)
		
		}

		return(ret)
	}
)
