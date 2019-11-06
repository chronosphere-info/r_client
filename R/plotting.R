#' Wrapper function to plot maps of different classes
#' 
#' This function plots Raster and sp-type objects.
#' 
#' @param x Object to be plotted 
#' @param legend (\code{logical}) Triggers whether the legend of a RasterLayer would be plotted.
#' @param col \code{character} Color or color scheme of the plot. See \code{?ramps} for available palettes.
#' @rdname mapplot
#' @exportMethod mapplot
setGeneric("mapplot", function(x,...) standardGeneric("mapplot"))

#' @rdname mapplot
setMethod("mapplot", signature="RasterLayer", 
	definition = function(x, legend=FALSE, col=gradinv(255),...){
		if(length(col)==1){
			if(col %in% c("ocean", "gradinv", "terra", "coldhot", "drywet", "wet")){
				raster::plot(x,legend=legend, col=eval(parse(text = col))(255), ...)
			}
		  
		  if (col == "earth"){
		    negBreaks <- seq(-10000, 0, by=50)
		    posBreaks <- seq(0, 10000, by=50)
		    
		    raster::plot(x,legend=legend, 
		                 breaks =c(negBreaks[1:(length(negBreaks)-1)], posBreaks), 
		                           col=c(ocean(length(negBreaks)-1), 
		                                 terra(length(posBreaks))), ...)
		  }
		}else{
			raster::plot(x,legend=legend, col=col, ...)
		}
	}
)

#' @rdname mapplot
setMethod("mapplot", signature="RasterStack", 
	definition = function(x, col=gradinv(255),  ...){
		raster::plotRGB(x,col=col, ...)
	}
)

#' @rdname mapplot
setMethod("mapplot", signature="RasterArray", 
	definition = function(x, col=gradinv(255),  ...){
		raster::plotRGB(x@stack,col=col, ...)
	}
)

setMethod("mapplot", signature="SpatialPolygonsDataFrame", 
	definition = function(x, col="#71351d", ...){
		sp::plot(x, col=col,...)
	}
)

setMethod("mapplot", signature="SpatialPolygons", 
	definition = function(x, col="#71351d",  ...){
		sp::plot(x, col=col,...)
	}
)
