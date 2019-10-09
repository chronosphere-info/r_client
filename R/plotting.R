#' Pseudo generic function to plot maps of different object classes
#' 
#' This function plots the different paleo
#' 
#' @param x Object to be plotted 
#' @param legend (\code{logical}) Triggers whether the legend of a RasterLayer would be plotted.
#' @export
mapplot<-function(x,legend=FALSE, ...){
	if(class(x)=="RasterLayer"){
		raster::plot(x,legend=legend, ...)
	}
	if(class(x)=="RasterStack"){
		raster::plotRGB(x,...)
	}

	if(class(x)=="RasterArray"){
		raster::plotRGB(x@stack,...)
	}


	if(class(x)=="SpatialPolygonsDataFrame" | class(x)=="SpatialPolygons"){
		sp::plot(x,...)
	}
}
