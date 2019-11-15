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
	definition = function(x, col=gradinv(255), axes=FALSE, box=FALSE, legend=FALSE, legend.title=NULL,...){
		if(length(col)==1){
			if(col %in% c("ocean", "gradinv", "terra", "coldhot", "drywet", "wet")){
				raster::plot(x,legend=FALSE, col=eval(parse(text = col))(255), axes=axes, box=box, ...)
			  
			  if (legend == TRUE){
			    op <- par()
			    par(oma=c(1,0,0,0))
			    plot(x, col=eval(parse(text = col))(255), legend.only=TRUE, horizontal = TRUE,
			         legend.args=list(text=legend.title,side=1, font=2, line=2.5, cex=1))
			    par(op)
			  }
			}
		  
		  if (col == "earth"){
		    negBreaks <- seq(-10000, 0, by=50)
		    posBreaks <- seq(0, 10000, by=50)
		    
		    raster::plot(x,legend=legend, 
		                 breaks =c(negBreaks[1:(length(negBreaks)-1)], posBreaks), 
		                           col=c(ocean(length(negBreaks)-1), 
		                                 terra(length(posBreaks))), axes=axes, box=box,...)
		    
		    #add legend stuff here
		  }
		}else{
			raster::plot(x,legend=legend, col=col, axes=axes, box=box,...)
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
          definition = function(x, col=gradinv(255), rgb=FALSE, legend=FALSE, axes=FALSE, box=FALSE, ask=TRUE,
                                nplots = 6, legend.title=NULL, plot.title =NULL, ...){
            if(rgb == TRUE){
              raster::plotRGB(x@stack, ...)  	    
            } else {
              nvars <- ifelse (is.na(dim(x)[2]), 1, dim(x)[2])
              
            #check for colour palettes - might need to check for typos
              if(length(col)==nvars){
                col <- as.list(col)
                
                
              } else col <- rep(col,nvars)
              
              for (i in 1:nvars){
                if(col[[i]] %in% c("ocean", "gradinv", "terra", "coldhot", "drywet", "wet")){
                  col[[i]]=eval(parse(text = col[[i]]))(255)
                  pal=NULL
                }
                
                if(col[[i]] == "earth"){
                  pal="earth"
                  negBreaks <- seq(-10000, 0, by=50)
                  posBreaks <- seq(0, 10000, by=50)
                  
                  brks <- c(negBreaks[1:(length(negBreaks)-1)], posBreaks) #need to change
                  col[[i]] <- c(ocean(length(negBreaks)-1), terra(length(posBreaks)-1))
                }else pal=NULL
                
              } 
            
              #edit for plot title
            if (is.null(plot.title)){
              plot.title <- names(demo)
            }
             
              if (nvars > 1){
                devAskNewPage(ask=FALSE)
                
                ncol=nvars
                nrow=2
                nplots=ncol*nrow
                pg <-if(dim(x)[1]%%nrow == 0) seq(0,dim(x)[1],nrow) else  c(seq(0,dim(x)[1],nrow), dim(x)[1])
                pg2 <- ceiling(dim(x)[1]/nrow)
                
                m <- matrix(c(1:(nplots+2)),nrow = nrow+1,ncol = ncol,byrow = TRUE)
                layout(mat = m,heights = c(0.4,0.4,0.2))
                
                rng <- list()
                
                for (n in 1:nvars){
                  rng[[n]] <- range(range(x[n,])[,], na.rm=TRUE)
                }
                
                if (is.null(pal)){
                  nbrks = lapply(col, length)[[1]] #need to edit for multiple variables, for now need to be the same
                  brks <- lapply(rng, function (x){seq(x[1], x[2], length.out = nbrks+1)})
                } else {
                  nbrks <- length(brks)
                }
                
                for (i in 1:pg2){
                  
                  for (j in (pg[i]+1):pg[i+1]){
                    
                    for (k in 1:nvars){
                      par(mar=c(0,1,2,1))
                      raster::image(x[j,k], axes=axes, xlab="", ylab="", asp=1, col=col[[k]], breaks=brks[[k]], main=names(x[j,k]))
                      devAskNewPage(ask=FALSE)
                    }
                  }
                  
                  #for empty plots
                  # if (i == length(pg)-1 & pg[i+1]%%nplots !=0) {
                  #   for (m in 1:(nplots - pg[i+1]%%nplots)) {
                  #     plot(1, type="n", axes=F, ylab=NULL, xlab=NULL)
                  #   }
                  # }
                  
                  for (k in 1:nvars){
                    par(mar=c(6,10,2,10))
                    plot(rng[[k]],c(0,5), type="n", axes=FALSE, ylab="", xlab="", xaxs="i", yaxs="i")
                    image(x=brks[[k]], z=as.matrix(brks[[k]]), col=col[[k]], add=TRUE)
                    box()
                    
                    par(xpd=TRUE)
                    labs <- pretty(rng[[k]], n=6)
                    labs <- labs[labs > rng[[k]][1] & labs < rng[[k]][2]]
                    text(x = labs, y=-8, labels = labs, cex=1.5)
                    
                    for(l in 1:length(labs)){
                      lines(c(labs[l], labs[l]), c(-3, 0))
                    }
                    
                    graphics::text(x=mean(rng[[k]]), y=10, labels=legend.title, cex=1.4, font=2)
                  }
                  
                  #ask
                  devAskNewPage(ask=ask)
                }
              } else {
                devAskNewPage(ask=FALSE)
                
                pg <- if(nlayers(x)%%nplots == 0) seq(0,nlayers(x),6) else  c(seq(0,nlayers(x),6), nlayers(x))
                
                ncol=3
                nrow=nplots/ncol + 1
                m <- matrix(c(1:nplots,rep((nplots+1), ncol)),nrow = nrow,ncol = ncol,byrow = TRUE)
                layout(mat = m,heights = c(0.4,0.4,0.2))
                
                #consistency
                rng <- range(range(x)[])
                
                if (is.null(pal)){
                  nbrks = length(col)
                  brks <- seq(rng[1], rng[2], length.out = nbrks+1)
                } else {
                  nbrks <- length(brks)
                }
                
                for (i in 1:(length(pg)-1)){
                  
                  for (j in (pg[i]+1): (pg[i+1])){
                    par(mar=c(0,1,2,1))
                    raster::image(x[j], axes=axes, xlab="", ylab="", asp=1, col=col, breaks=brks, main=plot.title[j])
                  }
                  
                  if (i == length(pg)-1 & pg[i+1]%%nplots !=0) {
                    for (i in 1:(nplots - pg[i+1]%%nplots)) {
                      plot(1, type="n", axes=F, ylab=NULL, xlab=NULL)
                    }
                  }
                  par(mar=c(6,10,2,10))
                  plot(rng,c(0,5), type="n", axes=FALSE, ylab="", xlab="", xaxs="i", yaxs="i")
                  image(x=brks, z=as.matrix(brks), col=col, add=TRUE)
                  box()
                  
                  par(xpd=TRUE)
                  labs <- pretty(rng, n=10)
                  labs <- labs[labs > rng[1] & labs < rng[2]]
                  text(x = labs, y=-8, labels = labs, cex=1.5)
                  
                  for(l in 1:length(labs)){
                    lines(c(labs[l], labs[l]), c(-3, 0))
                  }
                  
                  graphics::text(x=mean(rng), y=10, labels=legend.title, cex=1.4, font=2)
                  devAskNewPage(ask=ask)
                }
                
              }
              
            }
            
          }
)

mapplot(demo)
#' @rdname mapplot
setMethod("mapplot", signature="SpatialPolygonsDataFrame", 
	definition = function(x, col="lightgrey", ...){
		sp::plot(x, col=col,...)
	}
)

#' @rdname mapplot
setMethod("mapplot", signature="SpatialPolygons", 
	definition = function(x, col="lightgrey",  ...){
		sp::plot(x, col=col,...)
	}
)
