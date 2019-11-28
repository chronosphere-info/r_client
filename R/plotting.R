#' Wrapper function to plot maps of different classes
#' 
#' This function plots Raster and sp-type objects.
#' 
#' @param x Object to be plotted 
#' @param legend (\code{logical}) Triggers whether the legend of a RasterLayer would be plotted.
#' @param col \code{character} Color or color scheme of the plot. See \code{?ramps} for available palettes.
#' @param ... arguments passed to class-specific methods.
#' @param axes \code{logical} Should axes be displayed?
#' @param box \code{logical} Should bounding boxes be displayed?
#' @param legend.title title for the legend, if legend = TRUE. 
#' @param rgb set to \code{TRUE} to make a red-green-blue plot based on three layers or bands.
#' @param ask NULL or a logical values. If multi.page = TRUE and ask = TRUE, then the user will be prompted before a new page of output is started 
#' @param nplots maximum number of plots per page
#' @param plot.title write!
#' @rdname mapplot
#' @exportMethod mapplot
setGeneric("mapplot", function(x,...) standardGeneric("mapplot"))

#' @rdname mapplot
setMethod("mapplot", signature="RasterLayer", 
          definition = function(x, col="gradinv", axes=FALSE, box=FALSE, legend=FALSE, legend.title=NULL,...){
            if(length(col)==1){
              if(col %in% c("ocean", "gradinv", "terra", "coldhot", "drywet", "wet")){
                par(mar=c(3,1,1,0))
                raster::plot(x,legend=FALSE, col=eval(parse(text = col))(255), axes=axes, box=box, ...)
                
                if (legend == TRUE){
                  par(oma=c(1,0,0,0))
                  plot(x, col=eval(parse(text = col))(255), legend.only=TRUE, horizontal = TRUE,
                       legend.args=list(text=legend.title,side=1, font=2, line=2.5, cex=1))
                }
              }
              
              if (col == "earth"){
                negBreaks <- seq(-10000, 0, by=50)
                posBreaks <- seq(0, 10000, by=50)
                
                raster::plot(x,legend=FALSE, 
                             breaks =c(negBreaks[1:(length(negBreaks)-1)], posBreaks), 
                             col=c(ocean(length(negBreaks)-1), 
                                   terra(length(posBreaks))), axes=axes, box=box,...)
                
                if (legend == TRUE){
                  par(oma=c(1,0,0,0))
                  plot(x, col=c(ocean(length(negBreaks)-1), 
                                terra(length(posBreaks))), legend.only=TRUE, horizontal = TRUE,
                       legend.args=list(text=legend.title,side=1, font=2, line=2.5, cex=1))
                }
              }
            }else{
              raster::plot(x,legend=FALSE, col=col, axes=axes, box=box,...)
              
              if (legend == TRUE){
                par(oma=c(1,0,0,0))
                plot(x, col=eval(parse(text = col))(255), legend.only=TRUE, horizontal = TRUE,
                     legend.args=list(text=legend.title,side=1, font=2, line=2.5, cex=1))
              }
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
          definition = function(x, col="gradinv", rgb=FALSE, legend=FALSE, axes=FALSE, box=FALSE, ask=FALSE,
                                ncol = 3, legend.title=NULL, plot.title =NULL, ...){
            if(rgb == TRUE){
              raster::plotRGB(x@stack, ...)  	    
            } else {
              nvars <- ifelse (is.na(dim(x)[2]), 1, dim(x)[2])
              
              #check for colour palettes - might need to check for typos
              if(length(col)==nvars){
                col <- as.list(col)
              } else col <- as.list(rep(col,nvars))
              
              for (i in 1:nvars){
                if(col[[i]][1] %in% c("ocean", "gradinv", "terra", "coldhot", "drywet", "wet")){
                  col[[i]]=eval(parse(text = col[[i]]))(255)
                }
                
                if(col[[i]][1] == "earth"){
                  negBreaks <- seq(-10000, 0, by=50)
                  posBreaks <- seq(0, 10000, by=50)
                  
                  brks <- c(negBreaks[1:(length(negBreaks)-1)], posBreaks)
                  col[[i]] <- c(ocean(length(negBreaks)-1), terra(length(posBreaks)-1))
                }
                
              } 
              
              #edit for plot title
              if (is.null(plot.title)){
                plot.title <- names(x)
              }
              
              if (nvars > 1){
                devAskNewPage(ask=FALSE)
                
                ncol=nvars
                nrow=2
                nplots=ncol*nrow
                
                pg <-if(dim(x)[1]%%nrow == 0) seq(0,dim(x)[1],nrow) else  c(seq(0,dim(x)[1],nrow), dim(x)[1])
                pg2 <- ceiling(dim(x)[1]/nrow)
                
                m <- matrix(c(1:(nplots+nvars)),nrow = nrow+1 ,ncol = ncol,byrow = TRUE)
                layout(mat = m,heights = c(rep(0.4, nrow),0.2))
                
                rng <- list()
                
                for (n in 1:nvars){
                  rng[[n]] <- range(range(x[n,])[,], na.rm=TRUE)
                }
                
                nbrks = lapply(col, length) 
                brks <- list()
                  
                for (i in 1:length(nbrks)){
                    brks[[i]] <- seq(rng[[i]][1], rng[[i]][2], length.out = nbrks[[i]]+1)
                }
                
                for (i in 1:pg2){
                  
                  for (j in (pg[i]+1):pg[i+1]){
                    
                    for (k in 1:nvars){
                      par(mar=c(0,1,2,1))
                      raster::image(x[j,k], axes=axes, xlab="", ylab="", asp=1, col=col[[k]], breaks=brks[[k]], main=names(x[j,k]))
                      devAskNewPage(ask=FALSE)
                    }
                  }
                  
                  for (k in 1:nvars){
                    par(mar=c(6,4,2,4))
                    plot(rng[[k]],c(0,5), type="n", axes=FALSE, ylab="", xlab="", xaxs="i", yaxs="i")
                    image(x=brks[[k]], z=as.matrix(brks[[k]]), col=col[[k]], add=TRUE)
                    box()
                    
                    par(xpd=TRUE)
                    labs <- pretty(rng[[k]], n=6, min.n = 4)
                    labs <- labs[labs > rng[[k]][1] & labs < rng[[k]][2]]
                    text(x = labs, y=-8, labels = labs, cex=1)
                    
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
                
                nrow = 2 #unless < 3
                nplots = nrow * ncol
                
                pg <- if(nlayers(x)%%nplots == 0) seq(0,nlayers(x),nplots) else  c(seq(0,nlayers(x),nplots), nlayers(x))
                
                m <- matrix(c(1:nplots,rep((nplots+1), ncol)), nrow = nrow+1,ncol = ncol,byrow = TRUE)
                layout(mat = m,heights = c(rep(0.4, nrow),0.2))
                
                #consistent legend
                rng <- range(range(x)[])
                
                nbrks = length(col[[1]])
                brks <- seq(rng[1], rng[2], length.out = nbrks+1)
                
                
                for (i in 1:(length(pg)-1)){
                  
                  for (j in (pg[i]+1): (pg[i+1])){
                    par(mar=c(0,1,2,1))
                    raster::image(x[j], axes=axes, xlab="", ylab="", asp=1,
                                  col=col[[1]], breaks=brks, 
                                  main=plot.title[j])
                  }
                  
                  if (i == length(pg)-1 & pg[i+1]%%nplots !=0) {
                    for (i in 1:(nplots - pg[i+1]%%nplots)) {
                      plot(1, type="n", axes=F, ylab=NULL, xlab=NULL)
                    }
                  }
                  
                  par(mar=c(6,4,2,4))
                  plot(rng,c(0,5), type="n", axes=FALSE, ylab="", xlab="", xaxs="i", yaxs="i")
                  image(x=brks, z=as.matrix(brks), col=col[[1]], add=TRUE)
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
