#' Wrapper function to plot maps of different classes
#' 
#' This function plots Raster and sp-type objects.
#' 
#' @return None.
#' @param x Object to be plotted 
#' @param col (\code{character}) Color or color scheme of the plot. See \code{\link{ramps}} for available palettes (\code{ipccLine} and \code{ipccRCP} are not available).
#' @param rgb set to (\code{TRUE}) to make a red-green-blue plot based on three layers or bands.
#' @param legend (\code{logical}) Triggers whether the legend of a \code{\link[raster:raster]{RasterLayer}} would be plotted.
#' @param axes (\code{logical}) Should axes be displayed?
#' @param box (\code{logical}) Should bounding boxes be displayed?
#' @param ncol \code{numeric}) Set number of columns in a multi-plot for a single variable. For a \code{\link[chronosphere:RasterArray-class]{RasterArray}} with multiple variables, this number is automatically set to the number of variables. 
#' @param legend.title (\code{character}) Title for the legend, if \code{legend = TRUE}. 
#' @param plot.title (\code{character}) The title for each individual plot. Only available for a single variable at the moment.
#' @param rowlabels (\code{character}) label for each row of the overall plot. Uses the rownames of the \code{\link[chronosphere:RasterArray-class]{RasterArray}} by default. Only availble for multivariate \code{\link[chronosphere:RasterArray-class]{RasterArray}}s. 
#' @param multi (\code{logical}) Should the plots be printed on multiple pages? 
#' @param ask (\code{logical} or \code{NULL}) If \code{multi.page = TRUE} and \code{ask = TRUE}, then the user will be prompted before a new page of output is started
#' @param ... arguments passed to class-specific methods.
#' @examples
#' 
#'  #single variable
#' 	data(dems)
#' 	mapplot(dems, ncol=4)
#' 	
#' @rdname mapplot
#' @exportMethod mapplot
setGeneric("mapplot", function(x,...) standardGeneric("mapplot"))

#' @rdname mapplot
setMethod("mapplot", signature="RasterLayer", 
          definition = function(x, col="gradinv", axes=FALSE, box=FALSE, legend=FALSE, legend.title=NULL,...){
            
            if(length(col)==1){
              if(col %in% c("ocean", "gradinv", "terra", "ipccTemp", "ipccPrec", "wet")){
                raster::plot(x,legend=FALSE, col=eval(parse(text = col))(255), axes=axes, box=box, ...)
                
                # #save par from raster
                nplots <- par()$mfrow[1] * par()$mfcol[2]
                
                if (nplots == 1){
                  # #save par from raster
                  para <- names(par()) 
                  para <- para[-which(para %in% c("mfcol", "mfrow", "mfg", "fig", "cin", "cra", "csi", "cxy", "din", "page"))]
                  old.par <- par(para)
                  
                  on.exit(graphics::par(old.par))
                }
                
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
                #save from raster
                nplots <- par()$mfrow[1] * par()$mfcol[2]
                
                if (nplots == 1){
                  # #save par from raster
                  para <- names(par()) 
                  para <- para[-which(para %in% c("mfcol", "mfrow", "mfg", "fig", "cin", "cra", "csi", "cxy", "din", "page"))]
                  old.par <- par(para)
                  
                  on.exit(graphics::par(old.par))
                }
                
                if (legend == TRUE){
                  par(oma=c(1,0,0,0))

                  plot(x, col=c(ocean(length(negBreaks)-1), 
                                terra(length(posBreaks))), legend.only=TRUE, horizontal = TRUE,
                       legend.args=list(text=legend.title,side=1, font=2, line=2.5, cex=1))
                }
              }
            }else{
              raster::plot(x,legend=FALSE, col=col, axes=axes, box=box,...)
              
              nplots <- par()$mfrow[1] * par()$mfcol[2]
              
              if (nplots == 1){
                # #save par from raster
                para <- names(par()) 
                para <- para[-which(para %in% c("mfcol", "mfrow", "mfg", "fig", "cin", "cra", "csi", "cxy", "din", "page"))]
                old.par <- par(para)
                
                on.exit(graphics::par(old.par))
              }
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
            
            raster::plotRGB(x, ...)
            nplots <- par()$mfrow[1] * par()$mfcol[2]
            
            if (nplots == 1){
            # #save par from raster
            para <- names(par()) 
            para <- para[-which(para %in% c("mfcol", "mfrow", "mfg", "fig", "cin", "cra", "csi", "cxy", "din", "page"))]
            old.par <- par(para)
            
            on.exit(graphics::par(old.par))
            }
            
          }
)

#' @rdname mapplot
setMethod("mapplot", signature="RasterArray", 
          definition = function(x, col="gradinv", rgb=FALSE, legend=FALSE, axes=FALSE, box=FALSE, 
                                ncol = 3, legend.title=NULL, plot.title =NULL, rowlabels=rownames(x), multi=FALSE, ask=FALSE,...){
            
            
            if(rgb == TRUE){ #plot with rgb bands
              raster::plotRGB(x@stack, ...) 
              
              #save par from raster
              nplots <- par()$mfrow[1] * par()$mfcol[2]
              
              if (nplots == 1){
                # #save par from raster
                para <- names(par()) 
                para <- para[-which(para %in% c("mfcol", "mfrow", "mfg", "fig", "cin", "cra", "csi", "cxy", "din", "page"))]
                old.par <- par(para)
                
                on.exit(graphics::par(old.par))
              }
              
            } else { #uni and multivariate rasterArrays
              old.par <- par(no.readonly = TRUE)
              on.exit(graphics::par(old.par))
              
              #number of variables in array
              nvars <- ifelse (is.na(dim(x)[2]), 1, dim(x)[2])
              
              #check for colour palettes - might need to check for typos
              
              #set colours
              if(length(col)==nvars){
                col <- as.list(col)
              } else col <- as.list(rep(col,nvars))
              
              for (i in 1:nvars){
                if(col[[i]][1] %in% c("ocean", "gradinv", "terra", "ipccTemp", "ipccPrec", "wet")){
                  col[[i]]=eval(parse(text = col[[i]]))(255)
                }
                
                if(col[[i]][1] == "earth"){
                  pal_earth <- i #save for later to be used for rng and brks
                  brks <- list() 
                  rng <- list()
                  
                  negBreaks <- seq(-10000, 0, by=50)
                  posBreaks <- seq(0, 10000, by=50)
                  
                  brks[[i]] <- c(negBreaks[1:(length(negBreaks)-1)], posBreaks)
                  col[[i]] <- c(ocean(length(negBreaks)-1), terra(length(posBreaks)-1))
                  
                  rng[[i]] <- range(brks[[i]])
                } else {
                  brks <- NULL
                  pal_earth <- NULL}
              }
              
              #multivariate
              if (nvars > 1){
                devAskNewPage(ask=FALSE)
                
                #multiple or single page layout
                if(multi == TRUE){
                  ncol=nvars
                  nrow=2
                  pg <-if(dim(x)[1]%%nrow == 0) seq(0,dim(x)[1],nrow) else  c(seq(0,dim(x)[1],nrow), dim(x)[1]) #index per page
                  pg2 <- ceiling(dim(x)[1]/nrow) #number of pages
                } else {
                  pg2 <- 1
                  
                  ncol = nvars
                  nrow <- nrow(x)
                  pg <- c(0, nrow)
                }
                
                #generate layout
                nplots=ncol*nrow
                
                if (legend == TRUE) {
                  #set legend to list
                  if (is.null(legend.title)) legend.title <-colnames(x)
                  
                  if(length(legend.title)==nvars){
                    legend.title <- as.list(legend.title)
                  } else legend.title <- as.list(rep(legend.title,nvars))
                  
                  m <- matrix(c(1:(nplots+nvars)),nrow = nrow+1 ,ncol = ncol,byrow = TRUE)
                  layout(mat = m,heights = c(rep(0.4, nrow),0.2))
                } else{
                  m <- matrix(c(1:nplots),nrow = nrow ,ncol = ncol,byrow = TRUE)
                  layout(mat = m,heights = c(rep(0.4, nrow)))
                }
                
                # if one of the chosen colours were set to earth
                if (is.null(pal_earth)){
                  #get ranges for each variable
                  rng <- list()
                  brks <- list()
                  pal_earth <- nvars+1 #so that subsetting works
                }
                
                #breaks for each variable
                nbrks = lapply(col, length) 
                
                
                for (n in (1:nvars)[-pal_earth]){
                  rng[[n]] <- range(range(x[,n])[,], na.rm=TRUE)
                }
                
                for (i in (1:length(nbrks))[-pal_earth]){
                  brks[[i]] <- seq(rng[[i]][1], rng[[i]][2], length.out = nbrks[[i]]+1)
                }
                
                na.raster <- is.na(x)
                
                for (i in 1:pg2){
                  
                  for (j in (pg[i]+1):pg[i+1]){
                    
                    #main plots
                    for (k in 1:nvars){
                      par(mar=c(0,1,2,1))
                      
                      
                      if (na.raster[j,k]){
                        plot(c(0,1), c(0,1), type="n", axes=FALSE, ylab="", xlab="")
                        graphics::text(0.5, 0.5, labels = c("Plot \nnot available"), font=2)
                      } else {
                        raster::image(x[j,k], axes=axes, xlab="", ylab="", asp=1, col=col[[k]], breaks=brks[[k]], main=plot.title)
                        
                        if (box == TRUE) box()
                        
                      }
                      
                      devAskNewPage(ask=FALSE)
                      
                      if (k == 1) mtext(rowlabels[j], adj=0.03, line=0, font=2, cex=0.8) # colnames
                    }
                  }
                  
                  if (legend == TRUE){
                    #add legend
                    for (k in 1:nvars){
                      
                      par(mar=c(4,2,2,2))
                      
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
                      
                      graphics::text(x=mean(rng[[k]]), y=10, labels=legend.title[[k]], cex=1.4, font=2)
                    }
                  }
                  
                  #ask
                  devAskNewPage(ask=ask)
                }
              } else {
                devAskNewPage(ask=FALSE)
                
                if (is.null(plot.title)){
                  plot.title <- names(proxy(x))
                }
                
                if(multi == TRUE){
                  nrow = 2 #unless < 3
                  nplots = nrow * ncol
                  pg <- if(nlayers(x)%%nplots == 0) seq(0,nlayers(x),nplots) else  c(seq(0,nlayers(x),nplots), nlayers(x))
                } else {
                  nrow <- ceiling(nlayers(x)/ncol)
                  nplots <- nrow*ncol
                  pg <- c(0,nlayers(x)) 
                }
                
                if (legend == TRUE){
                  #set legend to list
                  if (is.null(legend.title)) legend.title <- ""
                  
                  m <- matrix(c(1:nplots,rep((nplots+1), ncol)), nrow = nrow+1,ncol = ncol,byrow = TRUE)
                  layout(mat = m,heights = c(rep(0.4, nrow),0.3))
                } else {
                  m <- matrix(c(1:nplots), nrow = nrow,ncol = ncol,byrow = TRUE)
                  layout(mat = m,heights = rep(0.4, nrow))
                }
                
                
                if (is.null(brks)){ #if earth hasn't been assigned
                  #consistent legend
                  rng <- range(range(x, na.rm = TRUE)[])
                  
                  nbrks = length(col[[1]])
                  brks[[1]] <- seq(rng[1], rng[2], length.out = nbrks+1)
                }
                
                na.raster <- which(is.na(x))
                
                for (i in 1:(length(pg)-1)){
                  
                  for (j in (pg[i]+1): (pg[i+1])){
                    par(mar=c(0,1,2,1))
                    
                    
                    if (j %in% na.raster){ #plot empty if na
                      plot(c(0,1), c(0,1), type="n", axes=FALSE, ylab="", xlab="")
                      graphics::text(0.5, 0.5, labels = c("Plot \nnot available"), font=2)
                    } else {
                      raster::image(x[j], axes=axes, xlab="", ylab="", asp=1,
                                    col=col[[1]], breaks=brks[[1]], 
                                    main=plot.title[j])
                    }
                    if (box == TRUE) box()
                  }
                  
                  #empty plots if needed
                  if (i == length(pg)-1 & pg[i+1]%%nplots !=0) {
                    for (i in 1:(nplots - pg[i+1]%%nplots)) {
                      plot(1, type="n", axes=FALSE, ylab=NULL, xlab=NULL)
                    }
                  }
                  
                  if (legend == TRUE){
                    #add legend
                    if (multi == TRUE){ 
                      par(mar=c(6,4,3,4))
                    }else{
                      par(mar=c(2.5,4,2,4))
                    }
                    plot(rng,c(0,5), type="n", axes=FALSE, ylab="", xlab="", xaxs="i", yaxs="i")
                    image(x=brks[[1]], z=as.matrix(brks[[1]]), col=col[[1]], add=TRUE)
                    box()
                    
                    par(xpd=TRUE)
                
                    labs <- pretty(rng, n=10)
                    labs <- labs[labs > rng[1] & labs < rng[2]]
                    text(x = labs, y=-8, labels = labs, cex=1.5)
                    
                    for(l in 1:length(labs)){
                      lines(c(labs[l], labs[l]), c(-3, 0))
                    }
                    
                    graphics::text(x=mean(rng), y=10, labels=legend.title, cex=1.4, font=2)
                  }
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



#' Shorthand for the plotting \code{\link[chronosphere:RasterArray-class]{RasterArray}} and \code{\link[chronosphere:SpatialArray-class]{SpatialArray}} objects
#' 
#' This \code{plot}, method executes the \code{\link{mapplot}} function on the \code{\link[chronosphere:RasterArray-class]{RasterArray}} or \code{\link[chronosphere:SpatialArray-class]{SpatialArray}}object.
#' 
#' @return None.
#' @param x A (\code{\link[chronosphere:RasterArray-class]{RasterArray}} or \code{\link[chronosphere:SpatialArray-class]{SpatialArray}}) Object to be plotted.
#' @param y Not implemented yet.
#' @param ... Arguments passed to the \code{\link{mapplot}} function.
#' @rdname plots
#' @examples
#' data(dems)
#' plot(dems)
#' @exportMethod plot
setMethod(
  "plot",
  signature=c("RasterArray", "missing"),
  function(x,y, ...){
    mapplot(x,...)
  }
)


#' @rdname plots
#' @exportMethod plot
setMethod(
  "plot",
  signature=c("SpatialArray", "missing"),
  function(x,y, ...){
    mapplot(x,...)
  }
)



