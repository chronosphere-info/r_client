#' Redefine bounds of a named matrix
#' 
#' The function restructures a \code{\link[base]{matrix}} and extends its current limits to a range defined by a names attribute
#' 
#' This is essentially a subsetting function that allows you to subset even when the rownames or colnames vector
#' extends beyond the bounds of a matrix and traditional subsetting methods result in the notorious 'out of bounds' error.
#' @param x The matrix to be restructured.
#' @param cols Column names guiding the restructuring.
#' @param rows Row names guiding the restructuring.
#' 
#' @return A matrix with extended bounds. 
#' @examples
#' a<-matrix(1:9, ncol=3)
#' rownames(a) <- c("a", "c", "d")
#' newbounds(a, rows=letters[1:5])
#' @export
newbounds <- function(x, cols=NULL, rows=NULL){
  if(!is.matrix(x)) stop("The newbounds() function is only applicable to matrices.")
  
  if(!is.null(rows)){
    if(is.null(rownames(x))) stop("The matrix must have rownames.")
    newX <- matrix(NA, ncol=ncol(x), nrow=length(rows))
    colnames(newX) <- colnames(x)
    rownames(newX) <- rows
    # reorder items to match the new order
    ordering <- rows[rows%in%rownames(x)]
    x2 <- x[ordering, , drop=FALSE]
    
    # insert into new bounds
    newX[rows%in%rownames(x2), ] <- x2[rownames(x2)%in%rows, , drop=FALSE]
  }
  if(!is.null(cols)){
    if(is.null(colnames(x))) stop("The matrix must have colnames.")
    newX <- matrix(NA, nrow=nrow(x), ncol=length(cols))
    rownames(newX) <- rownames(x)
    colnames(newX) <- cols
    # reorder items to match the new order
    ordering <- cols[cols%in%colnames(x)]
    x2 <- x[,ordering , drop=FALSE]
    
    # insert into new bounds
    newX[,cols%in%colnames(x)] <- x[, colnames(x)%in%cols, drop=FALSE]
  }
  return(newX)
}



#' Code snippets defining ranges based on points located on a plot
#' 
#' The function returns snippets of code that you can paste in your script after you select points on a plot. Useful for defining areas on a map. The default methods assume that you will first click in the bottom left and then in the bottom right corner.
#' 
#' @param round (\code{integer}) Number of digits to round to, can be two values, first is for \code{x} second for \code{y}. 
#' @param f (\code{character}) A single letter value specifying for which function's arugment format you want to get parameters. \code{"p"} is for \code{plot}, \code{"r"} is for \code{\link[graphics]{rect}}, \code{"s"} is for \code{\link[graphics]{segments}}. \code{"e"} returns a call to create an \code{\link[raster]{extent}} class object from the package \code{raster}. \code{"m"} will return code to define a 2 column matrix.
#' @param n (\code{integer}) The number of points to request. 
#' @param ... arguments passed to the \code{\link[graphics]{locator}} function
#' @return For certain methods (\code{"m"} and \code{"e"}) the function returns a \code{matrix} or \code{extent} class object if the function output is assigned to a name.
#' @examples
#' \donttest{
#' # plot something
#' data(dems)
#' mapplot(dems[1], col="earth")
#' # click 5 times to get the long-lat coords of 5 points
#' # shaper("m",5)
#' # example output:
#' mat <- matrix(c(                
#'   -2.89, 31.55,
#'   3.32, 26.99,
#'   21.17, 17.87,
#'   33.6, 11.03,
#'   5.65, 19.39
#' ), ncol=2, byrow=TRUE)
#' #plot them 
#' points(mat)
#'}
#' @export
shaper <- function(f="p",n=2, round=2,...){
  message("This function will deprecated in the next major version, \n and will be implemented in a separate R package.")
  if(length(round)==1) round <- rep(round,2)
  
  if(n==2){
    if(length(f)==1 & any(f=="s")){
      cat("Click on the first point:        \r")
      p1 <- graphics::locator(1,...)
      flush.console()
      
      cat("Click on the second point:      \r")
      p2 <- graphics::locator(1,...)
      flush.console()
    }else{
      cat("Click in bottom-left corner:    \r")
      p1 <- graphics::locator(1, ...)
      flush.console()
      
      cat("Click in top-right corner:      \r")
      p2 <- graphics::locator(1,...)
      flush.console()
    }
    
    x1<- p1$x
    y1<- p1$y
    x2<- p2$x
    y2<- p2$y
    
    x1 <- round(x1, round[1])
    y1 <- round(y1, round[2])
    x2 <- round(x2, round[1])
    y2 <- round(y2, round[2])
    
    
  }else{
    if(any(f=="s")) stop("You cannot get a segments() call for multiple points.")
    
    cat("Select", n, "points:\r")
    loc <- graphics::locator(n, ...)
    
    xs<-loc$x
    ys<-loc$y
    
    x1 <- round(min(xs), round[1])
    y1 <- round(min(ys), round[2])
    x2 <- round(max(xs), round[1])
    y2 <- round(max(ys), round[2])
    
  }
  # clean the console
  cat("                                \r")
  flush.console()
  
  # stop if the two are the same
  
  # results
  res <- NULL
  
  if(any(f=="p")){
    cat(paste("xlim=c(", deparse(x1),", ", deparse(x2), "), ylim=c(", deparse(y1),", ", deparse(y2), ")", sep=""), "\n")
  }
  
  if(any(f=="r")){
    cat(paste("xleft=", deparse(x1), ", xright=", deparse(x2), ", ybottom=", deparse(y1), ", ytop=", deparse(y2), sep=""), "\n")
  }
  
  if(any(f=="s")){
    cat(paste("x0=", deparse(x1), ", x1=", deparse(x2), ", y1=", deparse(y1), ", y2=", deparse(y2), ")", sep=""), "\n")
  }
  
  if(any(f=="e")){
    xmin <- min(c(x1, x2))
    xmax <- max(c(x1, x2))
    ymin <- min(c(y1, y2))
    ymax <- max(c(y1, y2))
    
    cat("ext <- extent(c(", "\n")
    cat(paste("  xmin = ", deparse(xmin), ",\n", sep=""))
    cat(paste("  xmax = ", deparse(xmax), ",\n", sep=""))
    cat(paste("  ymin = ", deparse(ymin), ",\n", sep=""))
    cat("  ymax =", deparse(ymax), "\n")
    cat("))", "\n")
    res <- raster::extent(c(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
  }
  
  if(any(f=="m")){
    cat("mat <- matrix(c(", "\n")
    
    if(!is.null(round)) xs<-round(xs, round)
    if(!is.null(round)) ys<-round(ys, round)
    for(i in 1:n){
      if(i!=n){
        cat(paste("  ", deparse(xs[i]),", ", deparse(ys[i]), ",\n", sep=""))
      }else{
        cat(paste("  ", deparse(xs[i]),", ", deparse(ys[i]), "\n", sep=""))
      }
      
    }
    
    cat("), ncol=2, byrow=TRUE)", "\n")
    res <- matrix(c(xs, ys), ncol=2)
  }
  invisible(res)
  
}

#' Names as numerics
#' 
#' The set of functions return names of objects directly transformed to numeric values.
#' 
#' @param x Object with names, colnames or rownames attributes.
#' @rdname nums
#' @return Numeric vector.
#' @examples
#' data(dems)
#' # ages as numerics
#' nums(dems)
#' # younger than 20Ma
#' dems[nums(dems)<20]
#' 
#' 
#' @export
nums <- function(x){
  as.numeric(names(x))
}

#' @rdname nums
#' @export
colnums<- function(x){
  as.numeric(colnames(x))
}

#' @rdname nums
#' @export
rownums <- function(x){
  as.numeric(rownames(x))
}

# Get operating system 
# 
# This package returns the type of OS you use.
# 
# This function and derivates have been circling over the web, I could not trace the original source.
getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


# get directory from a paht
dirFromPath <- function(x){
  all <- unlist(strsplit(x, "/"))
  paste(all[-length(all)], collapse="/")
}

# get directory from a paht
fileFromPath <- function(x){
  all <- unlist(strsplit(x, "/"))
  paste(all[length(all)], collapse="/")
}

# from: https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
randomString <- function(n=1, length=12){
  # initialize vector
  randomString <- c(1:n)                  
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE), collapse="")
  }
  return(randomString)
}




# one dimensional subscript of n dimensional array on a given margin
marginsubset <- function(x, mar, i){
  # number of dimensions necessary
  dims <- length(dim(x))
  
  # construct subsetting call
  callThis <- paste("x[", paste(rep(",",mar-1), collapse=""),"i", paste(rep(",", dims-mar), collapse=""), "]", collapse="")
  
  # as an expression
  express <- parse(text=callThis)
  
  eval(express)
}


#Accessing file within the package
pkg_file <- function(...) {
  system.file(..., package = "chronosphere")
}





# function used by mapedge

detailedBounds <- function(x,y, xmin=-180, xmax=180, ymin=-90, ymax=90){
  rbind(
    cbind(seq(xmin, xmax, length.out=x), rep(ymax, x)),
    cbind(rep(xmax, y), seq(ymax, ymin, length.out=y)),
    cbind(seq(xmax, xmin, length.out=x), rep(ymin, x)),
    cbind(rep(xmin, y), seq(ymin, ymax, length.out=y))
  )
}


#' Function to quickly draft the edge of the equirectangular projection 
#' 
#' Function to plot the edge of a map with different projections.
#' 
#' @param x (\code{numeric}) Number of segments in the x (longitude) dimension. 
#' @param y (\code{numeric}) Number of segments in the y (latitude) dimension. 
#' @param xmin (\code{numeric}) Minimum value of x (longitude).
#' @param xmax (\code{numeric}) Minimum value of x (longitude).
#' @param ymin (\code{numeric}) Maximum value of y (latitude).
#' @param ymax (\code{numeric}) Maximum value of y (latitude).
#' 
#' @return A \code{SpatialPolygons} class object.
#' @examples
#' # requires rgdal
#' edge <- mapedge()
#' molledge <- spTransform(edge, CRS("+proj=moll"))
#' 
#' @export
mapedge <- function(x=360, y=180, xmin=-180, xmax=180, ymin=-90, ymax=90){
	# return a rectangle
  	rectangle <- detailedBounds(x, y, xmin, xmax, ymin, ymax)

  	# now make it a SpatialPolygons
  	final <- SpatialPolygons(list(Polygons(list(Polygon(rectangle)), ID="0")), proj4string=CRS("+proj=longlat"))

  	# return object
  	return(final)
}
