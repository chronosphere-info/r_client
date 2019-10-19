#' Redefine bounds of a named matrix
#' 
#' The function restructures a matrix and extends its current limits to a range defined by a names attribute
#' 
#' This is essentially a subsetting function that allows you to subset even when the rownames or colnames vector
#' extends beyond the bounds of a matrix and traditional subsetting methods result in the notorious 'out of bounds' error.
#' @param x The matrix to be restructured.
#' @param cols Column names guiding the restructuring.
#' @param rows Row names guiding the restructuring.
#' 
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
		newX[rows%in%rownames(x), ] <- x[rownames(x)%in%rows, ]
	}
	if(!is.null(cols)){
		if(is.null(colnames(x))) stop("The matrix must have colnames.")
		newX <- matrix(NA, nrow=nrow(x), ncol=length(cols))
		rownames(newX) <- rownames(x)
		colnames(newX) <- cols
		newX[cols%in%colnames(x)] <- x[, colnames(x)%in%cols]
	}
	return(newX)
}
