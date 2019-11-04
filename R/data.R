#' PaleoDEM rasters from the reconstructions Chris Scotese.
#'
#' A dataset containing the paleoDEM reconstructions of Chris Scotese for the time interval 0 - 45 Ma. 
#'
#' @format A RasterArray with 10 layers.
#' @source Chris Scotese
"dems"


#' Colour gradient ramps for default plotting 
#' 
#' The object contains functions produced by the \code{colorRampPalette} function.
#' 
#' The following ramps are implemented:
#' gradinv(): inverse heatmap.
#' @rdname ramps
#' 
#' @export 
gradinv <- grDevices::colorRampPalette(c("#33358a", "#76acce", "#fff99a",  "#e22c28", "#690720"))
