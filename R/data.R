#' Temperature dataset from modelled simulations 
#'
#' A dataset containing temperature data from Paul Valdes and Chris Scotese for the time
#' 0, 4 and 11 Ma. 
#'
#' @format A raster brick of 3 layers corresponding to time intervals.
#' \describe{
#'   \item{X0}{temperature data for 0 Ma}
#'   \item{X4}{temperature data for 4 Ma}
#'   \item{X11}{temperature data for 11 Ma}
#' }
#' @source Valdes simulations
"clim"


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
