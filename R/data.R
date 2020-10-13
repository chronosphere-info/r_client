#' PaleoDEM rasters from the reconstructions Chris Scotese.
#'
#' A dataset containing the paleoDEM reconstructions of Chris Scotese for the time interval 0 - 45 Ma. 
#'
#' This is a subset of the total dataset, use \code{fetch(dat="paleomap", var="dem", res=1, ver="20190719")} to get the whole set.
#' @format A \code{\link[chronosphere:RasterArray-class]{RasterArray}} with 10 layers. 
#' @usage data(dems)
#' @source 
#' Scotese, C. R. Wright, N. (2018). PALEOMAP Paleodigital Elevation Models (PaleoDEMS) for the Phanerozoic. URL: \url{https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/}
"dems"


#' PaleoMAP PaleoCoastlines (demo)
#'
#' A dataset containing the coastline reconstructions based on the PaleoMAP PaleoDEMS (\link{dems}) and the Paleobiology Database \url{https://paleobiodb.org} for the time interval 0 - 25 Ma. 
#'
#' This is version v7. The article describing the entire set is under review. Once that is published, the entire dataset will be available.
#'
#' @format A \code{\link[chronosphere:SpatialArray-class]{SpatialArray}} with 5 continental margin and 5 paleocoastline layers.
#' @source 
#' Kocsis, A. T., & Scotese, C. R. (2020). PaleoMAP PaleoCoastlines data [Data set]. Zenodo. https://doi.org/10.5281/zenodo.3903164
#' @usage data(coasts)
"coasts"

