# SpatialArray basics
setClassUnion("VectorSpatialClasses", c("SpatialPoints", "SpatialPointsDataFrame", "SpatialLines", "SpatialLinesDataFrame", "SpatialPolygons", "SpatialPolygonsDataFrame"))

# to be used as index in the the Arrays
setClassUnion("arrayORmatrixORvector", c("vector", "matrix", "array"))


#' Stack of Spatial Objects
#' 
#' Vector data in the same CRS organized into a vector.
#' 
#' The class implements a stack of vector data that mimic \code{\link[raster:Raster-classes]{RasterStack}}-class objects, only with vector data. 
#' Classes, such as \code{link[sp]{SpatialPoints}}, \code{link[sp]{SpatialPointsDataFrame}}, \code{link[sp]{SpatialLines}}, \code{link[sp]{SpatialLinesDataFrame}},
#' \code{link[sp]{SpatialPolygons}} and \code{link[sp]{SpatialPolygonsDataFrame}} can be concatenated to a vector/list, where elements can be accessed
#' using list-type subsetting. The only restriction is that the items must share the same \code{\link[sp]{CRS}}. 
#' 
#' The class has two slots:
#' Spatials: List of Spatial items.
#' CRS: The coordinate reference system (\code{\link[sp]{CRS}}). 
#' bbox: The bounding box of all items. 
#' 
#' 
#' @param Spatials A \code{list} of Spatial objects or a \code{character} vector of file names identifying items for readOGR, in case the rgdal package is installed..
#' @param proj4string A \code{\link[sp]{CRS}}-class object.
#' @param verbose A \code{logical} value. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A \code{SpatialStack} class object.
#' @exportClass SpatialStack
SpatialStack <- setClass("SpatialStack", slots=list(Spatials="list", proj4string="CRS", bbox="matrix"))

#' Virtual Array of Vector Spatial data
#' 
#' Array template for Spatial object
#' 
#' The class implements structures to organize Spatial objects that have the same CRS. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: SpatialStack, the actual data.
#' index: A proxy object that represents the organization of the layers. 
#' 
#' 
#' @param stack A \code{SpatialStack} class object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A \code{SpatialStack} class object.
##' @examples
##' # data import
##'   data(dems)
##'   st <-dems@stack
##'   ind <- 1:nlayers(st)
##'   names(ind) <- letters[1:length(ind)]
##'   ra<- RasterArray(stack=st, index=ind)
##'   
#' @exportClass SpatialArray
SpatialArray <- setClass("SpatialArray", slots=list(index="arrayORmatrixORvector", stack="SpatialStack"))

#' Virtual Array of RasterLayers
#' 
#' Array template for RasterLayers
#' 
#' The class implements structures to organize RasterLayers that have the same dimensions. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: RasterStack, the actual data.
#' index: A proxy object that represents the organization of the layers. 
#' 
#' 
#' @param stack A \code{RasterStack} class object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A \code{RasterArray} class object.
#' @examples
#' # data import
#'   data(dems)
#'   st <-dems@stack
#'   ind <- 1:nlayers(st)
#'   names(ind) <- letters[1:length(ind)]
#'   ra<- RasterArray(stack=st, index=ind)
#'   
#' @exportClass RasterArray
RasterArray <- setClass("RasterArray", slots=list(index="arrayORmatrixORvector", stack="RasterStack"))

# to be used for shared methods
setClassUnion("XArray", c("RasterArray", "SpatialArray"))

