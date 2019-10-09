#' Calculate west to east spatial gradient
#' 
#' \code{WE_gradient} returns the west to east spatial gradient in a variable between neighbouring
#' cells (corrected for distance by latitude).
#' 
#' Based on Burrows et al. (2011)
#' 
#' @param x is a single-layer raster containing the variable of interest
#' averaged over a particular time period.
#' 
#' @return RasterLayer
#'
#' @references Loarie, S. R., P. B. Duffy, H. Hamilton, G. P. Asner, C. B. Field, and D. D. Ackerly. 2009. The velocity of climate change. Nature 462:1052–5.
#' 
#' Burrows, M. T., D. S. Schoeman, L. B. Buckley, P. Moore, E. S. Poloczanska, K. M. Brander, C. Brown, J. F. Bruno, C. M. Duarte, B. S. Halpern, J. Holding, C. V Kappel, W. Kiessling, M. I. O’Connor, J. M. Pandolfi, C. Parmesan, F. B. Schwing, W. J. Sydeman, and A. J. Richardson. 2011. The pace of shifting climate in marine and terrestrial ecosystems. Science (New York, N.Y.) 334:652–5.
#' 
#' @export
WE_gradient <- function(x) {
  w <- matrix(rep(1, 9), nrow=3, ncol=3)
  lat <- sp::coordinates(x)[, 2]
  WE_diff <- raster::focal(x, w = w, nrow=3, ncol=3, pad = TRUE,
                           fun = function(x, ...) {
                             ba <- x[6] - x[5]
                           }
  )
  WE_adjusted <- WE_diff / (111.325 * cos (lat * pi / 180))
  return(WE_adjusted)
}

#' Calculate north to south spatial gradient
#' 
#' \code{get_NS_gradient} returns north to south spatial gradient in a variable between neighbouring
#' cells (corrected for distance by latitude).
#' 
#' Based on Burrows et al. (2011)
#' 
#' @param x is a single-layer raster containing the variable of interest
#' averaged over a particular time period.
#' 
#' @return RasterLayer
#'
#' @references Loarie, S. R., P. B. Duffy, H. Hamilton, G. P. Asner, C. B. Field, and D. D. Ackerly. 2009. The velocity of climate change. Nature 462:1052–5.
#' 
#' Burrows, M. T., D. S. Schoeman, L. B. Buckley, P. Moore, E. S. Poloczanska, K. M. Brander, C. Brown, J. F. Bruno, C. M. Duarte, B. S. Halpern, J. Holding, C. V Kappel, W. Kiessling, M. I. O’Connor, J. M. Pandolfi, C. Parmesan, F. B. Schwing, W. J. Sydeman, and A. J. Richardson. 2011. The pace of shifting climate in marine and terrestrial ecosystems. Science (New York, N.Y.) 334:652–5.
#' 
#' @export
NS_gradient <- function(x) {
  w <- matrix(rep(1, 9), nrow=3, ncol=3)
  NS_diff <- raster::focal(x, w = w, nrow=3, ncol=3, pad = TRUE,
                           fun = function(x, ...) {
                             be <- x[2] - x[5]
                           }
  )
  NS_adjusted <- NS_diff / 111.325
  return(NS_adjusted)
}

#'Calculate the spatial gradient of climatic change
#'
#'\code{spatial_gradient} calculates the spatial gradient of climatic change, given a variable of interest over a period of time.
#'
#'Based on Burrows et al. (2011)
#'
#' @param x is a single-layer raster containing the variable of interest
#' averaged over a particular time period.
#' 
#' @return RasterLayer
#' 
#' @references Loarie, S. R., P. B. Duffy, H. Hamilton, G. P. Asner, C. B. Field, and D. D. Ackerly. 2009. The velocity of climate change. Nature 462:1052–5.
#' 
#' Burrows, M. T., D. S. Schoeman, L. B. Buckley, P. Moore, E. S. Poloczanska, K. M. Brander, C. Brown, J. F. Bruno, C. M. Duarte, B. S. Halpern, J. Holding, C. V Kappel, W. Kiessling, M. I. O’Connor, J. M. Pandolfi, C. Parmesan, F. B. Schwing, W. J. Sydeman, and A. J. Richardson. 2011. The pace of shifting climate in marine and terrestrial ecosystems. Science (New York, N.Y.) 334:652–5.
#' 
#' @export
spatial_gradient <- function(x) {
  f <- matrix(rep(1, 9), nrow=3, ncol=3)
  x_gradient <- raster::focal(WE_gradient(x), w = f, nrow=3, ncol=3, pad = TRUE,
                              fun = function(x, ...) {
                                #browser()
                                wt <- c(1, 2, 1, 2, 0, 2, 1, 2, 1)
                                weighted.mean(x, wt, na.rm = TRUE)
                              })
  y_gradient <- raster::focal(NS_gradient(x), w = f, nrow = 3, ncol = 3, pad = TRUE,
                              fun = function(x, ...) {
                                wt <- c(1, 2, 1, 2, 0, 2, 1, 2, 1)
                                weighted.mean(x, wt, na.rm = TRUE)
                              })
  
  # Get magnitude of resultant vector
  magnitude <- sqrt(x_gradient^2 + y_gradient^2)
  
  # Get angle of resultant vector
  angle <- raster::atan2(x_gradient, y_gradient) * 180 / pi
  
  # Create correction raster to produce positive angles
  neg_angles <- (angle < 0) * 360
  
  angle <- raster::overlay(x = angle, y = neg_angles, fun = function(x, y) {x + y})
  
  spatial_grad<- raster::brick(magnitude, angle)
  names(spatial_grad) <- c('spatial_gradient', 'angle')
  return(spatial_grad)
}

#'Calculate the rate of climate change over a given time period for a specified unit of time
#'
#'\code{sp_linear_rate} calculates the spatial gradient of climatic change, given a variable of interest over a period of time.
#'
#'Based on Burrows et al. (2011)
#'
#' @param x is a raster object of the variable of interest, with each layer representing time
#' @param period is a vector contains the time intervals corresponding to each raster layers, with negative values representing time before present.
#' @param t is the unit of time considered, e.g. if the rate per 5 myr is wanted, then t=5. 
#' 
#' @return RasterLayer
#' 
#' @references Loarie, S. R., P. B. Duffy, H. Hamilton, G. P. Asner, C. B. Field, and D. D. Ackerly. 2009. The velocity of climate change. Nature 462:1052–5.
#' 
#' Burrows, M. T., D. S. Schoeman, L. B. Buckley, P. Moore, E. S. Poloczanska, K. M. Brander, C. Brown, J. F. Bruno, C. M. Duarte, B. S. Halpern, J. Holding, C. V Kappel, W. Kiessling, M. I. O’Connor, J. M. Pandolfi, C. Parmesan, F. B. Schwing, W. J. Sydeman, and A. J. Richardson. 2011. The pace of shifting climate in marine and terrestrial ecosystems. Science (New York, N.Y.) 334:652–5.
#' 
#' @export
#' 
sp_linear_rate <- function (x, period=NULL, t=1){
  
  #add something to subset here
  if(!is.null(period)){
    ID <- paste0("X", abs(period))
  
    x <- raster::subset(x, ID)
  }else{
    period<-raster::nlayers(x)
  }
  
  #apply to raXster
  lin_change <- raster::calc(x, function(x) { 
    if (sum(is.na(x)) < length(x)) {
      slope <- t*lm(x ~ period, na.action = na.exclude)$coefficients[2]
      return(slope)
    }
    return(NA)
  })
  return(lin_change)
  
}


#'Calculates the spatial average of a variable
#'
#'\code{avg_var} calculates the spatial the spatial average of a variable of interest over a period of time.
#'
#' @param x is a raster object of the variable of interest, with each layer representing time
#' @param period corresponds to the name of the raster layers of x of interest. Leave blank to calculate average of all the layers. 
#' @return RasterLayer
#' 
#' 
#' @export
avg_var <- function(x, period = NULL){
  
  if (!is.null(period)){
    ID <- paste0("X", abs(period))
    x <- raster::subset(x, ID)
  } 
  
  avg <- raster::calc(x, mean, na.rm = TRUE) #calculate average of rasters
  return(avg)
}

#' Calculate the velocity of climate change over a given set of years
#'
#' @param x is the climate data as a raster object.
#' @param period is a numeric vector specifying the time interval over which the velocity of
#' climate change will be calculated. Should correspond to the name of the raster layers 
#' although time intervals before present are given as negative values.
#' @param truncate logical expression indicating whether minimum and maximum
#' values should be constrained to a certain threshold. Default is TRUE.
#' @param thr is the threshold to contrain the minimum and maximum values of the climate velocity.
#' 
#' @return a two-layer raster containing 'velocity_mag' and 'velocity_angle',
#' the magnitude and direction of the velocity of climate change, respectively.

#' @references Loarie, S. R., P. B. Duffy, H. Hamilton, G. P. Asner, C. B. Field, and D. D. Ackerly. 2009. The velocity of climate change. Nature 462:1052–5.
#' 
#' Burrows, M. T., D. S. Schoeman, L. B. Buckley, P. Moore, E. S. Poloczanska, K. M. Brander, C. Brown, J. F. Bruno, C. M. Duarte, B. S. Halpern, J. Holding, C. V Kappel, W. Kiessling, M. I. O’Connor, J. M. Pandolfi, C. Parmesan, F. B. Schwing, W. J. Sydeman, and A. J. Richardson. 2011. The pace of shifting climate in marine and terrestrial ecosystems. Science (New York, N.Y.) 334:652–5.
#' @export
vocc <- function(x, period, t=1, truncate = TRUE, thr=500) {
  
  linear_change <- sp_linear_rate(x, period, t=t)
  
  avg <- avg_var(x, period)
  spatial_gradient <- spatial_gradient(avg)
  
  velocity <- linear_change / raster::subset(spatial_gradient, 'spatial_gradient')
  
  if (truncate == TRUE) {
    m <- c(-Inf, -thr, -thr, thr, Inf, thr)
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    velocity <- raster::reclassify(velocity, m)
  }
  
  angle <- raster::subset(spatial_gradient, 'angle')
  
  velocity_brick <- raster::brick(velocity, angle)
  names(velocity_brick) <- c('velocity', 'angle')
  return(velocity_brick)
}

