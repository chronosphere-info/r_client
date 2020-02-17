#' Colour gradient ramps
#'
#' The object contains functions produced by the \code{\link[grDevices:colorRamp]{colorRampPalette}} function.
#' 
# \cr
#' \code{showPal} can be used to display the available palettes. You can use \code{pal = "all"} or \code{pal=""} if you want to look at all the available palettes. 
#' You can also view single palettes individually. The following colour palettes are implemented:
#' \itemize{
#' \item \code{gradinv()}: inverse heatmap.
#' \item \code{ocean()}: ocean bathymetrical colours.
#' \item \code{terra()}: terrestrial topographical colours.
#' \item \code{ipccTemp()}: gradient from blue to red according to the official IPCC AR6 WG2 colour palette.
#' \item \code{ipccPrec()}: gradient from brown to green according to the official IPCC AR6 WG2  colour palette.
#' \item \code{wet()}: gradient from white to green to blue. 
#' \item \code{ipccLine()}: discrete colours for line graphs according to the official IPCC AR6 WG2  colour palette. 
#' \item \code{ipccRCP()}: discrete colours for climate scenarios according to the official IPCC AR6 WG2  colour palette. 
#' }
#' 
#' @return A function producing a colour gradient ramp.
#' @name ramps
#' @param n (\code{numeric}) Number of different colors to generate from the palette
#' @param force (\code{logical}) Specify \code{pal} when multiple are available. More details to come. 
#' @param pal (\code{character}) A palette name from the lists below
#' 
NULL


#' inverse heatmap
#' @rdname ramps
#' 
#' @export 
gradinv <- grDevices::colorRampPalette(c("#33358a", "#76acce", "#fff99a",  "#e22c28", "#690720"))

#' colour ramp: ocean
#' @rdname ramps
#' 
#' @export 
ocean <- colorRampPalette(c("#131328FF","#060717FF","#232243FF","#2F2D5DFF",
                            "#383771FF","#3B4284FF","#40549EFF","#416EB0FF",
                            "#4782B9FF","#5899C0FF","#6FB2CAFF","#8AC8D4FF",
                            "#A4D6DCFF","#BCE4E6FF","#DDF5F6FF"))

#' colour ramp: terra
#' @rdname ramps
#' 
#' @export 
terra <- colorRampPalette(c("#336600","#F3CA89","#D9A627","#A49019",
                            "#9F7B0D","#996600","#B27676","#C2B0B0",
                            "#E5E5E5","#FFFFFF"))

#' colour ramp: ipccTemp
#' @rdname ramps
#' 
#' @export 
ipccTemp <- function(n, force=11){
  mincol <- 5
  maxcol <- 11
  
  if (force > maxcol) {
    warning(paste("n too large, allowed maximum for palette ipccLine is", maxcol, 
                  "\nReturning the palette you asked for with ", maxcol, " colours \n"))
    force <- maxcol
  }
  
  if (force < mincol) {
    warning(paste("minimal values for palette ipccLine is", mincol, 
                  "\nReturning the palette you asked for with ", mincol, " colours \n"))
    
    force <- mincol
  }
  
  force <- switch(force-4, 
                # 5 colours
                rgb(c(202, 244, 247, 146, 5), 
                    c(0, 165, 247, 197, 113), 
                    c(32, 130, 247, 222, 176), maxColorValue = 255),
                # 6 colours
                rgb(c(178, 239, 253, 209, 103, 33), 
                    c(24, 138, 219, 229, 169, 102), 
                    c(43, 98, 199, 240, 207, 172), maxColorValue = 255),
                # 7 colours
                rgb(c(178, 239, 253, 247, 209, 103, 33), 
                    c(24, 138, 219, 247, 229, 169, 102), 
                    c(43, 98, 199, 247, 240, 207, 172), maxColorValue = 255),
                # 8 colours
                rgb(c(178, 214, 244, 253, 209, 146, 67, 33), 
                    c(24, 96, 165, 219, 229, 197, 147, 102), 
                    c(43, 77, 130, 199, 240, 222, 195, 172), maxColorValue = 255),
                # 9 colours
                rgb(c(178, 214, 244, 253, 247, 209, 146, 67, 33), 
                    c(24, 96, 165, 219, 247, 229, 197, 147, 102), 
                    c(43, 77, 130, 199, 247, 240, 222, 195, 172), maxColorValue = 255),
                # 10 colours
                rgb(c(103, 178, 214, 244, 253, 209, 146, 67, 33, 5), 
                    c(0, 24, 96, 165, 219, 229, 197, 147, 102, 48), 
                    c(31, 43, 77, 130, 199, 240, 222, 195, 172, 97), maxColorValue = 255),
                # 11 colours
                rgb(c(103, 178, 214, 244, 253, 247, 209, 146, 67, 33, 5), 
                    c(0, 24, 96, 165, 219, 247, 229, 197, 147, 102, 48), 
                    c(31, 43, 77, 130, 199, 247, 240, 222, 195, 172, 97), maxColorValue = 255)
  )
  
  colorRampPalette(force)(n)
}

#' colour ramp: ipccPrec
#' @rdname ramps
#' 
#' @export 
ipccPrec <- function(n, force=11){
  mincol <- 5
  maxcol <- 11
  
  if (force > maxcol) {
    warning(paste("n too large, allowed maximum for palette ipccLine is", maxcol, 
                  "\nReturning the palette you asked for with ", maxcol, " colours \n"))
    force <- maxcol
  }
  
  if (force < mincol) {
    warning(paste("minimal values for palette ipccLine is", mincol, 
                  "\nReturning the palette you asked for with ", mincol, " colours \n"))
    
    force <- mincol
  }
  
  force <- switch(force-4, 
                # 5 colours
                rgb(c(166, 223, 245, 128, 1), 
                    c(67, 194, 245, 205, 133), 
                    c(26, 125, 245, 193, 113), maxColorValue = 255),
                # 6 colours
                rgb(c(140, 216, 246, 199, 90, 1), 
                    c(81, 179, 232, 234, 180, 102), 
                    c(10, 101, 195, 229, 172, 94), maxColorValue = 255),
                # 7 colours
                rgb(c(140, 216, 246, 245, 199, 90, 1), 
                    c(81, 179, 232, 245, 234, 180, 102), 
                    c(10, 101, 195, 245, 229, 172, 94), maxColorValue = 255),
                # 8 colours
                rgb(c(140, 191, 223, 246, 199, 128, 53, 1), 
                    c(81, 129, 194, 232, 234, 205, 151, 102), 
                    c(10, 45, 125, 195, 229, 193, 143, 94), maxColorValue = 255),
                # 9 colours
                rgb(c(150, 191, 223, 246, 245, 199, 128, 53, 1), 
                    c(81, 129, 194, 232, 245, 23, 205, 151, 102), 
                    c(10, 45, 125, 195, 245, 229, 193, 143, 94), maxColorValue = 255),
                # 10 colours
                rgb(c(84, 140, 191, 223, 246, 199, 128, 53, 1, 0), 
                    c(48, 81, 129, 194, 232, 234, 205, 151, 102, 60), 
                    c(5, 10, 45, 125, 195, 229, 193, 143, 94, 48), maxColorValue = 255),
                # 11 colours
                rgb(c(84, 140, 191, 223, 246, 245, 199, 128, 53, 1, 0), 
                    c(48, 81, 129, 194, 232, 245, 234, 205, 151, 102, 60), 
                    c(5, 10, 45, 125, 195, 245, 229, 193, 143, 94, 48), maxColorValue = 255)
  )
  
  colorRampPalette(force)(n)
}

#' colour ramp: wet
#' @rdname ramps
#' 
#' @export 
wet <-  colorRampPalette(c("#FBFEF9FF","#D8F0D3FF","#97D5BCFF","#568975FF","#53B2D3FF","#308ABDFF","#1767A9FF","#0E4281FF","#0B3568FF","#061F3BFF"))

#' colour ramp: ipccLine
#' @rdname ramps
#' 
#' @export 
ipccLine <- function(n=6){
  mincol <- 1
  maxcol <- 6
  
  if (n > maxcol) {
    warning(paste("n too large, allowed maximum for palette ipccLine is", maxcol, 
                  "\nReturning the palette you asked for with ", maxcol, " colours \n"))
    n <- maxcol
  }
  
  if (n < mincol) {
    warning(paste("minimal values for palette ipccLine is", mincol, 
                  "\nReturning the palette you asked for with ", mincol, " colours \n"))
    
    n <- mincol
  }
  
  return(switch(n, 
                #1 colour
                rgb(0,0,0, maxColorValue = 255), 
                # 2 colours
                rgb(c(0, 84), 
                    c(0, 146), 
                    c(0, 205), maxColorValue = 255),
                # 3 colours
                rgb(c(0, 84, 196), 
                    c(0, 146, 121), 
                    c(0, 205, 0), maxColorValue = 255),
                # 4 colours
                rgb(c(0, 84, 196, 128), 
                    c(0, 146, 121, 128), 
                    c(0, 205, 0, 128), maxColorValue = 255),
                # 5 colours
                rgb(c(0, 84, 196, 128, 0), 
                    c(0, 146, 121, 128, 52), 
                    c(0, 205, 0, 128, 102), maxColorValue = 255),
                # 6 colours
                rgb(c(0, 84, 196, 128, 0, 0), 
                    c(0, 146, 121, 128, 52, 79), 
                    c(0, 205, 0, 128, 102, 0), maxColorValue = 255))
  )
}

#' colour ramp: ipccRCP
#' @rdname ramps
#' 
#' @export 
ipccRCP <- function(n=4){
  mincol <- 1
  maxcol <- 4
  
  if (n > maxcol) {
    warning(paste("n too large, allowed maximum for palette ipccLine is", maxcol, 
                  "\nReturning the palette you asked for with ", maxcol, " colours \n"))
    n <- maxcol
  }
  
  if (n < mincol) {
    warning(paste("minimal values for palette ipccLine is", mincol, 
                  "\nReturning the palette you asked for with ", mincol, " colours \n"))
    
    n <- mincol
  }
  
  pal <- rgb(c(153, 196, 84, 0), 
             c(0, 121, 146, 52), 
             c(2,0,205, 102), maxColorValue = 255)
  names(pal) <- c(8.5, 6.0, 4.5, 2.6)
  
  return(pal[1:n])
}

#'display palettes
#'@rdname ramps
#'
#'@export
showPal <- function(pal="all"){
  pals <- c("ocean", "gradinv", "terra", "ipccTemp", "ipccPrec", "wet", "ipccLine", "ipccRCP")
  n <- c(rep(11,6), 6, 4)
  
  if(pal %in% pals){
    suppressWarnings(image(1:n[which(pals == pal)],1,as.matrix(1:n[which(pals == pal)]),col=eval(parse(text = pal))(n[which(pals == pal)]),xlab=pal, 
          ylab="",xaxt="n",yaxt="n",bty="n"))
  } else {
    if (pal=="all" | pal==""){
       old.par <- graphics::par(no.readonly = TRUE)
       on.exit(graphics::par(old.par), add = TRUE)
          
      par(mfrow=c(2,4))
      for (i in 1:length(pals)){
        suppressWarnings(image(1:n[i],1,as.matrix(1:n[i]),col=eval(parse(text = pals[i]))(n[i]),main=pals[i], 
              xlab="", ylab="",xaxt="n",yaxt="n",bty="n"))
      }
    } else{
      message("Oops! This colour palette doesn't seem to exist yet :-) ") 
    }
  }
}
