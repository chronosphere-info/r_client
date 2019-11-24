#' Colour gradient ramps for default plotting
#'
#' The object contains functions produced by the \code{colorRampPalette} function.
#' 
# \cr
#' \code{show.pal} is used to display the available palettes. You can use \code{pal = "all"} or \code{pal=""} if you want to look at all the available palettes. 
#' You can also view single palettes individually. The following colour palettes are implemented:
#' \itemize{
#' \item \code{gradinv()}: inverse heatmap.
#' \item \code{ocean()}: ocean bathymetrical colours.
#' \item \code{terra()}: terrestrial topographical colours.
#' \item \code{coldhot()}: gradient from blue to red.
#' \item \code{drywet()}: gradient from brown to green.
#' \item \code{wet()}: gradient from white to green to blue. 
#' }
#' 
#' @name ramps
#' @param n Number of different colors to generate from the palette
#' @param pal A palette name from the lists below
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

#' colour ramp: coldhot
#' @rdname ramps
#' 
#' @export 
coldhot <- colorRampPalette(c("#0B2E60FF","#2364A6FF","#4691C0FF","#92C5DEFF","#D1E5EE","#F5F5F5","#FDDDC8","#F2A686FF","#D76152FF","#B12131FF","#640D21FF"))

#' colour ramp: drywet
#' @rdname ramps
#' 
#' @export 
drywet <- colorRampPalette(c("#533110FF","#8A531AFF","#BC8238FF","#E0C481FF","#F5E8C6FF","#F5F5F5","#DCF0ECFF","#83CCC0FF","#3A958DFF","#10665FFF","#093C31FF"))

#' colour ramp: wet
#' @rdname ramps
#' 
#' @export 
wet <-  colorRampPalette(c("#FBFEF9FF","#D8F0D3FF","#97D5BCFF","#568975FF","#53B2D3FF","#308ABDFF","#1767A9FF","#0E4281FF","#0B3568FF","#061F3BFF"))

#'display palettes
#'@rdname ramps
#'
#'@export
show.pal <- function(pal="all", n=11){
  pals <- c("ocean", "gradinv", "terra", "coldhot", "drywet", "wet")
  
  if(pal %in% pals){
    image(1:n,1,as.matrix(1:n),col=eval(parse(text = pal))(n),xlab=pal, 
          ylab="",xaxt="n",yaxt="n",bty="n")
  } else {
    if (pal=="all" | pal==""){
      par(mfrow=c(2,3))
      for (i in 1:length(pals)){
        image(1:n,1,as.matrix(1:n),col=eval(parse(text = pals[i]))(n),main=pals[i], 
              xlab="", ylab="",xaxt="n",yaxt="n",bty="n")
      }
    } else{
      message("Oops! This colour palette doesn't seem to exist yet :-) ") 
    }
  }
}