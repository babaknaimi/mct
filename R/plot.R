#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): March 2018
# Date (last update):  March 2018
# Version 0.1
# Licence GPL v3
#------------------------

if (!isGeneric("plot")) {
  setGeneric("plot", function(x,y,...)
    standardGeneric("plot"))
}	

setMethod("plot", signature(x='fuzzyMC'),
          function(x,xlab,ylab,col,main,...) {
            if (missing(xlab)) xlab <- "X Coordinates"
            if (missing(ylab)) ylab <- "Y Coordinates"
            if (missing(col)) col=colorRampPalette(c("red","yellow","green","darkgreen"))(100)
            
            if (missing(main)) main=paste("Global.Match = ",round(x@globalMatch,3),sep="")
            plot(x@fuzzyMap,xlab=xlab,ylab=ylab,main=main,col=col,...)
          }
)
