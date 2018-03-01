#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): March 2018
# Date (last update):  March 2018
# Version 0.1
# Licence GPL v3
#------------------------


if (!isGeneric("combine")) {
  setGeneric("combine", function(map1,map2)
    standardGeneric("combine"))
}



setMethod('combine', signature(map1='RasterLayer',map2='RasterLayer'), 
          function(map1,map2) {
            if (!compareRaster(map1,map2)) stop('map1 and map2 are not comparable!')
            out <- raster(map1)
            x <- as.matrix(map1)
            y <- as.matrix(map2)
            z <- x
            z[] <- NA
            
            comb <- matrix(nc=4,nr=0)
            colnames(comb) <- c("map1","map2","id","count")
            id <- 1
            for (i in 1:nrow(x)){
              for (j in 1:ncol(x)){
                if (!is.na(x[i,j]) & !is.na(y[i,j])){
                  m1 <- x[i,j]; m2 <- y[i,j]
                  w1 <- which(comb[,1] == m1)
                  w2 <- which(comb[w1,2] == m2)
                  w <- w1[w2]
                  if (length(w) > 0) {
                    z[i,j] <- comb[w,3]
                    comb[w,4] <- comb[w,4] + 1
                  }
                  else {
                    comb <- rbind(comb,c(m1,m2,id,1))
                    z[i,j] <- id
                    id <- id + 1
                  }
                }
              }
            }
            out[] <- z[]
            list(out,comb)
          }
)

