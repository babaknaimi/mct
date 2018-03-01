#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): March 2018
# Date (last update):  March 2018
# Version 0.1
# Licence GPL v3
#------------------------


.crossG <- function(map1,map2,g2,comb){
  c1 <- crosstab(map1,comb)
  c1 <- data.frame(c1)
  w <- which(c1[,3] == 0)
  if (length(w) > 0) c1 <- c1[-w,]
  c1[,1] <- as.numeric(as.character(c1[,1]))
  c1[,2] <- as.numeric(as.character(c1[,2]))
  c1 <- na.omit(c1)
  
  c2 <- crosstab(map2,comb)
  c2 <- data.frame(c2)
  w <- which(c2[,3] == 0)
  if (length(w) > 0) c2 <- c2[-w,]
  c2[,1] <- as.numeric(as.character(c2[,1]))
  c2[,2] <- as.numeric(as.character(c2[,2]))
  c2 <- na.omit(c2)
  
  c2g <- crosstab(g2,comb)
  c2g <- data.frame(c2g)
  w <- which(c2g[,3] == 0)
  if (length(w) > 0) c2g <- c2g[-w,]
  c2g[,1] <- as.numeric(as.character(c2g[,1]))
  c2g[,2] <- as.numeric(as.character(c2g[,2]))
  c2g <- na.omit(c2g)
  
  ct <- data.frame(uniqueID=c1[,2],map1=c1[,1],map2=c2[,1],count=c1[,3],group2=c2g[,1],Ag=0,Disag=0)
  
  g2codes <- unique(c2g[,1])
  ct.e <- matrix(nr=length(g2codes),nc=4)
  
  for (i in 1:length(g2codes)) {
    ct.t <- ct[which(ct[,5] == g2codes[i]),]
    area <- sum(ct.t[,4])
    for (j in 1:nrow(ct.t)) {
      if (ct.t[j,2] == ct.t[j,3]) ct.t[j,6] <- ct.t[j,4] / area
      else ct.t[j,7] <- ct.t[j,4] / area
    }
    ct.e[i,1] <- g2codes[i]
    ct.e[i,2] <- sum(ct.t[,6])
    ct.e[i,3] <- sum(ct.t[,7])
    ct.e[i,4] <- area
  }
  ct.e
}
#---------

if (!isGeneric("fuzzyMc")) {
  setGeneric("fuzzyMc", function(map1,map2)
    standardGeneric("fuzzyMc"))
}



setMethod('fuzzyMc', signature(map1='RasterLayer',map2='RasterLayer'), 
          function(map1,map2) {
            if (!compareRaster(map1,map2)) stop('map1 and map2 are not comparable!')
            g1 <- g2 <- raster(map1)
            g1[] <- group(map1[],nrow(map1),ncol(map1))
            g2[] <- group(map2[],nrow(map2),ncol(map2))
            co <- combine(g1,g2)
            cg <- .crossG(map1,map2,g2,co[[1]])
            fuz.int <- matrix(nr=nrow(cg),nc=5)
            fuz.comp <- matrix(nr=nrow(cg),nc=5)
            fuz.grp <- matrix(nr=nrow(cg),nc=2)
            fuz.loc <- matrix(nr=nrow(cg),nc=5)
            
            fuz.int[,1] <- fuzzy(cg[,2],params=c(-0.02,0.15),type='dec')
            fuz.int[,2] <- fuzzy(cg[,2],params=c(0,0.2,0.3,0.65),type='bel')
            fuz.int[,3] <- fuzzy(cg[,2],params=c(0.15,0.45,0.55,0.85),type='bel')
            fuz.int[,4] <- fuzzy(cg[,2],params=c(0.38,0.75,0.85,1.05),type='bel')
            fuz.int[,5] <- fuzzy(cg[,2],params=c(0.87,0.97),type='inc') 
            
            fuz.comp[,1] <- fuzzy(cg[,3],params=c(0,0.15),type='dec')
            fuz.comp[,2] <- fuzzy(cg[,3],params=c(0,0.2,0.3,0.65),type='bel')
            fuz.comp[,3] <- fuzzy(cg[,3],params=c(0.15,0.45,0.55,0.85),type='bel')
            fuz.comp[,4] <- fuzzy(cg[,3],params=c(0.38,0.75,0.85,1.05),type='bel')
            fuz.comp[,5] <- fuzzy(cg[,3],params=c(0.87,0.97),type='inc')
            
            fuz.grp[,1] <- fuzzy(cg[,4],params=c(1.5,2.5),type='dec')
            fuz.grp[,2] <- fuzzy(cg[,4],params=c(1.9,2.25),type='inc')
            
            rules <- matrix(nr=10,nc=4)
            colnames(rules) <- c("int","comp","grp","loc")
            # for int and comp: 1=very low; 2=low; 3=Medium; 4=High; 5=Very high
            # for group: 1=Small; 2=Large
            # for loc (Local matching): 1=Very poor; 2=Poor; 3=Good; 4=Very good; 5=Perfect
            rules[1,] <- c(1,5,1,2)
            rules[2,] <- c(1,5,2,1)
            rules[3,] <- c(2,4,1,3)
            rules[4,] <- c(2,4,2,2)
            rules[5,] <- c(3,3,1,3)
            rules[6,] <- c(3,3,2,3)
            rules[7,] <- c(4,2,1,3)
            rules[8,] <- c(4,2,2,4)
            rules[9,] <- c(5,1,1,5)
            rules[10,] <- c(5,1,2,5)
            
            for (j in 1:nrow(cg)){
              fuz.loc.temp <- matrix(nr=10,nc=5)
              for (i in 1:10) {
                fuz.loc.temp[i,rules[i,4]] <- min(fuz.int[j,rules[i,1]],fuz.comp[j,rules[i,2]],fuz.grp[j,rules[i,3]])
              }
              fuz.loc[j,] <- apply(fuz.loc.temp,2,function(x){max(x,na.rm=TRUE)})
            }
            #--- Fuzzy local matching output
            fuz.loc.mtx <- matrix(nr=101,nc=6)
            v <- seq(0,1,0.01)
            fuz.loc.mtx[,1] <- v
            fuz.loc.mtx[,2] <- fuzzy(v,params=c(0,0.07),type="gau")
            fuz.loc.mtx[,3] <- fuzzy(v,params=c(0.15,0.12),type="gau")
            fuz.loc.mtx[,4] <- fuzzy(v,params=c(0.5,0.15),type="gau")
            fuz.loc.mtx[,5] <- fuzzy(v,params=c(0.78,0.12),type="gau")
            fuz.loc.mtx[,6] <- fuzzy(v,params=c(1.02,0.05),type="gau")
            #--------------
            LM <- matrix(nr=101,nc=nrow(cg))
            for(r in 1:nrow(cg)){
              v.out <- matrix(0,nr=101,nc=5)
              for (f in 1:5) {
                for (i in 1:101) {
                  m <- fuz.loc.mtx[i,(f+1)]
                  if (fuz.loc[r,f] > 0) {
                    if (fuz.loc[r,f] > m) v.out[i,f] <- m
                    else v.out[i,f] <- fuz.loc[r,f]
                  }
                }
              }
              LM[,r] <- apply(v.out,1,max)
            }
            
            crisp <- rep(0,nrow(cg))
            for(r in 1:nrow(cg)){ 
              crisp [r] <- sum(v*LM[,r])/sum(LM[,r])
            }
            area <- cg[,4]*res(co[[1]])[1]*res(co[[1]])[2]
            while (sum(area) <= 1) area <- area * 100
            outTable <- data.frame(cg,Area=area,local_match=crisp)
            rcmtx <- matrix(nr=nrow(cg),nc=2)
            rcmtx[,1] <- cg[,1]
            rcmtx[,2] <- crisp
            
            
            global.Match <- sum(area * crisp) / sum(area)
            new('fuzzyMC', fuzzyMap= reclassify(g2,rcmtx),globalMatch=global.Match,statistics=outTable)
          }
)