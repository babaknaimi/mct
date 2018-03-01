#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): March 2018
# Date (last update):  March 2018
# Version 0.1
# Licence GPL v3
#------------------------




if (!isGeneric("fuzzy")) {
  setGeneric("fuzzy", function(x,type,params)
    standardGeneric("fuzzy"))
}




setMethod('fuzzy', signature(x='numeric'), 
          function(x,type,params) {
            f <- rep(NA,length(x))
            
            if (type == "decreasing" | type == "dec"){
              c <- params[1]
              d <- params[2]
              f <- ifelse(x < c,1,f)
              f <- ifelse(x >= c & x <= d,cos(((x-c)/(d-c))*pi/2)^2,f)
              f <- ifelse(x > d,0,f)
            } else if (type == "increasing" | type == "inc"){
              a <- params[1]
              b <- params[2]
              f <- ifelse(x < a,0,f)
              f <- ifelse(x >= a & x <= b,cos((1-(x-a)/(b-a))*pi/2)^2,f)
              f <- ifelse(x > b,1,f)
            } else if (type == "bell" | type == "bel"){
              a <- params[1]
              b <- params[2]
              c <- params[3]
              d <- params[4]
              f <- ifelse(x < a,0,f)
              f <- ifelse(x >= a & x <= b,cos((1-(x-a)/(b-a))*pi/2)^2,f)
              f <- ifelse(x > b & x < c,1,f)
              f <- ifelse(x >= c & x <= d,cos(((x-c)/(d-c))*pi/2)^2,f)
              f <- ifelse(x > d,0,f)
            } else if (type == "gaussian" | type == "gau") {
              opt <- params[1]
              tol <- params[2]
              f <- exp(-0.5*((x-opt)/tol)^2)
            }
            
            round(f,3)
            
          }
)
