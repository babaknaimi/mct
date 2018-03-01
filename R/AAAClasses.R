#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): March 2018
# Date (last update):  March 2018
# Version 0.1
# Licence GPL v3
#------------------------



#setClassUnion('data.frameORmatrix',c("data.frame","matrix"))
#setClassUnion('matrixORnull',c("NULL","matrix"))

setClass("fuzzyMC",
         representation(
           fuzzyMap="RasterLayer",
           globalMatch="numeric",
           statistics="data.frame"
         )
)
