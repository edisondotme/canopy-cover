library(tcltk)
library(raster)
library(imager)
setwd(tk_choose.dir())

image <- "./004_1_Oct3_2015_GOPRO002.JPG"

im <- load.image(image)

# working with circles
# Blank plot


#par(las=1)
#plot(0, 0, type="n", xlab="", ylab="", xlim=c(0,4000), ylim=c(0,3000), asp=1, bty="n")
# One circle
plot(im)
symbols(2000, 1500, inches=FALSE, circles = 1000, add=TRUE)

