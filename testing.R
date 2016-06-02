library(tcltk)
library(raster)
library(imager)
dir <- tk_choose.dir()
setwd(dir)

image <- "./004_1_Oct3_2015_GOPRO002.JPG"

im <- load.image(image)
# make image smaller
ims <- resize(im, round(width(im)/10), round(height(im)/10))
rm(im)
# make a df
df <- as.data.frame(ims)

# working with circles
# Blank plot

#par(las=1)
#plot(0, 0, type="n", xlab="", ylab="", xlim=c(0,4000), ylim=c(0,3000), asp=1, bty="n")
# One circle
plot(im)
symbols(2000, 1500, inches=FALSE, circles = 1000, add=TRUE)

library(LAI)
# the above library doesn't work because it does the thing on the whole image.
# I need to crop the image circularly before feeding it to LAI
# actually, you need to research and understand everything about how this program works.
# that is your workwork.
# make it draw the graph and use ?symbols to draw exactly where it is taking the LAI from.



rst <- raster::raster(x = system.file("extdata", "IMG_7595.JPG", package = "LAI"), band = 3)  # only blue band is necessary for some reason
# crop_around_angle function

crop_around_angle_t <- function(
  img, # raster object
  camera_horiz_FOV, # degrees - 73.7
  focal_angle, # degrees angle at which the camera was pointing (degrees) - 45
  
  # crop box
  crop_top_angle = (90 - 57.5) + 5, # our angles are calculated from the ground
  crop_bottom_angle = (90 - 57.5) - 5 # ten degree range
) {
  
  # pixel/degree ratio
  pixel_degree_ratio <- img@nrows / camera_horiz_FOV
  
  # at what angle is the picture bottom pointing to
  bottom_angle <- focal_angle - camera_horiz_FOV / 2
  
  crop_top_pixel <- (crop_top_angle - bottom_angle) * pixel_degree_ratio
  crop_bottom_pixel <- (crop_bottom_angle - bottom_angle) * pixel_degree_ratio
  if ((crop_top_pixel < 0) | (crop_bottom_pixel < 0)) {
    stop("Image does not include the 57 degrees band")
  }
  e <- raster::extent(c(0,img@ncols,crop_bottom_pixel,crop_top_pixel) )
  
  return(raster::crop(img,e))
  
}

# left off on researching how to clip a raster with round circle
# could use the erase function in raster package